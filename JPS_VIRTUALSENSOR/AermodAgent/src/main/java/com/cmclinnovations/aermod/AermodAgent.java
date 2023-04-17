package com.cmclinnovations.aermod;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.locationtech.jts.geom.Polygon;

import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.WeatherData;
import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;

import it.geosolutions.geoserver.rest.GeoServerRESTManager;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/"})
public class AermodAgent extends DerivationAgent {
    private static final Logger LOGGER = LogManager.getLogger(AermodAgent.class);
    private QueryClient queryClient;

    @Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {   
        String weatherStationIri = derivationInputs.getIris(QueryClient.REPORTING_STATION).get(0);
        String nxIri = derivationInputs.getIris(QueryClient.NX).get(0);
        String nyIri = derivationInputs.getIris(QueryClient.NY).get(0);
        String scopeIri = derivationInputs.getIris(QueryClient.SCOPE).get(0);
        String simulationTimeIri = derivationInputs.getIris(QueryClient.SIMULATION_TIME).get(0);
        
        long simulationTime = queryClient.getMeasureValueAsLong(simulationTimeIri);

        if (simulationTime == 0) {
            LOGGER.info("Simulation time = 0, this is from calling createSyncDerivationForNewInfo the first time");
            return;
        }

        int nx = queryClient.getMeasureValueAsInt(nxIri);
        int ny = queryClient.getMeasureValueAsInt(nyIri);

        // get ships within a scope and time
        Polygon scope = queryClient.getScopeFromOntop(scopeIri);
        List<Ship> ships = queryClient.getShipsWithinTimeAndScopeViaTsClient(simulationTime, scope);

        // update derivation of ships (on demand)
        List<String> derivationsToUpdate = queryClient.getDerivationsOfShips(ships);
        updateDerivations(derivationsToUpdate);

        // get emissions and set the values in the ships
        LOGGER.info("Querying emission values");
        queryClient.setEmissions(ships);

        
        /*The next two lines of code are for use with the Weather Agent. 
        // update weather station with simulation time
        updateWeatherStation(weatherStationIri, simulationTime);

        // get weather data from station
        WeatherData weatherData = queryClient.getWeatherData(weatherStationIri, simulationTime);
        */

        
        // making directory for simulation
        LOGGER.info("Creating directory for simulation files");
        Path simulationDirectory = Paths.get(EnvConfig.SIMULATION_DIR, UUID.randomUUID().toString());

        // create input files
        Aermod aermod = new Aermod(simulationDirectory);

        //compute srid
        int centreZoneNumber = (int) Math.ceil((scope.getCentroid().getCoordinate().getX() + 180)/6);
        int srid;
        if (scope.getCentroid().getCoordinate().getY() < 0) {
        	srid = Integer.valueOf("327" + centreZoneNumber);
            
        } else {
        	srid = Integer.valueOf("326" + centreZoneNumber);
        }

        // Query buildings and plant items.
        //Run BPIPPRM
 
        Buildings bpi;
        try {
            bpi = new Buildings();
            bpi.init(simulationDirectory,scope, srid, nx, ny);
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }

        if (bpi.locindex >= 0) {
            int buildRes = bpi.run();
            if (buildRes != 0) {
                LOGGER.error("Failed to run BPIPPRM, terminating");
                throw new RuntimeException();
            }
        } else {
            bpi.createAERMODBuildingsInput();
            bpi.createAERMODSourceInput();
            bpi.createAERMODReceptorInput(nx, ny);
        }

        // Send POST request to OpenMeteo Agent to update OpenMeteo Station

        double lat = scope.getCentroid().getCoordinate().getY();
        double lon = scope.getCentroid().getCoordinate().getX();
        String StartTS = bpi.timeStamps.get(0);
        String EndTS = bpi.timeStamps.get(bpi.timeStamps.size()-1);
        LocalDateTime ldt = LocalDateTime.parse(StartTS);
        String StartDate = ldt.getYear() + "-" + ldt.getMonthValue() + "-" + ldt.getDayOfMonth();
        ldt = LocalDateTime.parse(EndTS);
        String EndDate = ldt.getYear() + "-" + ldt.getMonthValue() + "-" + ldt.getDayOfMonth();
        updateOpenMeteoStation(lat, lon, StartDate, EndDate);
        WeatherData weatherData = queryClient.getOpenMeteoData(lat, lon);
        deleteOpenMeteoStation(scope);

        weatherData.timeStamps = bpi.timeStamps;
        aermod.create144File(weatherData);


        // run aermet (weather preprocessor)
        if (aermod.runAermet(scope) != 0) {
            LOGGER.error("Stopping agent execution");
            return;
        }

        // create emissions input
        if (aermod.createPointsFile(ships, srid) != 0) {
            LOGGER.error("Failed to create points emissions file, terminating");
            return;
        }

        if (!bpi.aermodInputCreated) aermod.createAermodInputFile(scope, nx, ny, srid);
        aermod.runAermod("aermod.inp");
        aermod.runAermod("aermodVirtualSensor.inp");
        // Call Python Service to draw contour plot for pollution concentrations
        String outputFileURL = aermod.uploadToFileServer("averageConcentration.dat");
        JSONObject geoJSON = aermod.getGeoJSON(EnvConfig.PYTHON_SERVICE_URL, outputFileURL, srid);

        String outFileURL = aermod.uploadToFileServer("receptor.dat");
        JSONObject geoJSON2 = aermod.getGeoJSON(EnvConfig.PYTHON_SERVICE_ELEVATION_URL, outFileURL, srid);


        // layer name
        String dispLayerName = "disp_" + simulationDirectory.getFileName();
        String shipLayerName = "ships_" + simulationTime; // hardcoded in ShipInputAgent
        String plantsLayerName = "source_layer" ;
        String elevationLayerName = "elevation_layer";

        // upload to PostGIS using GDAL
        GDALClient gdalClient = new GDALClient();
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, dispLayerName, geoJSON.toString(), new Ogr2OgrOptions(), true);
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, elevationLayerName, geoJSON2.toString(), new Ogr2OgrOptions(), true);

        // create geoserver layer based for that
        GeoServerClient geoServerClient = new GeoServerClient();

        // make sure style is uploaded first
        uploadStyle(geoServerClient);

        // then create a layer with that style as default
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        geoServerVectorSettings.setDefaultStyle("dispersion_style");
        geoServerClient.createPostGISLayer(null, EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, dispLayerName, geoServerVectorSettings);
        geoServerClient.createPostGISLayer(null, EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, elevationLayerName, geoServerVectorSettings);

        // ships_ is hardcoded here and in ShipInputAgent
        queryClient.updateOutputs(derivationInputs.getDerivationIRI(), outputFileURL, dispLayerName, shipLayerName, simulationTime);
        if (aermod.createDataJson(shipLayerName, dispLayerName, plantsLayerName,elevationLayerName) != 0) {
            LOGGER.error("Failed to create data.json file for visualisation, terminating");
            return;
        }

        if (aermod.createVisSettingsFile(scope.getCentroid()) != 0) {
            LOGGER.error("Failed to create settings file, terminating agent request");
        }

        if (aermod.modifyFilePermissions("data.json") != 0) {
            LOGGER.error("Failed to modify permissions for data.json, terminating");
            return;
        }

        if (aermod.modifyFilePermissions("settings.json") != 0) {
            LOGGER.error("Failed to modify permissions for settings.json, terminating");
            return;
        }

    }
    
    void updateDerivations(List<String> derivationsToUpdate) {
        CompletableFuture<String> getAsync = null;
        for (String derivation : derivationsToUpdate) {
            getAsync = CompletableFuture.supplyAsync(() -> {
                try {
                    devClient.updatePureSyncDerivation(derivation);
                    return null; // need to return something, could not get it to work with CompletableFuture<Void>
                } catch (Exception e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Error occured while updating ship derivations");
                    return null;
                }
            });   
        }
        if (getAsync != null) {
            getAsync.join();
        }
    }

    /**
     * sends a PUT request to the weather agent
     * @param weatherStationIri
     */
    void updateWeatherStation(String weatherStationIri, long simulationTime) {
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            URIBuilder builder = new URIBuilder(EnvConfig.WEATHER_AGENT);
            builder.setParameter("iri", weatherStationIri);
            builder.setParameter("timestamp", String.valueOf(simulationTime));

            HttpPut httpPut = new HttpPut(builder.build());
            CloseableHttpResponse response = httpClient.execute(httpPut);
            if (response.getStatusLine().getStatusCode() != 200) {
                LOGGER.fatal("Status code = {}", response.getStatusLine().getStatusCode());
            }
        } catch (IOException e) {
            LOGGER.fatal(e.getMessage());
            throw new JPSRuntimeException("Failed at closing connection",e);
        } catch (URISyntaxException e) {
            LOGGER.fatal(e.getMessage());
            throw new JPSRuntimeException("Failed at building URL for update request",e);
        }
    }

    void uploadStyle(GeoServerClient geoServerClient) {
        RESTEndpointConfig geoserverEndpointConfig = geoServerClient.readEndpointConfig("geoserver", RESTEndpointConfig.class);
        GeoServerRESTManager manager = new GeoServerRESTManager(geoserverEndpointConfig.getUrl(), geoserverEndpointConfig.getUserName(), geoserverEndpointConfig.getPassword());

        if (!manager.getReader().existsStyle(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DISPERSION_STYLE_NAME)) {
            try {
                File sldFile = new File(getClass().getClassLoader().getResource("dispersion.sld").toURI());

                if (manager.getPublisher().publishStyleInWorkspace(EnvConfig.GEOSERVER_WORKSPACE, sldFile, EnvConfig.DISPERSION_STYLE_NAME)) {
                    LOGGER.info("GeoServer style created");
                } else {
                    throw new RuntimeException("GeoServer style cannot be created");
                }
            } catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig(); 

        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        RemoteStoreClient ontopStoreClient = new RemoteStoreClient(endpointConfig.getOntopurl());
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());

        queryClient = new QueryClient(storeClient, ontopStoreClient, rdbStoreClient);
        super.devClient = new DerivationClient(storeClient, QueryClient.PREFIX_DISP);
    }

     /**
     * Send PUT request to OpenMeteo Agent to create a weather station and instantiate the weather data at one hour intervals
     * between the start and end dates. 
     * @param polygon
     * @return
     */
    void updateOpenMeteoStation(double lat, double lon, String StartDate, String EndDate) {
        
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            URIBuilder builder = new URIBuilder(EnvConfig.OPENMETEO_AGENT_RUN);
            builder.addParameter("lon",String.valueOf(lon));
            builder.addParameter("lat",String.valueOf(lat));
            builder.addParameter("start_date", StartDate);
            builder.addParameter("end_date", EndDate);
            HttpPut httpPut = new HttpPut(builder.build());

            CloseableHttpResponse response = httpClient.execute(httpPut);
            if (response.getStatusLine().getStatusCode() != 200) {
                LOGGER.fatal("Status code = {}", response.getStatusLine().getStatusCode());
            }

        } catch (URISyntaxException e) {
            LOGGER.error("Failed to build URI for OpenMeteo agent post request");
            LOGGER.error(e.getMessage());
        } catch (IOException e) {
            LOGGER.error("Http PUT request failed for OpenMeteo agent");
            LOGGER.error(e.getMessage());
        }

    }

      /**
     * Send PUT request to OpenMeteo Agent to delete the weather station
     * @param polygon
     * @return
     */
    void deleteOpenMeteoStation(Polygon scope) {

        double lat = scope.getCentroid().getCoordinate().getY();
        double lon = scope.getCentroid().getCoordinate().getX();
        
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            URIBuilder builder = new URIBuilder(EnvConfig.OPENMETEO_AGENT_DELETE);
            builder.addParameter("lon",String.valueOf(lon));
            builder.addParameter("lat",String.valueOf(lat));
            HttpPut httpPut = new HttpPut(builder.build());

            CloseableHttpResponse response = httpClient.execute(httpPut);
            if (response.getStatusLine().getStatusCode() != 200) {
                LOGGER.fatal("Status code = {}", response.getStatusLine().getStatusCode());
            }
        } catch (URISyntaxException e) {
            LOGGER.error("Failed to build URI for OpenMeteo agent post request");
            LOGGER.error(e.getMessage());
        } catch (IOException e) {
            LOGGER.error("Http PUT request failed for OpenMeteo agent");
            LOGGER.error(e.getMessage());
        }

    }






}