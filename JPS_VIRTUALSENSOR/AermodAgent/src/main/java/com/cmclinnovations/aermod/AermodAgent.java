package com.cmclinnovations.aermod;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
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
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Polygon;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.DispersionOutput;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.Pollutant;
import com.cmclinnovations.aermod.objects.Ship;
import com.cmclinnovations.aermod.objects.StaticPointSource;
import com.cmclinnovations.aermod.objects.WeatherData;
import com.cmclinnovations.aermod.objects.Pollutant.PollutantType;
import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;

import it.geosolutions.geoserver.rest.GeoServerRESTManager;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = { "/" })
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
        // citiesNamespaceIri will be null if this parameter was excluded from the POST
        // request sent to DispersionInteractor
        String citiesNamespace = null;
        if (derivationInputs.getIris(QueryClient.CITIES_NAMESPACE) != null) {
            String citiesNamespaceIri = derivationInputs.getIris(QueryClient.CITIES_NAMESPACE).get(0);
            citiesNamespace = queryClient.getCitiesNamespace(citiesNamespaceIri);
        } else {
            LOGGER.info("No citieskg namespace was specified in the POST request to Dispersion Interactor." +
                    "Static point sources will not be included in this AERMOD run.");
        }

        long simulationTime = queryClient.getMeasureValueAsLong(simulationTimeIri);

        if (simulationTime == 0) {
            LOGGER.info("Simulation time = 0, this is from calling createSyncDerivationForNewInfo the first time");
            return;
        }

        int nx = queryClient.getMeasureValueAsInt(nxIri);
        int ny = queryClient.getMeasureValueAsInt(nyIri);

        // get ships within a scope and time
        Polygon scope = queryClient.getScopeFromOntop(scopeIri);

        List<StaticPointSource> staticPointSources = new ArrayList<>();
        BuildingsData bd = null;

        if (citiesNamespace != null) {
            String namespaceCRS = queryClient.getNamespaceCRS(citiesNamespace);
            queryClient.setcitiesNamespaceCRS(citiesNamespace, namespaceCRS);
            try {
                staticPointSources = queryClient.getStaticPointSourcesWithinScope(scope);
                bd = new BuildingsData(namespaceCRS, queryClient);
                bd.setStaticPointSourceProperties(staticPointSources);
            } catch (ParseException e) {
                e.printStackTrace();
                throw new JPSRuntimeException("Could not set static point source properties.");
            }

        }
        List<Ship> ships = queryClient.getShipsWithinTimeAndScopeViaTsClient(simulationTime, scope);

        List<PointSource> allSources = new ArrayList<>();
        allSources.addAll(staticPointSources);
        allSources.addAll(ships);

        List<Building> buildings = new ArrayList<>();
        if (bd != null) {
            try {
                buildings = bd.getBuildings(allSources);
            } catch (ParseException e) {
                e.printStackTrace();
                throw new JPSRuntimeException("Could not set building properties.");

            }
        }

        // update derivation of ships (on demand)
        List<String> derivationsToUpdate = queryClient.getDerivationsOfPointSources(allSources);
        updateDerivations(derivationsToUpdate);

        // get emissions and set the values in the ships
        LOGGER.info("Querying emission values");
        queryClient.setEmissions(allSources);

        // update weather station with simulation time
        updateWeatherStation(weatherStationIri, simulationTime);
        // get weather data from station
        WeatherData weatherData = queryClient.getWeatherData(weatherStationIri, simulationTime);

        // making directory for simulation
        LOGGER.info("Creating directory for simulation files");
        Path simulationDirectory = Paths.get(EnvConfig.SIMULATION_DIR, UUID.randomUUID().toString());

        // create input files
        Aermod aermod = new Aermod(simulationDirectory);

        // compute srid
        int centreZoneNumber = (int) Math.ceil((scope.getCentroid().getCoordinate().getX() + 180) / 6);
        int srid;
        if (scope.getCentroid().getCoordinate().getY() < 0) {
            srid = Integer.valueOf("327" + centreZoneNumber);

        } else {
            srid = Integer.valueOf("326" + centreZoneNumber);
        }

        if (citiesNamespace != null) {
            if (queryClient.tableExists(EnvConfig.ELEVATION_TABLE)) {
                queryClient.setElevation(staticPointSources, buildings, srid);
                List<byte[]> elevData = queryClient.getScopeElevation(scope);
                if (aermod.createAERMAPInput(elevData, centreZoneNumber) != 0) {
                    LOGGER.error("Could not create input data file for running AERMAP.");
                    throw new JPSRuntimeException("Error creating AERMAP elevation data input.");
                }
                aermod.createAERMAPReceptorInput(scope, nx, ny, srid);
                aermod.runAermap();
            } else {
                LOGGER.warn(
                        "The specified elevation data table {} does not exist. Base elevations of static point sources and buildings will be set to zero. ",
                        EnvConfig.ELEVATION_TABLE);
            }

            aermod.createBPIPPRMInput(staticPointSources, buildings, srid);
            aermod.runBPIPPRM();
        }

        // Moved this part outside the if statement to ensure that the buildings.dat
        // file is created in all cases.
        // It will be blank if citiesNamespace is null.
        aermod.createAERMODBuildingsInput(citiesNamespace);

        aermod.create144File(weatherData);

        // run aermet (weather preprocessor)
        if (aermod.runAermet(scope) != 0) {
            LOGGER.error("Stopping agent execution");
            return;
        }

        // Variable initialization
        // TODO: The height values will eventually be obtained from the derivation
        // inputs.
        List<Double> receptorHeights = new ArrayList<>();
        receptorHeights.add(0.0);

        // DispersionOutput object holds dispersion matrix (file), dispersion layer
        // names, and raster filenames
        DispersionOutput dispersionOutput = new DispersionOutput();

        String shipLayerName = "ships_" + simulationTime; // hardcoded in ShipInputAgent
        String sourceLayerName = "source_layer";
        String elevationLayerName = "elevation_layer";
        // create geoserver layer based for that
        GeoServerClient geoServerClient = GeoServerClient.getInstance();
        // make sure style is uploaded first
        uploadStyle(geoServerClient);
        // then create a layer with that style as default
        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();
        geoServerVectorSettings.setDefaultStyle("dispersion_style");

        // Upload point sources layer to POSTGIS and GeoServer
        JSONObject pointSourceFeatures = aermod.createStaticPointSourcesJSON(staticPointSources);
        GDALClient gdalClient = GDALClient.getInstance();
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.SOURCE_LAYER,
                pointSourceFeatures.toString(),
                new Ogr2OgrOptions(), true);
        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, EnvConfig.SOURCE_LAYER,
                new GeoServerVectorSettings());

        // Upload elevation contour plot to POSTGIS and GeoServer
        // The receptor.dat file may have been previously created by running AERMAP. If
        // so, it should not be overwritten.
        if (Files.notExists(simulationDirectory.resolve("aermod").resolve("receptor.dat")))
            aermod.createAERMODReceptorInput(scope, nx, ny, srid);
        String receptorFileURL = aermod.uploadToFileServer("receptor.dat");
        JSONObject geoJSON2 = aermod.getGeoJSON(EnvConfig.PYTHON_SERVICE_ELEVATION_URL, receptorFileURL, srid, 0.0,
                null);
        gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, elevationLayerName, geoJSON2.toString(),
                new Ogr2OgrOptions(), true);
        geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, elevationLayerName,
                geoServerVectorSettings);

        for (PollutantType pollutantType : Pollutant.getPollutantList()) {
            String pollutId = Pollutant.getPollutantLabel(pollutantType);
            // create emissions input
            if (aermod.createPointsFile(allSources, srid, pollutantType) != 0) {
                LOGGER.error("Failed to create points emissions file for pollutant {} at timestep {}", pollutId,
                        simulationTime);
                continue;
            }
            aermod.createAermodInputFile(scope, nx, ny, srid);
            aermod.runAermod("aermod.inp", pollutId);

            // Upload files used by scripts within Python Service to file server.
            String outputFileURL = aermod.uploadToFileServer("averageConcentration.dat");
            dispersionOutput.addDispMatrix(pollutantType, outputFileURL);

            String rasterFileName = UUID.randomUUID().toString();
            try {
                aermod.createFileForRaster(rasterFileName);
                dispersionOutput.addDispRaster(pollutantType, rasterFileName + ".tif");
            } catch (FileNotFoundException e) {
                LOGGER.error("Average concentration file not found, probably failed to run simulation");
            }

            // Get contour plots as geoJSON objects from PythonService and upload them to
            // PostGIS using GDAL
            for (int i = 0; i < receptorHeights.size(); i++) {
                double height = receptorHeights.get(i);
                JSONObject geoJSON = aermod.getGeoJSON(EnvConfig.PYTHON_SERVICE_URL, outputFileURL, srid, height,
                        pollutId);
                // The derivation IRI, pollutant ID, simulation time and height properties are
                // added here to
                // enable the
                // post-processing agent to retrieve the relevant contours for the
                // visualization.
                JSONArray features = geoJSON.getJSONArray("features");
                for (int j = 0; j < features.length(); j++) {
                    JSONObject feature = features.getJSONObject(j);
                    JSONObject featureProperties = feature.getJSONObject("properties");
                    featureProperties.put("derivationIRI", derivationInputs.getDerivationIRI());
                    featureProperties.put("pollutantID", pollutId);
                    featureProperties.put("simulation_time", simulationTime);
                    featureProperties.put("height", height);
                    feature.put("properties", featureProperties);
                    features.put(j, feature);
                }

                geoJSON.put("features", features);

                // The true option is used to append the contours obtained from the latest
                // simulation to previously obtained data.
                gdalClient.uploadVectorStringToPostGIS(EnvConfig.DATABASE, EnvConfig.DISPERSION_CONTOURS_TABLE,
                        geoJSON.toString(), new Ogr2OgrOptions(), true);

                String receptorHeightString = String.valueOf(height).replace(".", "_dp_");
                String derivationIRI = derivationInputs.getDerivationIRI();
                int derivationIdIndex = derivationIRI.indexOf("TimeSeries") + 11;
                String derivationId = derivationIRI.substring(derivationIdIndex);
                derivationId = derivationId.replace("-", "_");
                String layerName = "Pollutant_" + pollutId + "_height_" + receptorHeightString
                        + "_time_" + simulationTime + "_" + derivationId;
                // For the PM2.5 layer. Cannot have decimal points in a GeoServer layer name
                layerName = layerName.replace(".", "_dp_");

                String sqlQuery = String.format(
                        "SELECT ogc_fid, \"stroke-opacity\", \"stroke-width\", fill, title, \"fill-opacity\", stroke, wkb_geometry from %s"
                                + " WHERE \"derivationIRI\"='%s' and \"pollutantID\"='%s' and simulation_time = %d and height = %f ",
                        EnvConfig.DISPERSION_CONTOURS_TABLE, derivationInputs.getDerivationIRI(), pollutId,
                        simulationTime, height);

                GeoServerVectorSettings geoServerDispersionSettings = new GeoServerVectorSettings();

                UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
                virtualTable.setSql(sqlQuery);
                virtualTable.setEscapeSql(true);
                virtualTable.setName("dispersionVirtualTable");
                virtualTable.addVirtualTableGeometry("wkb_geometry", "MultiPolygon", "4326");
                LOGGER.info(virtualTable.getName());
                geoServerDispersionSettings.setVirtualTable(virtualTable);

                geoServerClient.createPostGISLayer(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, layerName,
                        geoServerDispersionSettings);

                dispersionOutput.addDispLayer(pollutantType, height, layerName);
            }
        }

        boolean append = false;
        if (queryClient.tableExists(EnvConfig.DISPERSION_RASTER_TABLE)) {
            append = true;
            // this is a temporary measure until an option is available to not add raster
            // constraints
            queryClient.dropRasterConstraints();
        }
        aermod.uploadRasterToPostGIS(srid, append);

        // ships_ is hardcoded here and in ShipInputAgent
        queryClient.updateOutputs(derivationInputs.getDerivationIRI(), dispersionOutput, shipLayerName, simulationTime,
                receptorFileURL);
        if (aermod.createDataJson(shipLayerName, dispersionOutput, sourceLayerName, elevationLayerName,
                aermod.getBuildingsGeoJSON(buildings)) != 0) {
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
        }
    }

    void updateDerivations(List<String> derivationsToUpdate) {
        CompletableFuture<String> getAsync = null;
        for (String derivation : derivationsToUpdate) {
            getAsync = CompletableFuture.supplyAsync(() -> {
                try {
                    devClient.updatePureSyncDerivation(derivation);
                    return null; // need to return something, could not get it to work with
                                 // CompletableFuture<Void>
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
     * 
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
            throw new JPSRuntimeException("Failed at closing connection", e);
        } catch (URISyntaxException e) {
            LOGGER.fatal(e.getMessage());
            throw new JPSRuntimeException("Failed at building URL for update request", e);
        }
    }

    void uploadStyle(GeoServerClient geoServerClient) {
        RESTEndpointConfig geoserverEndpointConfig = geoServerClient.readEndpointConfig("geoserver",
                RESTEndpointConfig.class);
        GeoServerRESTManager manager = new GeoServerRESTManager(geoserverEndpointConfig.getUrl(),
                geoserverEndpointConfig.getUserName(), geoserverEndpointConfig.getPassword());

        if (!manager.getReader().existsStyle(EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DISPERSION_STYLE_NAME)) {
            try {
                File sldFile = new File(getClass().getClassLoader().getResource("dispersion.sld").toURI());

                if (manager.getPublisher().publishStyleInWorkspace(EnvConfig.GEOSERVER_WORKSPACE, sldFile,
                        EnvConfig.DISPERSION_STYLE_NAME)) {
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
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());

        queryClient = new QueryClient(storeClient, ontopStoreClient, rdbStoreClient);
        super.devClient = new DerivationClient(storeClient, QueryClient.PREFIX_DISP);
    }

}