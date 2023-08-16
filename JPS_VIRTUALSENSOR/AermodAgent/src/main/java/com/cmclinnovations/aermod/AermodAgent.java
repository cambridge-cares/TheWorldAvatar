package com.cmclinnovations.aermod;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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

        long timeBuffer = 1800; // 30 minutes
        List<Ship> ships = queryClient.getShipsWithinTimeAndScopeViaTsClient(simulationTime, scope, timeBuffer);

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
        derivationsToUpdate.parallelStream().forEach(derivation -> devClient.updatePureSyncDerivation(derivation));

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
                aermod.createAERMAPInput(elevData, centreZoneNumber);
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
        aermod.runAermet(scope);

        // Upload point sources layer to POSTGIS and GeoServer
        String staticPointSourceLayer = null;
        if (!staticPointSources.isEmpty()) {
            staticPointSourceLayer = aermod.createStaticPointSourcesLayer(staticPointSources);
        }

        String shipLayerName = null;
        if (!ships.isEmpty()) {
            shipLayerName = aermod.createShipLayer(simulationTime, ships, timeBuffer);
        }

        // The receptor.dat file may have been previously created by running AERMAP. If
        // so, it should not be overwritten.
        Path receptorPath = simulationDirectory.resolve("aermod").resolve("receptor.dat");
        if (Files.notExists(receptorPath))
            aermod.createAERMODReceptorInput(scope, nx, ny, srid);

        // DispersionOutput object holds dispersion matrix (file), dispersion layer
        // names, and raster filenames
        DispersionOutput dispersionOutput = new DispersionOutput();

        for (PollutantType pollutantType : Pollutant.getPollutantList()) {
            if (!allSources.stream().allMatch(p -> p.hasPollutant(pollutantType))) {
                continue;
            }
            aermod.createPollutantSubDirectory(pollutantType);

            // create emissions input
            aermod.createPointsFile(allSources, srid, pollutantType);
            aermod.runAermod(pollutantType);

            // Upload files used by scripts within Python Service to file server.
            Path concFile = simulationDirectory.resolve(
                    Paths.get("aermod", Pollutant.getPollutantLabel(pollutantType), "averageConcentration.dat"));
            String outputFileURL = aermod.uploadToFileServer(concFile);
            dispersionOutput.addDispMatrix(pollutantType, outputFileURL);

            String rasterFileName = UUID.randomUUID().toString();
            try {
                aermod.createFileForRaster(rasterFileName, pollutantType);
                dispersionOutput.addDispRaster(pollutantType, rasterFileName + ".tif");
            } catch (FileNotFoundException e) {
                LOGGER.error("Average concentration file not found, probably failed to run simulation");
            }

            // Get contour plots as geoJSON objects from PythonService and upload them to
            // PostGIS using GDAL
            JSONObject response = aermod.getGeoJsonAndColourbar(EnvConfig.PYTHON_SERVICE_URL, outputFileURL, srid);
            String colourBarUrl = response.getString("colourbar");
            dispersionOutput.addColourBar(pollutantType, colourBarUrl);

            JSONObject geoJSON = response.getJSONObject("contourgeojson");
            String dispersionLayerName = aermod.createDispersionLayer(geoJSON, derivationInputs.getDerivationIRI(),
                    pollutantType, simulationTime);

            dispersionOutput.addDispLayer(pollutantType, dispersionLayerName);
        }

        boolean append = false;
        if (queryClient.tableExists(EnvConfig.DISPERSION_RASTER_TABLE)) {
            append = true;
            // this is a temporary measure until an option is available to not add raster
            // constraints
            queryClient.dropRasterConstraints();
        }
        aermod.uploadRasterToPostGIS(srid, append);

        queryClient.updateOutputs(derivationInputs.getDerivationIRI(), dispersionOutput, shipLayerName, simulationTime,
                staticPointSourceLayer);
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