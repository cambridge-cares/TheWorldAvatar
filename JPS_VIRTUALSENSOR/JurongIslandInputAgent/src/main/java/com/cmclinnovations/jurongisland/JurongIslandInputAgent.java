package com.cmclinnovations.jurongisland;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import org.apache.commons.io.FilenameUtils;
import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {"/update"})
public class JurongIslandInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(JurongIslandInputAgent.class);
    private static final String JSON_EXT = ".json";
    private QueryClient queryClient;

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig(); 
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class);
        DerivationClient derivationClient = new DerivationClient(storeClient, QueryClient.PREFIX);
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        queryClient = new QueryClient(storeClient, tsClient, derivationClient, remoteRDBStoreClient);
    }

    @Override
    public void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received GET request to update pollutant source data for Jurong Island");
    }

    






    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received POST request to update ship data");
        File dataDir = new File(EnvConfig.DATA_DIR);

        // turn into integer list to facilitate sorting
        List<Integer> fileNamesAsInt = null;
        // catch NumberFormatException here because it is recommended to not throw this in a servlet
        try {
            fileNamesAsInt = Arrays.asList(dataDir.listFiles()).stream().map(f -> Integer.parseInt(FilenameUtils.removeExtension(f.getName()))).collect(Collectors.toList());
        } catch (NumberFormatException e) {
            LOGGER.error(e.getMessage());
        }

        if (fileNamesAsInt != null) {
            Collections.sort(fileNamesAsInt);

            // record last used file in a folder that is a docker volume
            File lastReadFile = new File(EnvConfig.LAST_READ_FILE);
            File timeOffsetFile = new File(EnvConfig.TIME_OFFSET_FILE);
    
            Integer lastUsedFileInt = null;
            int timeOffset = 0;
    
            if (lastReadFile.exists()) {
                // read from file
                // catch NumberFormatException here because it is recommended to not throw this in a servlet
                try {
                    lastUsedFileInt = Integer.parseInt(new String(Files.readAllBytes(lastReadFile.toPath())));
                } catch (NumberFormatException | IOException e) {
                    LOGGER.error(e.getMessage());
                }
            } else {
                // first time creating the file
                updateFile(lastReadFile, String.valueOf(fileNamesAsInt.get(0)));
            }
    
            if (timeOffsetFile.exists()) {
                // read from file
                try {
                    timeOffset = Integer.parseInt(new String(Files.readAllBytes(timeOffsetFile.toPath())));
                } catch (NumberFormatException | IOException e) {
                    LOGGER.error(e.getMessage());
                }
            } else {
                // write the first file
                updateFile(timeOffsetFile, String.valueOf(0));
                timeOffset = 0;
            }
    
            File dataFile;
    
            // should only be null in the first POST call
            if (lastUsedFileInt != null) {
                int index = fileNamesAsInt.indexOf(lastUsedFileInt);
                if (index != fileNamesAsInt.size() - 1) {
                    dataFile = Paths.get(EnvConfig.DATA_DIR, fileNamesAsInt.get(index+1) + JSON_EXT).toFile();
                    updateFile(lastReadFile, String.valueOf(fileNamesAsInt.get(index+1)));
                } else {
                    // increment timeOffset and start a new cycle
                    timeOffset += fileNamesAsInt.size();
                    updateFile(timeOffsetFile, String.valueOf(timeOffset));
                    updateFile(lastReadFile, String.valueOf(fileNamesAsInt.get(0)));
                    dataFile = Paths.get(EnvConfig.DATA_DIR, fileNamesAsInt.get(0) + JSON_EXT).toFile();
                }
            } else {
                dataFile = Paths.get(EnvConfig.DATA_DIR, fileNamesAsInt.get(0) + JSON_EXT).toFile();
            }
    
            FileInputStream inputStream = new FileInputStream(dataFile);

            JSONTokener tokener = new JSONTokener(inputStream);
            JSONArray shipData = new JSONArray(tokener);
            int numship = shipData.length();

            List<Ship> ships = new ArrayList<>(numship);
            // create ship objects from API data
            for (int i = 0; i < numship; i++) {
                try {
                    ships.add(new Ship(shipData.getJSONObject(i), timeOffset));
                } catch (JSONException e) {
                    LOGGER.error(e.getMessage());
                }
            }

            // boolean initialiseObda = false;
            if (!queryClient.initialised()) {
                PostGISClient postGISClient = new PostGISClient();
                Path sqlFunctionFile = new ClassPathResource("function.sql").getFile().toPath();
                String sqlFunction = null;
                try {
                    sqlFunction = Files.readString(sqlFunctionFile);
                } catch (IOException e) {
                    LOGGER.error("Failed to read file containing custom SQL function");
                    LOGGER.error(e.getMessage());
                }
                postGISClient.executeUpdate(EnvConfig.DATABASE, sqlFunction);
                // initialiseObda = true;

                // this adds the OntoAgent triples, only do this once
                queryClient.initialiseAgent();
            }
            // initialise both triples and time series if ship is new
            List<Ship> newlyCreatedShips = queryClient.initialiseShipsIfNotExist(ships);

            // query ship IRI and location measure IRI from the KG and set the IRIs in the object
            queryClient.setShipIRIs(ships);

            // sets course, speed, location measure IRI in ship objects to be used later
            queryClient.setMeasureIri(ships);

            // add a row in RDB time series data, also updates derivation timestamps
            queryClient.updateTimeSeriesData(ships);

            // new derivations are created on the spot (request sent to agent immediately)
            queryClient.createNewDerivations(newlyCreatedShips);

            // calculate average timestep for ship layer name
            long averageTimestamp = ships.stream().mapToLong(s -> s.getTimestamp().getEpochSecond()).sum() / ships.size();
            LOGGER.info("Creating GeoServer layer for the average timestamp = {}", averageTimestamp);
            createGeoServerLayer(averageTimestamp, ships);

            JSONObject responseJson = new JSONObject();
            responseJson.put("averageTimestamp", averageTimestamp);

            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
            resp.getWriter().print(responseJson);

            // // first time adding ontop mapping
            // if (initialiseObda) {
            //     LOGGER.info("Initialising ontop mapping file");
            //     // add ontop mapping
            //     Path obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
            //     new OntopClient().updateOBDA(obdaFile);
            // }
        }
    }

    void updateFile(File file, String fileContent) {
        try (FileOutputStream outputStream = new FileOutputStream(file)) {
            outputStream.write(fileContent.getBytes());
        } catch (Exception e){
            LOGGER.error(e.getMessage());
        }
    }

    void createGeoServerLayer(long averageTimestamp, List<Ship> ships) {
        // build sql query to get points within 30 minutes of average timestamp
        Set<String> shipSet = new HashSet<>();
        ships.forEach(ship -> shipSet.add(String.format("'%s'", ship.getLocationMeasureIri())));
        String shipList = shipSet.stream().collect(Collectors.joining(","));

        StringBuilder queryBuilder = new StringBuilder();
        queryBuilder.append("SELECT timeseries.value as geom, \"dbTable\".\"dataIRI\" as iri ");
        queryBuilder.append("FROM \"dbTable\", public.get_geometry_table(\"tableName\", \"columnName\") as timeseries ");
        queryBuilder.append(String.format("WHERE \"dbTable\".\"dataIRI\" IN (%s) and timeseries.time > %d and timeseries.time < %d", shipList, averageTimestamp-1800, averageTimestamp+1800));
        String sqlQuery = queryBuilder.toString();

        GeoServerClient geoserverClient = new GeoServerClient();
        geoserverClient.createWorkspace(EnvConfig.GEOSERVER_WORKSPACE);

        // hardcoded "ships_" in AermodAgent
        String layerName = "ships_" + averageTimestamp;

        GeoServerVectorSettings geoServerVectorSettings = new GeoServerVectorSettings();

        UpdatedGSVirtualTableEncoder virtualTable = new UpdatedGSVirtualTableEncoder();
        virtualTable.setSql(sqlQuery);
        virtualTable.setEscapeSql(true);
        virtualTable.setName("shipVirtualTable");
        virtualTable.addVirtualTableGeometry("geom", "Point", "4326"); // geom needs to match the sql query
        LOGGER.info(virtualTable.getName());
        geoServerVectorSettings.setVirtualTable(virtualTable);

        geoserverClient.createPostGISLayer(null, EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, layerName, geoServerVectorSettings);
    }
}