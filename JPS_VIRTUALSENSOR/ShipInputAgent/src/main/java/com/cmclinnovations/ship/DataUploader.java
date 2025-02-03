package com.cmclinnovations.ship;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONTokener;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class DataUploader {
    private static final Logger LOGGER = LogManager.getLogger(DataUploader.class);
    private static final String JSON_EXT = ".json";

    public enum ShipDataSource {
        JSON,
        RDB,
        HTTP
    }

    public static int uploadShips(List<Ship> ships, QueryClient queryClient, ShipDataSource source,
            JSONArray tsData) throws IOException {
        if (!queryClient.initialised()) {
            PostGISClient postGISClient = PostGISClient.getInstance();
            Path sqlFunctionFile = new ClassPathResource("function.sql").getFile().toPath();
            String sqlFunction = null;
            try {
                sqlFunction = Files.readString(sqlFunctionFile);
            } catch (IOException e) {
                LOGGER.error("Failed to read file containing custom SQL function");
                LOGGER.error(e.getMessage());
            }
            postGISClient.getRemoteStoreClient(EnvConfig.DATABASE).executeUpdate(sqlFunction);

            // this adds the OntoAgent triples, only do this once
            queryClient.initialiseAgent();
        }
        // initialise both triples and time series if ship is new
        List<Ship> newlyCreatedShips = queryClient.initialiseShipsIfNotExist(ships);

        // query ship IRI and location measure IRI from the KG and set the IRIs in the
        // object
        queryClient.setShipIRIs(ships);

        // update value of ship type if they were initialised without a ship type in
        // previous updates
        queryClient.updateShipType(ships.stream().filter(s -> s.getShipType() != 0).collect(Collectors.toList()));

        // sets course, speed, location measure IRI in ship objects to be used later
        queryClient.setMeasureIri(ships);

        switch (source) {
            case JSON:

                LOGGER.info("Upload ship data from JSON.");
                // add a row in RDB time series data, also updates derivation timestamps
                queryClient.updateTimeSeriesData(ships);

                break;

            case RDB:

                LOGGER.info("Upload ship data from relational database.");
                if (EnvConfig.SKIP_UPDATE_RDB) {
                    // only update time series of new ships
                    // skip updating existing ships
                    queryClient.bulkUpdateTimeSeriesData(newlyCreatedShips, null);
                } else {
                    // update all ships
                    queryClient.bulkUpdateTimeSeriesData(ships, null);
                }
                break;
            
            case HTTP:
            
                LOGGER.info("Upload ship data from HTTP request.");
                queryClient.bulkUpdateTimeSeriesData(newlyCreatedShips, tsData);

            default:

                LOGGER.info("Unknown source of ship data.");
                break;
        }

        // new derivations are created on the spot (request sent to agent immediately)
        queryClient.createNewDerivations(newlyCreatedShips);

        return (newlyCreatedShips.size());

    }

    static List<Ship> uploadDataFromFile(QueryClient queryClient) throws IOException {
        File dataDir = new File(EnvConfig.DATA_DIR);

        // turn into integer list to facilitate sorting
        List<Integer> fileNamesAsInt;
        try {
            fileNamesAsInt = Arrays.asList(dataDir.listFiles()).stream()
                    .filter(f -> !f.getName().contentEquals(".gitignore"))
                    .map(f -> Integer.parseInt(FilenameUtils.removeExtension(f.getName())))
                    .collect(Collectors.toList());
        } catch (NumberFormatException e) {
            throw new RuntimeException("Error reading data files", e);
        }

        Collections.sort(fileNamesAsInt);

        // record last used file in a folder that is a docker volume
        File lastReadFile = new File(EnvConfig.LAST_READ_FILE);
        File timeOffsetFile = new File(EnvConfig.TIME_OFFSET_FILE);

        Integer lastUsedFileInt = null;
        int timeOffset = 0;

        if (lastReadFile.exists()) {
            // read from file
            // catch NumberFormatException here because it is recommended to not throw this
            // in a servlet
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
                dataFile = Paths.get(EnvConfig.DATA_DIR, fileNamesAsInt.get(index + 1) + JSON_EXT).toFile();
                updateFile(lastReadFile, String.valueOf(fileNamesAsInt.get(index + 1)));
            } else {
                // increment timeOffset and start a new cycle
                Instant firstTime = Instant.ofEpochSecond(fileNamesAsInt.get(0));
                Instant lastTime = Instant.ofEpochSecond(fileNamesAsInt.get(fileNamesAsInt.size() - 1));
                Duration timeRange = Duration.between(firstTime, lastTime);
                timeOffset += (int) timeRange.toHours() + 1;
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
        List<Ship> ships = parseShip(shipData, timeOffset);

        uploadShips(ships, queryClient, ShipDataSource.JSON, null);

        return ships;
    }

    static void updateFile(File file, String fileContent) {
        try (FileOutputStream outputStream = new FileOutputStream(file)) {
            outputStream.write(fileContent.getBytes());
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
        }
    }

    static List<Ship> parseShip(JSONArray shipData, int timeOffset) {
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
        return ships;
    }

    public static int loadDataFromRDB(QueryClient queryClient, String derivation) throws IOException {
        String polygonQuery = "(SELECT \"geom\" FROM \"scopes\")";
        if (derivation != null) {
            try {
                polygonQuery = "(SELECT \"geom\" FROM \"scopes\" WHERE \"iri\" = '"
                        + queryClient.getScopeIRI(derivation) + "')";
            } catch (Exception e) {
                LOGGER.warn("Unable to find the scope of the specified derivation");
            }
        }
        String sqlQuery = "SELECT DISTINCT points.\"MMSI\", points.\"VesselType\" AS \"SHIPTYPE\", 0 AS \"SPEED\", " + //
                " 0.0 AS \"COURSE\", 0.0 AS \"LAT\", 0.0 AS \"LON\", '2024-01-01T12:00:00' AS \"TIMESTAMP\" " + //
                "FROM (SELECT \"MMSI\", \"VesselType\", \"geom\" FROM \"ship\") AS points, " + //
                polygonQuery + " AS polygon WHERE " + //
                "ST_Contains(polygon.geom, points.geom)";
        LOGGER.info(polygonQuery);
        JSONArray shipData = queryClient.getRemoteRDBStoreClient().executeQuery(sqlQuery);
        LOGGER.info(String.format("Number of ships within scopes: %d", shipData.length()));
        List<Ship> ships = parseShip(shipData, 0);

        // initialise both triples and time series if ship is new

        return uploadShips(ships, queryClient, ShipDataSource.RDB, null);
    }

    public static int parseData(QueryClient queryClient, JSONArray shipData, JSONArray tsData) throws IOException{

        List<Ship> ships = parseShip(shipData, 0);

        return uploadShips(ships, queryClient, ShipDataSource.HTTP, tsData);
    }

    private DataUploader() {
        throw new IllegalStateException("DataUploader");
    }
}
