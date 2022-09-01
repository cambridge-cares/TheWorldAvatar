package com.cmclinnovations.ship;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONTokener;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {"/update"})
public class ShipInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(ShipInputAgent.class);
    private static final String JSON_EXT = ".json";

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received POST request to update ship data");
        
        EndpointConfig endpointConfig = new EndpointConfig(); 
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        QueryClient client = new QueryClient(storeClient, tsClient);

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
            Integer timeOffset = null;
    
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

            // initialise both triples and time series if ship is new
            client.initialiseShipsIfNotExist(ships);

            // query ship IRIs from the KG and set the IRIs in the object
            client.setShipIRIs(ships);

            // add a row in RDB time series data
            client.updateTimeSeriesData(ships);
        }
    }

    void updateFile(File file, String fileContent) {
        try (FileOutputStream outputStream = new FileOutputStream(file)) {
            outputStream.write(fileContent.getBytes());
        } catch (Exception e){
            LOGGER.error(e.getMessage());
        }
    }
}