package com.cmclinnovations.ship;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONTokener;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class ShipDataUploader {
    private static final Logger LOGGER = LogManager.getLogger(ShipDataUploader.class);

    public static void main(String[] args) throws FileNotFoundException {
        EndpointConfig endpointConfig = new EndpointConfig(); 
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<Long> tsClient = new TimeSeriesClient<>(storeClient, Long.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        QueryClient client = new QueryClient(storeClient, tsClient);

        File dataDir = new File(EnvConfig.DATA_DIR);
        File[] dataFiles = dataDir.listFiles();

        for (File dataFile : dataFiles) {
            FileInputStream inputStream = new FileInputStream(dataFile);

            JSONTokener tokener = new JSONTokener(inputStream);
            JSONArray shipData = new JSONArray(tokener);
            int numship = shipData.length();

            List<Ship> ships = new ArrayList<>(numship);
            // create ship objects from API data
            for (int i = 0; i < numship; i++) {
                try {
                    ships.add(new Ship(shipData.getJSONObject(i)));
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
}
