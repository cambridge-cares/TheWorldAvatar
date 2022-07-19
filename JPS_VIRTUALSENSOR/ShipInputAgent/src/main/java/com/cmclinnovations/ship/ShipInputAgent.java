package com.cmclinnovations.ship;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONTokener;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {"/update"})
public class ShipInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(ShipInputAgent.class);

    private static TimeSeriesClient<Instant> tsClient;
    private static RemoteStoreClient storeClient;
    private static QueryClient client;
    private int fileIndex = -999;

    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        LOGGER.info("Received POST request to update ship data");
        if (Config.KG_URL == null || Config.POSTGRES_URL == null) {
            Config.initURLs();
            storeClient = new RemoteStoreClient(Config.KG_URL, Config.KG_URL);
            tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.POSTGRES_URL, Config.POSTGRES_USER, Config.POSTGRES_PASSWORD);
            client = new QueryClient(storeClient, tsClient);
        }
        
        List<Ship> ships = mockShipAPI();
        // initialise both triples and time series if ship is new
        client.initialiseShipsIfNotExist(ships);

        // query ship IRIs from the KG and set the IRIs in the object
        client.setShipIRIs(ships);

        // add a row in RDB time series data
        client.updateTimeSeriesData(ships);
    }

    List<Ship> mockShipAPI()  {
        File dataDir = new File(Config.DATA_DIR);
        File[] dataFiles = dataDir.listFiles();

        if (fileIndex == -999 || fileIndex == dataFiles.length) {
            fileIndex = 0;
        }
        
        FileInputStream inputStream;
        try {
            inputStream = new FileInputStream(dataFiles[fileIndex]);
        } catch (FileNotFoundException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }

        JSONTokener tokener = new JSONTokener(inputStream);
        JSONArray shipData = new JSONArray(tokener);
        int numship = shipData.length();

        List<Ship> ships = new ArrayList<Ship>(numship);
        // create ship objects from API data
        for (int i = 0; i < numship; i++) {
            ships.add(new Ship(shipData.getJSONObject(i)));
        }
        // set data obtained to the current time
        Instant currentTime = Instant.now();
        for (Ship ship : ships) {
            ship.setTimestamp(currentTime);
        }

        fileIndex += 1; // increment for next call
        return ships;
    }
}
