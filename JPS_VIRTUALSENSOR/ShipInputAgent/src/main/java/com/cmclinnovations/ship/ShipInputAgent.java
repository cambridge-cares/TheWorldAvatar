package com.cmclinnovations.ship;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

@WebServlet(urlPatterns = { ShipInputAgent.UPDATE_PATH, ShipInputAgent.LIVE_SERVER_PATH,
        ShipInputAgent.CLEAR_OLD_DATA_PATH, ShipInputAgent.LOAD_RDB_PATH,
        ShipInputAgent.PARSE_PATH }, loadOnStartup = 1)
public class ShipInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(ShipInputAgent.class);
    private QueryClient queryClient;
    private AisStreamWebsocketClient client;
    public static final String UPDATE_PATH = "/update";
    public static final String LIVE_SERVER_PATH = "/live-server";
    public static final String CLEAR_OLD_DATA_PATH = "/clear-old-data";
    public static final String LOAD_RDB_PATH = "/load-rdb";
    public static final String PARSE_PATH = "/parse";
    private static final String URI_STRING = "wss://stream.aisstream.io/v0/stream";

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig();
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesRDBClientWithReducedTables<Instant> timeSeriesRdbClient = new TimeSeriesRDBClientWithReducedTables<>(Instant.class);
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(storeClient, timeSeriesRdbClient);
        DerivationClient derivationClient = new DerivationClient(storeClient, QueryClient.PREFIX);
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        queryClient = new QueryClient(storeClient, tsClient, derivationClient, remoteRDBStoreClient);

        if (EnvConfig.USE_LIVE_DATA) {
            URI uri;
            try {
                uri = new URI(URI_STRING);
            } catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }
            client = new AisStreamWebsocketClient(uri, queryClient);
            client.setReconnect(true);
            client.connect();
        }
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        if (req.getServletPath().contentEquals(LIVE_SERVER_PATH)) {
            if (client != null && client.isOpen()) {
                LOGGER.info("Live updates ongoing, ignoring POST request");
            } else {
                URI uri;
                try {
                    uri = new URI(URI_STRING);
                } catch (URISyntaxException e) {
                    throw new RuntimeException(e);
                }

                client = new AisStreamWebsocketClient(uri, queryClient);
                client.connect();
            }
        } else if (req.getServletPath().contentEquals(UPDATE_PATH)) {
            // read ship data from data folder
            LOGGER.info("Received POST request to update ship data");
            List<Ship> ships = DataUploader.uploadDataFromFile(queryClient);

            long averageTimestamp = ships.stream().mapToLong(s -> s.getTimestamp().getEpochSecond()).sum()
                    / ships.size();

            JSONObject responseJson = new JSONObject();
            responseJson.put("averageTimestamp", averageTimestamp);

            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
            resp.getWriter().print(responseJson);
        } else if (req.getServletPath().contentEquals(LOAD_RDB_PATH)) {
            // read ship data from postgres
            LOGGER.info("Received POST request to load ship data from relational database");
            String derivation = req.getParameter("derivation");
            int newNewShips = DataUploader.loadDataFromRDB(queryClient, derivation);

            JSONObject responseJson = new JSONObject();
            responseJson.put("numNewShips", newNewShips);

            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
            resp.getWriter().print(responseJson);

        } else if (req.getServletPath().contentEquals(PARSE_PATH)) {
            LOGGER.info("Received POST request to parse ship data on demand");
            StringBuilder sb = new StringBuilder();
            BufferedReader reader = req.getReader();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line);
            }
            String jsonString = sb.toString();
            LOGGER.info(jsonString);

            // Parse the JSON
            JSONObject jsonObject = new JSONObject(jsonString);
            JSONArray shipDataArray = jsonObject.getJSONArray("shipData");
            JSONArray tsDataArray = jsonObject.getJSONArray("tsData");

            int newNewShips = DataUploader.parseData(queryClient, shipDataArray, tsDataArray);

            JSONObject responseJson = new JSONObject();
            responseJson.put("newNewSteps", newNewShips);

            resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
            resp.setCharacterEncoding("UTF-8");
            resp.getWriter().print(responseJson);

        } else {
            LOGGER.warn("Unsupported path for POST request");
        }
    }

    @Override
    protected void doDelete(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        if (req.getServletPath().contentEquals(LIVE_SERVER_PATH)) {
            LOGGER.info("Received DELETE request to stop live updates");

            if (client != null) {
                client.setReconnect(false);
                client.close();
            } else {
                LOGGER.info("No ongoing live updates to stop");
            }
        } else if (req.getServletPath().contentEquals(CLEAR_OLD_DATA_PATH)) {
            LOGGER.info("Received DELETE request to clear old ship data");
            // number of days before current time to keep
            String daysBefore = req.getParameter("daysBefore");
            if (daysBefore != null) {
                long daysBeforeLong = Long.parseLong(daysBefore);
                LOGGER.info("Deleting data before {}", Instant.now().minus(daysBeforeLong, ChronoUnit.DAYS));

                List<String> shipsToDelete = queryClient.cleanUpTimeSeries(daysBeforeLong);
                queryClient.deleteShipDerivation(shipsToDelete);
                queryClient.deleteShips(shipsToDelete);
                LOGGER.info("Delete complete");
            } else {
                throw new RuntimeException("daysBefore value is needed");
            }
        } else {
            LOGGER.warn("Unsupported path for DELETE request");
        }
    }
}