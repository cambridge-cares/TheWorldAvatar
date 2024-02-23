package com.cmclinnovations.ship;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
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

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = { ShipInputAgent.UPDATE_PATH, ShipInputAgent.LIVE_SERVER_PATH }, loadOnStartup = 1)
public class ShipInputAgent extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(ShipInputAgent.class);
    private QueryClient queryClient;
    private AisStreamWebsocketClient client;
    public static final String UPDATE_PATH = "/update";
    public static final String LIVE_SERVER_PATH = "/live-server";
    private static final String URI_STRING = "wss://stream.aisstream.io/v0/stream";

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig();
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<>(storeClient, Instant.class);
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
            URI uri;
            try {
                uri = new URI(URI_STRING);
            } catch (URISyntaxException e) {
                throw new RuntimeException(e);
            }

            client = new AisStreamWebsocketClient(uri, queryClient);
            client.connect();
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
        } else {
            LOGGER.warn("Unsupported path for DELETE request");
        }
    }
}