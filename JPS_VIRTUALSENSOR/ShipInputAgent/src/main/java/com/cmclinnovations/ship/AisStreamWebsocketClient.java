package com.cmclinnovations.ship;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.logging.log4j.Logger;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.json.JSONObject;

public class AisStreamWebsocketClient extends WebSocketClient {
    private static final Logger LOGGER = LogManager.getLogger(AisStreamWebsocketClient.class);
    private QueryClient queryClient;
    private Map<Integer, Ship> mmsiToShipMap;
    private Instant start;
    private int numMessages;
    private Thread thread;

    public AisStreamWebsocketClient(URI uri, QueryClient queryClient) {
        super(uri);
        this.queryClient = queryClient;
        mmsiToShipMap = new HashMap<>();
    }

    @Override
    public void onClose(int code, String reason, boolean remote) {
        // The close codes are documented in class org.java_websocket.framing.CloseFrame
        LOGGER.error("Connection closed error code: {}", code);
        LOGGER.error("Connection closed reason: {}", reason);
        HttpPost post = new HttpPost("http://localhost:8080/ShipInputAgent" + ShipInputAgent.RESTART_PATH);

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse response = httpClient.execute(post);) {
            LOGGER.info("Restarting live updates");
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Error during restart");
        }
    }

    @Override
    public void onOpen(ServerHandshake handshakedata) {
        // send subscription message upon connection
        start = Instant.now();
        send(String.format(
                "{\"APIKey\":\"%s\",\"BoundingBoxes\":%s, \"FilterMessageTypes\": [\"PositionReport\", \"ShipStaticData\"]}",
                EnvConfig.API_KEY, EnvConfig.BOUNDING_BOXES));
    }

    @Override
    public void onMessage(ByteBuffer message) {
        numMessages += 1;
        LOGGER.info("Number of collated messages: {}", numMessages);
        String messageString = StandardCharsets.UTF_8.decode(message).toString();

        JSONObject messageJson = new JSONObject(messageString);
        JSONObject metaData = messageJson.getJSONObject("MetaData");

        int mmsi = messageJson.getJSONObject("MetaData").getInt("MMSI");

        // remove nanoseconds and convert string to ISO standard
        Instant timestampInstant = LocalDateTime.parse(metaData.getString("time_utc").split("\\.")[0].replace(" ", "T"))
                .toInstant(ZoneOffset.UTC);

        mmsiToShipMap.computeIfAbsent(mmsi, Ship::new);

        if (messageJson.getString("MessageType").contentEquals("PositionReport")) {
            JSONObject positionReport = messageJson.getJSONObject("Message").getJSONObject("PositionReport");
            double lat = positionReport.getDouble("Latitude");
            double lon = positionReport.getDouble("Longitude");
            double speed = positionReport.getDouble("Sog");
            double cog = positionReport.getDouble("Cog");
            mmsiToShipMap.get(mmsi).addTimeSeriesData(timestampInstant, speed, lat, lon, cog);

        } else if (messageJson.getString("MessageType").contentEquals("ShipStaticData")) {
            JSONObject staticData = messageJson.getJSONObject("Message").getJSONObject("ShipStaticData");
            int shipType = staticData.getInt("Type");
            mmsiToShipMap.get(mmsi).setShipType(shipType);
        }

        if (!metaData.getString("ShipName").trim().isBlank()) {
            mmsiToShipMap.get(mmsi).setShipName(metaData.getString("ShipName").trim());
        } else {
            mmsiToShipMap.get(mmsi).setShipName("Ship" + mmsi);
        }

        Duration elapsedTime = Duration.between(start, Instant.now());

        int uploadInterval = 10;
        if (EnvConfig.UPLOAD_INTERVAL_MINUTES != null) {
            uploadInterval = Integer.parseInt(EnvConfig.UPLOAD_INTERVAL_MINUTES);
        }

        if (elapsedTime.toMinutes() > uploadInterval) {
            LOGGER.info("Accumulated {} minutes' worth of data, uploading consolidated ship data", uploadInterval);
            List<Ship> ships = new ArrayList<>(mmsiToShipMap.values());
            try {
                if (thread != null && thread.isAlive()) {
                    LOGGER.info("Previous thread is still alive, waiting for it to finish");
                    thread.join();
                }
            } catch (InterruptedException e) {
                LOGGER.error("Error from previous uploading data thread");
                LOGGER.error(e.getMessage());
                Thread.currentThread().interrupt();
            }

            thread = new Thread(() -> {
                try {
                    DataUploader.uploadShips(ships, queryClient);
                } catch (IOException e) {
                    LOGGER.warn("Error in uploading ship data");
                    LOGGER.warn(e.getMessage());
                }
            });
            thread.start();
            mmsiToShipMap = new HashMap<>();
            numMessages = 0;
            start = Instant.now();
            LOGGER.info("Resetting");
            LOGGER.info("numMessages = {}", numMessages);
        }
    }

    @Override
    public void onError(Exception arg0) {
        throw new UnsupportedOperationException(arg0);
    }

    @Override
    public void onMessage(String arg0) {
        throw new UnsupportedOperationException("Unimplemented method 'onMessage'");
    }
}
