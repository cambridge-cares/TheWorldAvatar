package com.cmclinnovations.ship;

import java.io.IOException;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.json.JSONObject;

public class AisStreamWebsocketClient extends WebSocketClient {
    private static final Logger LOGGER = LogManager.getLogger(AisStreamWebsocketClient.class);
    private QueryClient queryClient;

    public AisStreamWebsocketClient(URI uri, QueryClient queryClient) {
        super(uri);
        this.queryClient = queryClient;
    }

    @Override
    public void onClose(int code, String reason, boolean remote) {
        // The close codes are documented in class org.java_websocket.framing.CloseFrame
        LOGGER.error("Connection closed error code: {}", code);
        LOGGER.error("Connection closed reason: {}", reason);

        LOGGER.info("Reconnecting");
        close();
        connect();
    }

    @Override
    public void onOpen(ServerHandshake handshakedata) {
        // send subscription message upon connection
        send(String.format(
                "{\"APIKey\":\"%s\",\"BoundingBoxes\":%s, \"FilterMessageTypes\": [\"PositionReport\"]}",
                EnvConfig.API_KEY, EnvConfig.BOUNDING_BOXES));
    }

    @Override
    public void onMessage(ByteBuffer message) {
        String messageString = StandardCharsets.UTF_8.decode(message).toString();

        JSONObject messageJson = new JSONObject(messageString);
        JSONObject metaData = messageJson.getJSONObject("MetaData");
        JSONObject positionReport = messageJson.getJSONObject("Message").getJSONObject("PositionReport");

        Ship ship = new Ship();
        ship.setMmsi(messageJson.getJSONObject("MetaData").getInt("MMSI"));
        ship.setLat(positionReport.getDouble("Latitude"));
        ship.setLon(positionReport.getDouble("Longitude"));
        ship.setSpeed(positionReport.getDouble("Sog"));
        if (!metaData.getString("ShipName").trim().isBlank()) {
            ship.setShipName(metaData.getString("ShipName").trim());
        }

        // Parse the timestamp string, remove nanoseconds and convert string to ISO
        // standard
        Instant timestampInstant = LocalDateTime.parse(metaData.getString("time_utc").split("\\.")[0].replace(" ", "T"))
                .toInstant(ZoneOffset.UTC);

        ship.setTimestamp(timestampInstant);

        if (queryClient.shipExists()) {
            Thread newThread = new Thread(() -> {
                try {
                    DataUploader.uploadShips(List.of(ship), queryClient);
                } catch (IOException e) {
                    LOGGER.warn("Error in uploading ship data");
                    LOGGER.warn(e.getMessage());
                }
            });
            newThread.start();
        } else {
            try {
                DataUploader.uploadShips(List.of(ship), queryClient);
            } catch (IOException e) {
                LOGGER.warn("Error in uploading ship data");
                LOGGER.warn(e.getMessage());
            }
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
