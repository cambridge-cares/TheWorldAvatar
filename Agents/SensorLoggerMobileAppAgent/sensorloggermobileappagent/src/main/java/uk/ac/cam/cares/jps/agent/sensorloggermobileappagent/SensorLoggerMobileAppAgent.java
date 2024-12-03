package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.Point;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.zip.GZIPInputStream;

@WebServlet(urlPatterns = { "/update", "/instantiate" })

public class SensorLoggerMobileAppAgent extends JPSAgent {
    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerMobileAppAgent.class);
    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontopClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private AgentConfig agentConfig;
    private ExecutorService addDataExecutor;
    private ExecutorService sendDataExecutor;
    private static final HashMap<String, SmartphoneRecordingTask> smartphoneHashmap = new HashMap<>();

    public void init() {
        loggerTest();

        agentConfig = new AgentConfig();

        EndpointConfig endpointConfig = new EndpointConfig();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(),
                endpointConfig.getDbpassword());
        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
        ontopClient = new RemoteStoreClient(endpointConfig.getOntopUrl());

        addDataExecutor = Executors.newFixedThreadPool(5);
        sendDataExecutor = Executors.newFixedThreadPool(5);

        Timer taskScanningTimer = new Timer();
        taskScanningTimer.schedule(new TimerTask() {
            @Override
            public void run() {
                synchronized (smartphoneHashmap) {
                    List<String> inactiveTask = new ArrayList<>();
                    for (Map.Entry<String, SmartphoneRecordingTask> entry : smartphoneHashmap.entrySet()) {
                        SmartphoneRecordingTask task = entry.getValue();
                        String deviceId = entry.getKey();

                        if (task.shouldTerminateTask()) {
                            LOGGER.info(deviceId + ": flush all data out");
                            sendDataExecutor.submit(task::processAndSendData);
                            inactiveTask.add(deviceId);
                        } else if (task.shouldProcessData()) {
                            sendDataExecutor.submit(task::processAndSendData);
                        }
                    }

                    if (!inactiveTask.isEmpty()) {
                        LOGGER.info(String.format("tasks: %s are inactive and has been removed from the hashmap",
                                String.join(",", inactiveTask)));
                        inactiveTask.forEach(smartphoneHashmap::remove);
                    }
                }
            }
        }, agentConfig.getTimerDelay() * 1000L, agentConfig.getTimerFrequency() * 1000L);
    }

    private void loggerTest() {
        LOGGER.debug("This is a debug message.");
        LOGGER.info("This is an info message.");
        LOGGER.warn("This is a warn message.");
        LOGGER.error("This is an error message.");
        LOGGER.fatal("This is a fatal message.");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        JSONObject result = new JSONObject();
        if (request.getServletPath().contentEquals("/instantiate")) {
            String deviceId = requestParams.getString("deviceId");

            SmartphoneRecordingTask task = new SmartphoneRecordingTask(storeClient, rdbStoreClient, agentConfig,
                    deviceId, ontopClient);
            task.instantiate();
            result.put("message", "instantiated deviceId = " + deviceId);
        } else {
            if (!validateInput(requestParams)) {
                result.put("message", "one or more of the request params is missing");
            }

            // extract the compressed data string from the request
            String deviceId = requestParams.getString("deviceId");
            String sessionId = requestParams.getString("sessionId");
            LOGGER.info(deviceId + ": receive request");
            result.put("message", deviceId + ": receive request");

            SmartphoneRecordingTask task = getSmartphoneRecordingTask(deviceId);
            addDataExecutor.submit(() -> {
                try {
                    if (requestParams.has("compressedData")) {
                        String compressedDataString = requestParams.getString("compressedData");

                        LOGGER.info(deviceId + " is fetched and start to add data");

                        byte[] compressedData = Base64.getDecoder().decode(compressedDataString);
                        String decompressedData = decompressGzip(compressedData);
                        JSONArray payload = new JSONArray(decompressedData);
                        HashMap<String, List<?>> dataHashmap = processRequestQueue(payload, sessionId);

                        task.addData(dataHashmap);
                    } else {
                        // retrieving non-compressed data
                        JSONArray payload = requestParams.getJSONArray("payload");
                        HashMap<String, List<?>> dataHashmap = processRequestQueue(payload, sessionId);
                        LOGGER.info(deviceId + " is fetched and start to add data");
                        task.addData(dataHashmap);
                    }
                } catch (JsonProcessingException jsonError) {
                    // handle JsonProcessingException
                } catch (IOException ioError) {
                    // handle IOException
                }
            });

            LOGGER.info(deviceId + " data being added");
            result.put("message", "data being processed");
        }
        return result;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        return requestParams.has("messageId")
                && requestParams.has("sessionId")
                && requestParams.has("deviceId")
                && (requestParams.has("payload") || requestParams.has("compressedData"));
    }

    private String decompressGzip(byte[] compressedData) throws IOException {
        try (GZIPInputStream gzipInputStream = new GZIPInputStream(new ByteArrayInputStream(compressedData));
                InputStreamReader inputStreamReader = new InputStreamReader(gzipInputStream, StandardCharsets.UTF_8);
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {

            StringBuilder outStr = new StringBuilder();
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                outStr.append(line);
            }
            return outStr.toString();
        }
    }

    private SmartphoneRecordingTask getSmartphoneRecordingTask(String deviceId) {
        synchronized (smartphoneHashmap) {
            if (smartphoneHashmap.containsKey(deviceId)) {
                return smartphoneHashmap.get(deviceId);
            }

            LOGGER.info(deviceId + ": creating new task");
            SmartphoneRecordingTask task = new SmartphoneRecordingTask(storeClient, rdbStoreClient, agentConfig,
                    deviceId, ontopClient);
            smartphoneHashmap.put(deviceId, task);
            return task;
        }
    }

    private HashMap<String, List<?>> processRequestQueue(JSONArray payload, String sessionId)
            throws JsonProcessingException {
        // Accelerometer list
        ArrayList<OffsetDateTime> accel_tsList = new ArrayList<>();
        List<Double> accelList_x = new ArrayList<>();
        List<Double> accelList_y = new ArrayList<>();
        List<Double> accelList_z = new ArrayList<>();

        // Magnetometer list
        ArrayList<OffsetDateTime> magnetometer_tsList = new ArrayList<>();
        List<Double> magnetometerList_x = new ArrayList<>();
        List<Double> magnetometerList_y = new ArrayList<>();
        List<Double> magnetometerList_z = new ArrayList<>();

        // Gravity sensor list
        ArrayList<OffsetDateTime> gravity_tsList = new ArrayList<>();
        List<Double> gravityList_x = new ArrayList<>();
        List<Double> gravityList_y = new ArrayList<>();
        List<Double> gravityList_z = new ArrayList<>();

        // Location list
        ArrayList<OffsetDateTime> location_tsList = new ArrayList<>();
        List<Double> bearingList = new ArrayList<>();
        List<Double> speedList = new ArrayList<>();
        List<Double> altitudeList = new ArrayList<>();
        List<Point> geomLocationList = new ArrayList<>();
        List<String> sessionIdList = new ArrayList<>();

        // Microphone lists
        ArrayList<OffsetDateTime> dBFS_tsList = new ArrayList<>();
        List<Double> dBFSList = new ArrayList<>();

        // Light value lists
        ArrayList<OffsetDateTime> lightValue_tsList = new ArrayList<>();
        List<Double> lightValueList = new ArrayList<>();

        ArrayList<OffsetDateTime> brightness_tsList = new ArrayList<>();
        List<Double> brightnessList = new ArrayList<>();

        ObjectMapper mapper = new ObjectMapper();
        JsonNode payloadNode = mapper.readTree(payload.toString());

        for (JsonNode node : payloadNode) {

            JsonNode timeEPOCH = node.get("time");
            JsonNode sensor = node.get("name");
            JsonNode values = node.get("values");
            Instant instant = Instant.ofEpochSecond(timeEPOCH.longValue() / 1000000000,
                    timeEPOCH.longValue() % 1000000000);
            OffsetDateTime timestamp = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC);

            if (sensor.textValue().equals("accelerometer")) {
                accel_tsList.add(timestamp);
                accelList_x.add(values.get("x").doubleValue());
                accelList_y.add(values.get("y").doubleValue());
                accelList_z.add(values.get("z").doubleValue());
            }

            if (sensor.textValue().equals("magnetometer")) {
                magnetometer_tsList.add(timestamp);
                magnetometerList_x.add(values.get("x").doubleValue());
                magnetometerList_y.add(values.get("y").doubleValue());
                magnetometerList_z.add(values.get("z").doubleValue());

            }

            if (sensor.textValue().equals("gravity")) {
                gravity_tsList.add(timestamp);
                gravityList_x.add(values.get("x").doubleValue());
                gravityList_y.add(values.get("y").doubleValue());
                gravityList_z.add(values.get("z").doubleValue());

            }

            if (sensor.textValue().equals("location")) {
                location_tsList.add(timestamp);
                bearingList.add(values.get("bearing").doubleValue());
                speedList.add(values.get("speed").doubleValue());
                altitudeList.add(values.get("altitude").doubleValue());

                // Parse latitude and longitude into geom_location
                double latitude = values.get("latitude").doubleValue();
                double longitude = values.get("longitude").doubleValue();
                Point point = new Point(longitude, latitude);
                point.setSrid(4326);

                geomLocationList.add(point);
                sessionIdList.add(sessionId);
            }

            if (sensor.textValue().equals("microphone")) {
                dBFS_tsList.add(timestamp);
                dBFSList.add(values.get("dBFS").doubleValue());
            }

            if (sensor.textValue().equals("light")) {
                lightValue_tsList.add(timestamp);
                lightValueList.add(values.get("lux").doubleValue());
            }

            if (sensor.textValue().equals("brightness")) {
                brightness_tsList.add(timestamp);
                brightnessList.add(values.get("brightness").doubleValue());
            }
        }

        HashMap<String, List<?>> dataHashmap = new HashMap<>();
        dataHashmap.put("accel_tsList", accel_tsList);
        dataHashmap.put("accelList_x", accelList_x);
        dataHashmap.put("accelList_y", accelList_y);
        dataHashmap.put("accelList_z", accelList_z);
        dataHashmap.put("magnetometer_tsList", magnetometer_tsList);
        dataHashmap.put("magnetometerList_x", magnetometerList_x);
        dataHashmap.put("magnetometerList_y", magnetometerList_y);
        dataHashmap.put("magnetometerList_z", magnetometerList_z);
        dataHashmap.put("gravity_tsList", gravity_tsList);
        dataHashmap.put("gravityList_x", gravityList_x);
        dataHashmap.put("gravityList_y", gravityList_y);
        dataHashmap.put("gravityList_z", gravityList_z);
        dataHashmap.put("location_tsList", location_tsList);
        dataHashmap.put("bearingList", bearingList);
        dataHashmap.put("speedList", speedList);
        dataHashmap.put("altitudeList", altitudeList);
        dataHashmap.put("geomLocationList", geomLocationList);
        dataHashmap.put("sessionIdList", sessionIdList);
        dataHashmap.put("dBFS_tsList", dBFS_tsList);
        dataHashmap.put("dBFSList", dBFSList);
        dataHashmap.put("lightValue_tsList", lightValue_tsList);
        dataHashmap.put("lightValueList", lightValueList);
        dataHashmap.put("brightness_tsList", brightness_tsList);
        dataHashmap.put("brightnessList", brightnessList);

        return dataHashmap;
    }

}