package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
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
                        task.addData(new Payload(payload, sessionId));
                    } else {
                        // retrieving non-compressed data
                        JSONArray payload = requestParams.getJSONArray("payload");
                        LOGGER.info(deviceId + " is fetched and start to add data");
                        task.addData(new Payload(payload, sessionId));
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

}