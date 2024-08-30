package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor.*;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class SmartphoneRecordingTask {

    Logger LOGGER;

    AccelerometerProcessor accelerometerProcessor;
    DBFSDataProcessor dbfsDataProcessor;
    GravityDataProcessor gravityDataProcessor;
    IlluminationProcessor illuminationProcessor;
    LocationDataProcessor locationDataProcessor;
    MagnetometerDataProcessor magnetometerDataProcessor;
    RelativeBrightnessProcessor relativeBrightnessProcessor;
    List<SensorDataProcessor> sensorDataProcessorList;

    private final Node smartphoneIRI;
    private final String deviceId;

    private final RemoteStoreClient storeClient;
    private final TimeSeriesClient<OffsetDateTime> tsClient;
    private final AgentConfig config;

    private long lastProcessedTime;
    private long lastActiveTime;
    private boolean isProcessing;

    public SmartphoneRecordingTask(RemoteStoreClient storeClient, RemoteRDBStoreClient rdbStoreClient, AgentConfig config, String deviceId) {
        LOGGER = LogManager.getLogger("SmartphoneRecordingTask_" + deviceId);

        this.storeClient = storeClient;
        this.tsClient = new TimeSeriesClient<>(storeClient, OffsetDateTime.class,
                rdbStoreClient.getRdbURL(), rdbStoreClient.getUser(), rdbStoreClient.getPassword());
        this.config = config;
        lastActiveTime = System.currentTimeMillis();
        lastProcessedTime = System.currentTimeMillis();

        String smartphoneString = "https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_" + deviceId;
        smartphoneIRI = NodeFactory.createURI(smartphoneString);
        this.deviceId = deviceId;

        initSensorProcessors();

    }

    public synchronized void addData(HashMap<String, List<?>> data) {
        LOGGER.info("adding data...");
        sensorDataProcessorList.forEach(p -> p.addData(data));
        lastActiveTime = System.currentTimeMillis();
        LOGGER.info("finish adding data and the last active time is updated to " + lastActiveTime);
    }

    public synchronized boolean shouldProcessData() {
        if (System.currentTimeMillis() - lastProcessedTime < config.getTimerFrequency() * 1000L) {
            LOGGER.debug(String.format("Current time: %d, last processed time: %d; No need to process data.",
                    System.currentTimeMillis(),
                    lastProcessedTime));
            return false;
        } else if (isProcessing) {
            LOGGER.debug("Another thread is processing the data, current thread should skip");
            return false;
        } else {
            return true;
        }
    }

    public synchronized void processAndSendData() {
        isProcessing = true;

        LOGGER.info("Processing and sending data");
        if (sensorDataProcessorList.stream().allMatch(SensorDataProcessor::isIriInstantiationNeeded)) {
            LOGGER.info("Need to init kg");
            bulkInitKg();
        }

        if (sensorDataProcessorList.stream().anyMatch(SensorDataProcessor::isRbdInstantiationNeeded)) {
            LOGGER.info("Need to init rdb");
            bulkInitRdb();
        }

        try {
            bulkAddTimeSeriesData();
        } catch (RuntimeException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error(e.getCause());

            lastProcessedTime = System.currentTimeMillis();
            isProcessing = false;
            return;
        }

        lastProcessedTime = System.currentTimeMillis();
        LOGGER.info("Done with sending data, and update the processed time to " + lastProcessedTime);

        isProcessing = false;
    }

    public synchronized boolean shouldTerminateTask() {
        if (System.currentTimeMillis() - lastActiveTime > config.getTaskInactiveTime() * 1000L) {
            return true;
        }

        return false;
    }

    private void initSensorProcessors() {
        accelerometerProcessor = new AccelerometerProcessor(this.config, this.storeClient, smartphoneIRI);
        dbfsDataProcessor = new DBFSDataProcessor(this.config, this.storeClient, smartphoneIRI);
        gravityDataProcessor = new GravityDataProcessor(this.config, this.storeClient, smartphoneIRI);
        illuminationProcessor = new IlluminationProcessor(this.config, this.storeClient, smartphoneIRI);
        locationDataProcessor = new LocationDataProcessor(this.config, this.storeClient, smartphoneIRI);
        magnetometerDataProcessor = new MagnetometerDataProcessor(this.config, this.storeClient, smartphoneIRI);
        relativeBrightnessProcessor = new RelativeBrightnessProcessor(this.config, this.storeClient, smartphoneIRI);

        sensorDataProcessorList = Arrays.asList(accelerometerProcessor,
                dbfsDataProcessor,
                gravityDataProcessor,
                illuminationProcessor,
                locationDataProcessor,
                magnetometerDataProcessor,
                relativeBrightnessProcessor);
        LOGGER.info("sensor data iri created: " + sensorDataProcessorList.stream()
                .map(SensorDataProcessor::getDataIRIMap)
                .flatMap(map -> map.entrySet().stream())
                .map(Entry::getValue)
                .collect(Collectors.joining("; ")));
    }

    private void bulkInitKg() {
        Map<String, String> iriMap = sensorDataProcessorList.stream()
                .map(SensorDataProcessor::getDataIRIMap)
                .flatMap(map -> map.entrySet().stream())
                .collect(Collectors.toMap(Entry::getKey, Entry::getValue));
        iriMap.put("deviceID", deviceId);
        StaticInstantiation.instantiationMethod(iriMap);

        sensorDataProcessorList.forEach(p -> p.setIriInstantiationNeeded(false));
        LOGGER.info("finish instantiating kg");
    }

    private void bulkInitRdb() {
        Iterator<SensorDataProcessor> iterator = sensorDataProcessorList.stream().filter(SensorDataProcessor::isRbdInstantiationNeeded).iterator();
        List<List<String>> dataIris = new ArrayList<>();
        List<List<Class<?>>> dataClasses = new ArrayList<>();
        while (iterator.hasNext()) {
            SensorDataProcessor p = iterator.next();
            dataIris.add(new ArrayList<>(p.getDataIRIMap().values()));
            dataClasses.add(p.getDataClass());
        }

        LOGGER.info("bulk init iris in rdb");
        List<String> timeUnits = Collections.nCopies(dataIris.size(), OffsetDateTime.class.getSimpleName());
        tsClient.bulkInitTimeSeries(dataIris, dataClasses, timeUnits);

        sensorDataProcessorList.stream().filter(SensorDataProcessor::isRbdInstantiationNeeded).forEach(p -> p.setRbdInstantiationNeeded(false));
        LOGGER.info("finish init postgres dbTable and the corresponding table");
    }

    private void bulkAddTimeSeriesData() throws RuntimeException {
        List<TimeSeries<OffsetDateTime>> tsList = sensorDataProcessorList.stream()
                .filter(p -> p.getTimeSeriesLength() > 0)
                .map(p -> {
                    try {
                        return p.getProcessedTimeSeries();
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }).collect(Collectors.toList());
        if (tsList.isEmpty() || tsList.stream().mapToInt(ts -> ts.getTimes().size()).sum() == 0) {
            LOGGER.info("No new data, skip bulk add time series data");
            return;
        }

        LOGGER.info(String.format("bulk adding %d time series", tsList.size()));
        tsClient.bulkaddTimeSeriesData(tsList);
        LOGGER.info("finish adding data to postgres tables");
    }
}
