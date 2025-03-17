package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.io.ClassPathResource;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor.*;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

import java.awt.*;
import java.nio.file.Path;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

import java.io.IOException;
import org.json.JSONArray;

public class SmartphoneRecordingTask {
    Logger logger;

    AccelerometerProcessor accelerometerProcessor;
    DBFSDataProcessor dbfsDataProcessor;
    GravityDataProcessor gravityDataProcessor;
    IlluminationProcessor illuminationProcessor;
    LocationDataProcessor locationDataProcessor;
    MagnetometerDataProcessor magnetometerDataProcessor;
    RelativeBrightnessProcessor relativeBrightnessProcessor;
    ActivityProcessor activityProcessor;
    List<SensorDataProcessor> sensorDataProcessors;

    private final Node smartphoneIRI;
    private final String deviceId;

    private final RemoteStoreClient ontopRemoteStoreClient;
    private final RemoteStoreClient blazegraphStoreClient;
    private final TimeSeriesClient<Long> tsClient;
    private final AgentConfig config;

    private long lastProcessedTime;
    private long lastActiveTime;
    private boolean isProcessing;

    private SensorLoggerPostgresClient sensorLoggerPostgresClient;

    public SmartphoneRecordingTask(RemoteStoreClient storeClient, RemoteRDBStoreClient rdbStoreClient,
            AgentConfig config, String deviceId, RemoteStoreClient ontopRemoteStoreClient) {
        logger = LogManager.getLogger("SmartphoneRecordingTask_" + deviceId);

        this.ontopRemoteStoreClient = ontopRemoteStoreClient;
        TimeSeriesRDBClientWithReducedTables<Long> tsRdbClient = new TimeSeriesRDBClientWithReducedTables<>(Long.class);
        tsRdbClient.setRdbURL(rdbStoreClient.getRdbURL());
        tsRdbClient.setRdbUser(rdbStoreClient.getUser());
        tsRdbClient.setRdbPassword(rdbStoreClient.getPassword());

        this.blazegraphStoreClient = storeClient;
        this.tsClient = new TimeSeriesClient<>(blazegraphStoreClient, tsRdbClient);
        this.config = config;
        lastActiveTime = System.currentTimeMillis();
        lastProcessedTime = System.currentTimeMillis();

        String smartphoneString = "https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_" + deviceId;
        smartphoneIRI = NodeFactory.createURI(smartphoneString);
        this.deviceId = deviceId;

        initSensorProcessors();

        sensorLoggerPostgresClient = new SensorLoggerPostgresClient(rdbStoreClient.getRdbURL(),
                rdbStoreClient.getUser(), rdbStoreClient.getPassword());
    }

    public synchronized void addData(Payload data) {
        logger.info("adding data...");
        sensorDataProcessors.forEach(p -> p.addData(data));
        lastActiveTime = System.currentTimeMillis();
        logger.info("finish adding data and the last active time is updated to " + lastActiveTime);
    }

    public synchronized boolean shouldProcessData() {
        if (System.currentTimeMillis() - lastProcessedTime < config.getTimerFrequency() * 1000L) {
            logger.debug(String.format("Current time: %d, last processed time: %d; No need to process data.",
                    System.currentTimeMillis(),
                    lastProcessedTime));
            return false;
        } else if (isProcessing) {
            logger.debug("Another thread is processing the data, current thread should skip");
            return false;
        } else {
            return true;
        }
    }

    public synchronized void processAndSendData() {
        isProcessing = true;

        logger.info("Processing and sending data");

        List<SensorDataProcessor> processorsToInstantiate = sensorDataProcessors.stream()
                .filter(SensorDataProcessor::isNeedToInstantiateDevice).toList();
        if (!processorsToInstantiate.isEmpty()) {
            logger.info("Need to init device: " + processorsToInstantiate.stream()
                    .map(SensorDataProcessor::getOntodeviceLabel).collect(Collectors.joining(",")));
            initDeviceKgUsingOntop(processorsToInstantiate);
        }

        List<SensorDataProcessor> processorsToInitTimeSeries = sensorDataProcessors.stream()
                .filter(SensorDataProcessor::isNeedToInitTimeSeries).toList();
        if (!processorsToInitTimeSeries.isEmpty()) {
            logger.info("Need to init rdb for " + processorsToInitTimeSeries.stream()
                    .map(SensorDataProcessor::getOntodeviceLabel).collect(Collectors.joining(",")));
            initDevicesInRDB();
        }

        fixUninitializedSensorData();

        try {
            bulkAddTimeSeriesData();
        } catch (RuntimeException e) {
            lastProcessedTime = System.currentTimeMillis();
            isProcessing = false;

            logger.error(e.getMessage());
            logger.error(e.getCause());
            logger.error("Failed to process, release isProcessing and update the processed time to " + lastProcessedTime);
            return;
        }

        lastProcessedTime = System.currentTimeMillis();
        logger.info("Done with sending data, and update the processed time to " + lastProcessedTime);

        isProcessing = false;
    }

    public synchronized boolean shouldTerminateTask() {
        return System.currentTimeMillis() - lastActiveTime > config.getTaskInactiveTime() * 1000L;
    }

    private void initSensorProcessors() {
        accelerometerProcessor = new AccelerometerProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        dbfsDataProcessor = new DBFSDataProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        gravityDataProcessor = new GravityDataProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        illuminationProcessor = new IlluminationProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        locationDataProcessor = new LocationDataProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        magnetometerDataProcessor = new MagnetometerDataProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        relativeBrightnessProcessor = new RelativeBrightnessProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);
        activityProcessor = new ActivityProcessor(this.config, ontopRemoteStoreClient, blazegraphStoreClient, smartphoneIRI);

        sensorDataProcessors = Arrays.asList(accelerometerProcessor,
                dbfsDataProcessor,
                gravityDataProcessor,
                illuminationProcessor,
                locationDataProcessor,
                magnetometerDataProcessor,
                relativeBrightnessProcessor,
                activityProcessor);
    }

    private void initDeviceKgUsingOntop(List<SensorDataProcessor> processors) {
        logger.info("Instantiating kg in ontop");

        List<String> sensorClasses = processors.stream().map(SensorDataProcessor::getOntodeviceLabel)
                .collect(Collectors.toList());

        boolean firstTime = sensorLoggerPostgresClient.populateTable(deviceId, sensorClasses);

        if (firstTime) {
            Path obdaFile = null;
            try {
                obdaFile = new ClassPathResource("ontop.obda").getFile().toPath();
            } catch (IOException e) {
                logger.error("Could not retrieve ontop.obda file.");
                throw new RuntimeException(e);
            }
            OntopClient.getInstance().updateOBDA(obdaFile);
        }

        logger.info(
                "finish instantiating kg, exception above is probably from updating the obda file and is harmless.");
        // wait 30 seconds for ontop to initialise before allow queries execution
        try {
            logger.info("Waiting for 30 seconds to allow ontop to intialise");
            Thread.sleep(30000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        sensorDataProcessors.forEach(s -> s.initIRIs());
    }

    private void initDevicesInRDB() {
        Iterator<SensorDataProcessor> iterator = sensorDataProcessors.stream()
                .filter(SensorDataProcessor::isNeedToInitTimeSeries).iterator();
        List<List<String>> dataIris = new ArrayList<>();
        List<List<Class<?>>> dataClasses = new ArrayList<>();
        while (iterator.hasNext()) {
            SensorDataProcessor p = iterator.next();
            dataIris.add(p.getDataIRIs());
            dataClasses.add(p.getDataClass());
        }

        logger.info("bulk init iris in rdb");
        List<String> timeUnits = Collections.nCopies(dataIris.size(), "millisecond");
        tsClient.bulkInitTimeSeries(dataIris, dataClasses, timeUnits, 4326);

        sensorDataProcessors.stream()
                .filter(SensorDataProcessor::isNeedToInitTimeSeries).forEach(SensorDataProcessor::getTimeSeriesIrisFromBlazegraph);
        logger.info("finish init postgres dbTable and the corresponding table");
    }

    /**
     * Updates blazegraph and time_series_quantities table for new sensor data when
     * device already exists.
     * This handles the case where the device and some of its sensor data exists,
     * but having new sensor data not yet linked with a time series in blazegraph
     * and RDB tables.
     * Only updates entries for sensor data that need to be initialized.
     * 
     * NOTICE: This function assumes all sensor data by a device has the same time
     * series.
     */
    private void fixUninitializedSensorData() {
        for (SensorDataProcessor sensorDataProcessor : sensorDataProcessors) {
            List<SensorData<?>> sensorData = sensorDataProcessor.getSensorData().stream().filter(sd -> sd.isNeedToInitTimeSeries()).collect(Collectors.toList());
            if (sensorData.isEmpty()) {
                continue;
            }

            List<String> dataIris = new ArrayList<>();
            List<Class<?>> classes = new ArrayList<>();
            for (SensorData sd : sensorData) {
                dataIris.add(sd.getDataIri());
                classes.add(sd.getType());
            }

            logger.debug(sensorDataProcessor.getSensorName() + ": " + String.join(",", dataIris) + " have no tsIRI, fixing is needed.");

            tsClient.addColumnsToExistingTimeSeries(sensorDataProcessor.getTsIri(), dataIris, classes,
                    classes.contains(Point.class)?4326:null);
            sensorData.forEach(sd -> sd.setNeedToInitTimeSeries(false));
        }
    }

    private void bulkAddTimeSeriesData() throws RuntimeException {
        List<TimeSeries<Long>> tsList = sensorDataProcessors.stream()
                .filter(p -> p.getTimeSeriesLength() > 0)
                .map(p -> {
                    try {
                        return p.getProcessedTimeSeries();
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }).collect(Collectors.toList());
        if (tsList.isEmpty() || tsList.stream().mapToInt(ts -> ts.getTimes().size()).sum() == 0) {
            logger.info("No new data, skip bulk add time series data");
            return;
        }

        // downsampling algorithm applied in getProcessedTimeSeries might make a time
        // series empty
        tsList.removeIf(longTimeSeries -> longTimeSeries.getTimes().isEmpty());

        logger.info(String.format("bulk adding %d time series", tsList.size()));
        tsClient.bulkaddTimeSeriesData(tsList);
        logger.info("finish adding data to postgres tables");
    }

    String getSmartphoneIRI() {
        String smartphone = null;
        String query = """
                SELECT ?device
                WHERE {
                  ?device <https://www.theworldavatar.com/kg/ontodevice/hasDeviceID> "%s"
                }
                """.formatted(deviceId);
        JSONArray queryResult = ontopRemoteStoreClient.executeQuery(query);

        if (queryResult.length() == 1) {
            smartphone = queryResult.getJSONObject(0).getString("device");
        } else if (queryResult.length() > 1) {
            throw new RuntimeException("More than one smartphone associated with " + deviceId);
        }

        return smartphone;
    }

    public void instantiate() {
        initDeviceKgUsingOntop(sensorDataProcessors);
        initDevicesInRDB();
    }
}
