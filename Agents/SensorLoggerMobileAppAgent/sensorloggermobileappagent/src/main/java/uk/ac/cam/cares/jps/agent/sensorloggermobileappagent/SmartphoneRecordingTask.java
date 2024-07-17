package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.log4j.Logger;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor.*;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.sql.Connection;
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

    private RemoteStoreClient storeClient;
    private RemoteRDBStoreClient rdbStoreClient;
    private DownSampleConfig config;

    private long lastProcessedTime;
    private long lastActiveTime;

    public SmartphoneRecordingTask(RemoteStoreClient storeClient, RemoteRDBStoreClient rdbStoreClient, DownSampleConfig config, String phoneId) {
        // todo: haven't decided whether the client is passed from outside or built inside
//        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
//        storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());

        LOGGER = Logger.getLogger("SmartphoneRecordingTask_" + phoneId);

        this.storeClient = storeClient;
        this.rdbStoreClient = rdbStoreClient;
        this.config = config;
        lastActiveTime = System.currentTimeMillis();
        lastProcessedTime = System.currentTimeMillis();

        initSensorProcessors(phoneId);

        bulkInitKg();
        bulkInitRdb();
    }

    public void addData(HashMap data) {
        sensorDataProcessorList.forEach(p -> p.addData(data));
        lastActiveTime = System.currentTimeMillis();
    }

    public void processData(boolean skipTimeCheck) {
        if (System.currentTimeMillis() - lastProcessedTime < config.getTimerFrequency() * 1000L && !skipTimeCheck) {
            LOGGER.debug("time from the last processed time is less than the set frequency, no need to update the rdb yet");
            return;
        }

        List<TimeSeries> tsList = sensorDataProcessorList.stream().map(sensorDataProcessor -> {
            try {
                return sensorDataProcessor.getProcessedTimeSeries();
            } catch (Exception e) {
                LOGGER.error("unable to downsample the data of " + sensorDataProcessor.getClass().getName());
                throw new RuntimeException(e);
            }
        }).collect(Collectors.toList());
        bulkAddTimeSeriesData(tsList);
        lastProcessedTime = System.currentTimeMillis();
    }

    public boolean checkTaskTermination() {
        if (System.currentTimeMillis() - lastActiveTime > 600 * 1000L) {
            processData(true);
            return true;
        }

        return false;
    }

    private void initSensorProcessors(String phoneId) {
        String smartphoneString = "https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_" + phoneId;
        Node smartphoneIRI = NodeFactory.createURI(smartphoneString);

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
    }

    private void bulkInitKg() {
        if (sensorDataProcessorList.stream().allMatch(SensorDataProcessor::isInstantiationNeeded)) {
            StaticInstantiation.instantiationMethod(sensorDataProcessorList.stream()
                    .map(SensorDataProcessor::getDataIRIMap)
                    .flatMap(map -> map.entrySet().stream())
                    .collect(Collectors.toMap(Entry::getKey, Entry::getValue)));
        }
    }

    private void bulkInitRdb() {
        Iterator<SensorDataProcessor> iterator = sensorDataProcessorList.stream().filter(SensorDataProcessor::isInstantiationNeeded).iterator();
        List<List<String>> dataIris = new ArrayList<>();
        List<List<Class>> dataClasses = new ArrayList<>();
        while (iterator.hasNext()) {
            SensorDataProcessor p = iterator.next();
            dataIris.add(new ArrayList<>(p.getDataIRIMap().values()));
            dataClasses.add(p.getDataClass());
        }

        List<String> timeUnits = Collections.nCopies(dataIris.size(), OffsetDateTime.class.getSimpleName());
        TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
        tsClient.bulkInitTimeSeries(dataIris, dataClasses, timeUnits);
    }

    // todo: make it async
    private void bulkAddTimeSeriesData(List<TimeSeries> tsList) {
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeriesClient tsClient = new TimeSeriesClient(storeClient, OffsetDateTime.class);
            tsClient.bulkaddTimeSeriesData(tsList, conn);
        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }
}
