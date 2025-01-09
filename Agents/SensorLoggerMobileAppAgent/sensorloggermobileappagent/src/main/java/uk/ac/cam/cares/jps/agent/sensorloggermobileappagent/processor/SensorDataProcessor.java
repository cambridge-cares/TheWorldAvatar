package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.graph.Node;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

public abstract class SensorDataProcessor {
    AgentConfig config;
    RemoteStoreClient storeClient;
    Node smartphoneIRINode;
    final List<OffsetDateTime> timeList = new ArrayList<>();
    boolean needToInstantiateDevice = false;
    List<SensorData<?>> sensorData;
    String sensorName;

    public SensorDataProcessor(String sensorName, AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        this.sensorName = sensorName;
        this.config = config;
        this.storeClient = storeClient;
        this.smartphoneIRINode = smartphoneIRINode;

        initSensorData();
        initIRIs();
    }

    public abstract void addData(Payload data);

    public abstract TimeSeries<Long> getProcessedTimeSeries() throws Exception;
    abstract void initSensorData();

    public void initIRIs() {
        List<String> iris = getDataIRIs();
        if (iris.stream().allMatch(Objects::nonNull)) {
            return;
        }

        getIrisFromKg();

        // device not instantiated, so all sensor data IRIs not instantiated with obda mapping
        if (getDataIRIs().stream().allMatch(iri -> iri == null)) {
            needToInstantiateDevice = true;
        }

        // Have existing deviceIRI, but some sensor data are not linked with time series, which require check of the blazegraph.
        // This is handled in the SmartphoneRecordingTask.
    };

    public List<SensorData<?>> getSensorData() {
        return sensorData;
    };

    public List<Class<?>> getDataClass() {
        return getSensorData().stream().map(SensorData::getType).collect(Collectors.toList());
    };

    public List<String> getDataIRIs() {
        return getSensorData().stream().map(SensorData::getIri).toList();
    };

    public List<List<?>> getValues() {
        return getSensorData().stream().map(SensorData::getValues).collect(Collectors.toList());
    }

    void clearData() {
        timeList.clear();
        getSensorData().forEach(SensorData::clearData);
    };

    abstract void getIrisFromKg();

    public boolean isNeedToInstantiateDevice() {
        return needToInstantiateDevice;
    }

    public void setNeedToInstantiateDevice(boolean needToInstantiateDevice) {
        this.needToInstantiateDevice = needToInstantiateDevice;
    }

    public int getTimeSeriesLength() {
        return timeList.size();
    }

    public String getSensorName() {
        return sensorName;
    }

    public abstract String getOntodeviceLabel();
}
