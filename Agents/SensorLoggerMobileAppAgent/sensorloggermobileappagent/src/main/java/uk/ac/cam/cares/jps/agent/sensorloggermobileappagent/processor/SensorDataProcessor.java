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
    boolean isIriInstantiationNeeded = false;
    boolean isRbdInstantiationNeeded = false;
    List<SensorData<?>> sensorData;

    public SensorDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
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

        if (iris.stream().allMatch(Objects::isNull)) {
            isIriInstantiationNeeded = true;
            isRbdInstantiationNeeded = true;
        }
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

    public boolean isIriInstantiationNeeded() {
        return isIriInstantiationNeeded;
    }

    public void setIriInstantiationNeeded(boolean iriInstantiationNeeded) {
        isIriInstantiationNeeded = iriInstantiationNeeded;
    }

    public boolean isRbdInstantiationNeeded() {
        return isRbdInstantiationNeeded;
    }

    public void setRbdInstantiationNeeded(boolean rbdInstantiationNeeded) {
        isRbdInstantiationNeeded = rbdInstantiationNeeded;
    }

    public int getTimeSeriesLength() {
        return timeList.size();
    }

    public abstract String getOntodeviceLabel();
}
