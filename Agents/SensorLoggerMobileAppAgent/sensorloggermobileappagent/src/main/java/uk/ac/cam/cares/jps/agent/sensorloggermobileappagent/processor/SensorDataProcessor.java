package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.graph.Node;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.DownSampleConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class SensorDataProcessor {
    DownSampleConfig config;
    RemoteStoreClient storeClient;
    Node smartphoneIRINode;
    final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    boolean isIriInstantiationNeeded = false;
    boolean isRbdInstantiationNeeded = false;

    public SensorDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        this.config = config;
        this.storeClient = storeClient;
        this.smartphoneIRINode = smartphoneIRINode;
        initIRIs();
    }

    public abstract void addData(HashMap data);

    public abstract TimeSeries<OffsetDateTime> getProcessedTimeSeries() throws Exception;

    public abstract void initIRIs();

    public abstract List<Class<?>> getDataClass();

    public abstract Map<String, String> getDataIRIMap();

    abstract void clearData();
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
}
