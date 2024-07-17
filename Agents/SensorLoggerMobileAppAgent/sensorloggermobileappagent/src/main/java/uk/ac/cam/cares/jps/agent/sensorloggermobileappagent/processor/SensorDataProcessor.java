package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.graph.Node;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.DownSampleConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.str_o;

public abstract class SensorDataProcessor {
    DownSampleConfig config;
    RemoteStoreClient storeClient;
    Node smartphoneIRINode;
    boolean isIriInstantiationNeeded = false;

    public SensorDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        this.config = config;
        this.storeClient = storeClient;
        this.smartphoneIRINode = smartphoneIRINode;
        initIRIs();
    }

    String getIRIfromJSONArray(JSONArray jsonArray){
        if(jsonArray.length()!=1){System.out.println("There is more than 1 data in this JSONArray, only the first one is returned");}
        return jsonArray.getJSONObject(0).get(str_o).toString();
    }

    public abstract void addData(HashMap data);

    public abstract TimeSeries getProcessedTimeSeries() throws Exception;

    public abstract void initIRIs();

    public abstract List<Class> getDataClass();

    public abstract Map<String, String> getDataIRIMap();

    abstract void clearData();

    public boolean isInstantiationNeeded() {
        return isIriInstantiationNeeded;
    }
}
