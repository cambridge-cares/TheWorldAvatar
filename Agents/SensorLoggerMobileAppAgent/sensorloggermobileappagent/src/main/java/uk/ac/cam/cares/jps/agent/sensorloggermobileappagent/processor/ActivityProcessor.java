package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class ActivityProcessor extends SensorDataProcessor {

    private SensorData<Integer> confidence;
    private SensorData<String> activityType;

    public ActivityProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super("Activity", config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getActivityTs());
        confidence.addData(data.getConfidences());
        activityType.addData(data.getActivityTypes());
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<String> iriList = getDataIRIs();
        List<List<?>> valueList = getValues().stream()
                .map(ArrayList::new)
                .collect(Collectors.toList());
                
        List<Long> epochlist = timeList.stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        clearData();
        return new TimeSeries<>(epochlist, iriList, valueList);
    }

    @Override
    void initSensorData() {
        confidence = new SensorData<>(Integer.class);
        activityType = new SensorData<>(String.class);
        sensorData = List.of(confidence, activityType);
    }

    @Override
    void getIrisFromKg() {
        Var confidence = Var.alloc("confidence");
        Var activityType = Var.alloc("activityType");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "ontodevice:detectedByDevice", "?activity")
                .addWhere("?activity", "rdf:type", "ontodevice:Activity")
                .addWhere("?activity", "ontodevice:hasConfidenceLevel", confidence)
                .addWhere("?activity", "ontodevice:hasActivityType", activityType);

        SelectBuilder sb = new SelectBuilder()
                .addVar(confidence).addVar(activityType).addWhere(wb);

        JSONArray queryResult;
        try {
            queryResult = storeClient.executeQuery(sb.buildString());
        } catch (Exception e) {
            // ontop does not accept queries before any mapping is added
            return;
        }
        if (queryResult.isEmpty()) {
            return;
        }
        this.confidence.setIri(queryResult.getJSONObject(0).optString("confidence"));
        this.activityType.setIri(queryResult.getJSONObject(0).optString("activityType"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "Activity";
    }
}
