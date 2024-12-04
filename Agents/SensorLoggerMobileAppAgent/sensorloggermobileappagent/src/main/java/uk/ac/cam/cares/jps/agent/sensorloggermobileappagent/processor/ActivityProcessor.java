package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.Payload;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;
import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.OM;

public class ActivityProcessor extends SensorDataProcessor {

    private String confidenceIRI = null;
    private String activityTypeIRI = null;

    private List<Integer> confidenceList = new ArrayList<>();
    private List<String> activityTypeList = new ArrayList<>();

    public ActivityProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getActivityTs());
        confidenceList.addAll(data.getConfidences());
        activityTypeList.addAll(data.getActivityTypes());
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<String> iriList = Arrays.asList(confidenceIRI, activityTypeIRI);
        List<List<?>> valueList = Arrays.asList(confidenceList, activityTypeList);
        List<Long> epochlist = timeList.stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        clearData();
        return new TimeSeries<>(epochlist, iriList, valueList);
    }

    @Override
    public List<Class<?>> getDataClass() {
        return List.of(Integer.class, String.class);
    }

    @Override
    public List<String> getDataIRIs() {
        return List.of(confidenceIRI, activityTypeIRI);
    }

    @Override
    void clearData() {
        timeList.clear();
        confidenceList.clear();
        activityTypeList.clear();
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
        confidenceIRI = queryResult.getJSONObject(0).optString("confidence");
        activityTypeIRI = queryResult.getJSONObject(0).optString("activityType");
    }

    @Override
    public String getOntodeviceLabel() {
        return "Activity";
    }
}
