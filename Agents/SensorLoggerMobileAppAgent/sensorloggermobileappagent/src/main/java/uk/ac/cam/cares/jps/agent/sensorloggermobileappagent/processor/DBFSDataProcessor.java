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

import java.util.List;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class DBFSDataProcessor extends SensorDataDownsampledProcessor {

    private SensorData<Double> dBFS;

    public DBFSDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneNode) {
        super(config, storeClient, smartphoneNode, config.getDbfsDSResolution(), config.getDbfsDSType());
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getdBFSTs());
        dBFS.addData(data.getdBFSs());
    }

    @Override
    void initSensorData() {
        dBFS = new SensorData<>(Double.class);
        sensorData = List.of(dBFS);
    }

    @Override
    void getIrisFromKg() {
        Var varO = Var.alloc("o");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?microphone")
                .addWhere("?microphone", "rdf:type", "ontodevice:Microphone")
                .addWhere("?microphone", "ontodevice:measures", "?om_soundPressureLevel")
                .addWhere("?om_soundPressureLevel", "om:hasValue", varO);

        SelectBuilder sb = new SelectBuilder().addVar(varO).addWhere(wb);

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
        dBFS.setIri(queryResult.getJSONObject(0).optString("o"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "Microphone";
    }
}
