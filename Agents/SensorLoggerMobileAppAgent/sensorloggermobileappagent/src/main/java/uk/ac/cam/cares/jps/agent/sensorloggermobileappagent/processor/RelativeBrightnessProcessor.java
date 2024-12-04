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

public class RelativeBrightnessProcessor extends SensorDataDownsampledProcessor {

    private SensorData<Double> brightness;

    public RelativeBrightnessProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode, config.getRbDSResolution(), config.getRbDSType());
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getBrightnessTs());
        brightness.addData(data.getBrightness());
    }

    @Override
    void initSensorData() {
        brightness = new SensorData<>(Double.class);
        sensorData = List.of(brightness);
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
                .addWhere(smartphoneIRINode, "ontodevice:hasScreenBrightness", "?relativeBrightness")
                .addWhere("?relativeBrightness", "rdf:type", "ontodevice:RelativeBrightness")
                .addWhere("?relativeBrightness", "om:hasValue", varO);

        SelectBuilder sb = new SelectBuilder()
                .addVar(varO).addWhere(wb);

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
        brightness.setIri(queryResult.getJSONObject(0).optString("o"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "RelativeBrightness";
    }
}
