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

public class IlluminationProcessor extends SensorDataDownsampledProcessor {

    private SensorData<Double> illumination;

    public IlluminationProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super("Camera", config, storeClient, smartphoneIRINode, config.getLightValueDSResolution(), config.getLightValueDSType());
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getLightValueTs());
        illumination.addData(data.getLightValues());
    }

    @Override
    void initSensorData() {
        illumination = new SensorData<>(Double.class);
        sensorData = List.of(illumination);
    }

    @Override
    void getIrisFromKg() {
        Var VAR_O = Var.alloc("o");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?camera")
                .addWhere("?camera", "rdf:type", "ontodevice:Camera")
                .addWhere("?camera", "ontodevice:measures", "?om_illuminance")
                .addWhere("?om_illuminance", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder().addVar(VAR_O).addWhere(wb);

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
        illumination.setIri(queryResult.getJSONObject(0).optString("o"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "Camera";
    }
}
