package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.apache.logging.log4j.LogManager;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class AccelerometerProcessor extends SensorDataDownsampledProcessor {

    public AccelerometerProcessor(AgentConfig config, RemoteStoreClient ontopClient, RemoteStoreClient blazegraphClient, Node smartphoneNode) {
        super("Accelerometer", config, ontopClient, blazegraphClient, smartphoneNode,
                config.getAccelDSResolution(),
                config.getAccelDSType()
                );
        logger = LogManager.getLogger(AccelerometerProcessor.class);
    }

    @Override
    void initSensorData() {
        x = new SensorData<>(Double.class);
        y = new SensorData<>(Double.class);
        z = new SensorData<>(Double.class);
        sensorData = List.of(x, y, z);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getAccelTs());
        x.addData(data.getAccelXs());
        y.addData(data.getAccelYs());
        z.addData(data.getAccelZs());
    }

    @Override
    void getDataIrisFromKg() {
        Var x = Var.alloc("x");
        Var y = Var.alloc("y");
        Var z = Var.alloc("z");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?accelerometer")
                .addWhere("?accelerometer", "rdf:type", "ontodevice:Accelerometer")
                .addWhere("?accelerometer", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontodevice:AccelerationVector")
                .addWhere("?vector", "ontodevice:hasXComponent", "?quantityX")
                .addWhere("?quantityX", "om:hasValue", x)
                .addWhere("?vector", "ontodevice:hasYComponent", "?quantityY")
                .addWhere("?quantityY", "om:hasValue", y)
                .addWhere("?vector", "ontodevice:hasZComponent", "?quantityZ")
                .addWhere("?quantityZ", "om:hasValue", z);

        SelectBuilder sb = new SelectBuilder()
                .addVar(x).addVar(y).addVar(z).addWhere(wb);

        JSONArray queryResult;
        try {
            queryResult = ontopClient.executeQuery(sb.buildString());
        } catch (Exception e) {
            // ontop does not accept queries before any mapping is added
            return;
        }
        if (queryResult.isEmpty()) {
            return;
        }

        this.x.setDataIri(queryResult.getJSONObject(0).optString("x"));
        this.y.setDataIri(queryResult.getJSONObject(0).optString("y"));
        this.z.setDataIri(queryResult.getJSONObject(0).optString("z"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "Accelerometer";
    }
}
