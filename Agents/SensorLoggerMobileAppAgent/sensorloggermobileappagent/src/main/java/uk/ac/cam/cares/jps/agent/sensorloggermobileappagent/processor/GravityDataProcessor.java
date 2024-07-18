package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.DownSampleConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class GravityDataProcessor extends SensorDataProcessor {

    private String xIri;
    private String yIri;
    private String zIri;

    private final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    private final List<Double> xList = new ArrayList<>();
    private final List<Double> yList = new ArrayList<>();
    private final List<Double> zList = new ArrayList<>();

    public GravityDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("gravity_tsList"));
        xList.addAll((List<Double>) data.get("gravityList_x"));
        yList.addAll((List<Double>) data.get("gravityList_y"));
        zList.addAll((List<Double>) data.get("gravityList_z"));
    }

    @Override
    public TimeSeries getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Arrays.asList(xIri, yIri, zIri);
        List<List<Double>> dataValues = Arrays.asList(xList, yList, zList);

        TimeSeries ts = new TimeSeries(timeList, dataIRIList, dataValues);
        ts = Downsampling.downsampleTS(ts, config.getGravityDSResolution(), config.getGravityDSType());

        clearData();
        return ts;
    }

    @Override
    public void initIRIs() {
        getIrisFromKg();

        if (xIri.isEmpty() && yIri.isEmpty() && zIri.isEmpty()) {
            xIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_x_" + UUID.randomUUID();
            yIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_y_" + UUID.randomUUID();
            zIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_gravity_z_" + UUID.randomUUID();

            isIriInstantiationNeeded = true;
        }
    }

    @Override
    public List<Class> getDataClass() {
        return Collections.nCopies(getDataIRIMap().size(), Double.class);
    }

    @Override
    public Map<String, String> getDataIRIMap() {
        Map<String, String> iriHashMap = new HashMap<>();
        iriHashMap.put("gravity_x", xIri);
        iriHashMap.put("gravity_y", yIri);
        iriHashMap.put("gravity_z", zIri);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        xList.clear();
        yList.clear();
        zList.clear();
    }

    @Override
    void getIrisFromKg() {
        Var x = Var.alloc("x");
        Var y = Var.alloc("y");
        Var z = Var.alloc("z");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoslma", ONTOSLMA)
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?gravitySensor")
                .addWhere("?gravitySensor", "rdf:type", "ontodevice:GravitySensor")
                .addWhere("?gravitySensor", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontoslma:GravityVector")
                .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", x)
                .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", y)
                .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", z);
        SelectBuilder sb = new SelectBuilder()
                .addVar(x).addVar(y).addVar(z).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        if (queryResult.isEmpty()) {
            return;
        }
        xIri = queryResult.getJSONObject(0).optString("x");
        yIri = queryResult.getJSONObject(0).optString("y");
        zIri = queryResult.getJSONObject(0).optString("z");
    }

//    String getXIri() {
//        WhereBuilder wb = new WhereBuilder()
//                .addPrefix("ontoslma", ONTOSLMA)
//                .addPrefix("slma", SLA)
//                .addPrefix("saref", SAREF)
//                .addPrefix("ontodevice", ONTODEVICE)
//                .addPrefix("rdf", RDF)
//                .addPrefix("om", OM)
//                .addWhere(smartphoneIRINode, "saref:consistsOf", "?gravitySensor")
//                .addWhere("?gravitySensor", "rdf:type", "ontodevice:GravitySensor")
//                .addWhere("?gravitySensor", "ontodevice:measures", "?vector")
//                .addWhere("?vector", "rdf:type", "ontoslma:GravityVector")
//                .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
//                .addWhere("?quantity", "om:hasValue", VAR_O);
//
//        SelectBuilder sb = new SelectBuilder()
//                .addVar(VAR_O).addWhere(wb);
//
//        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
//        return getIRIfromJSONArray(queryResult);
//    }
//
//    String getYIri() {
//        WhereBuilder wb = new WhereBuilder()
//                .addPrefix("ontoslma", ONTOSLMA)
//                .addPrefix("slma", SLA)
//                .addPrefix("saref", SAREF)
//                .addPrefix("ontodevice", ONTODEVICE)
//                .addPrefix("rdf", RDF)
//                .addPrefix("om", OM)
//                .addWhere(smartphoneIRINode, "saref:consistsOf", "?gravitySensor")
//                .addWhere("?gravitySensor", "rdf:type", "ontodevice:GravitySensor")
//                .addWhere("?gravitySensor", "ontodevice:measures", "?vector")
//                .addWhere("?vector", "rdf:type", "ontoslma:GravityVector")
//                .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
//                .addWhere("?quantity", "om:hasValue", VAR_O);
//        SelectBuilder sb = new SelectBuilder()
//                .addVar(VAR_O).addWhere(wb);
//
//        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
//        return getIRIfromJSONArray(queryResult);
//    }
//
//    String getZIri() {
//
//        WhereBuilder wb = new WhereBuilder()
//                .addPrefix("ontoslma", ONTOSLMA)
//                .addPrefix("slma", SLA)
//                .addPrefix("saref", SAREF)
//                .addPrefix("ontodevice", ONTODEVICE)
//                .addPrefix("rdf", RDF)
//                .addPrefix("om", OM)
//                .addWhere(smartphoneIRINode, "saref:consistsOf", "?gravitySensor")
//                .addWhere("?gravitySensor", "rdf:type", "ontodevice:GravitySensor")
//                .addWhere("?gravitySensor", "ontodevice:measures", "?vector")
//                .addWhere("?vector", "rdf:type", "ontoslma:GravityVector")
//                .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
//                .addWhere("?quantity", "om:hasValue", VAR_O);
//
//        SelectBuilder sb = new SelectBuilder()
//                .addVar(VAR_O).addWhere(wb);
//
//        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
//        return getIRIfromJSONArray(queryResult);
//    }
}
