package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class GravityDataProcessor extends SensorDataProcessor {

    private String xIri = null;
    private String yIri = null;
    private String zIri = null;

    private final List<Double> xList = new ArrayList<>();
    private final List<Double> yList = new ArrayList<>();
    private final List<Double> zList = new ArrayList<>();

    public GravityDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap<String, List<?>> data) {
        timeList.addAll((List<OffsetDateTime>) data.get("gravity_tsList"));
        xList.addAll((List<Double>) data.get("gravityList_x"));
        yList.addAll((List<Double>) data.get("gravityList_y"));
        zList.addAll((List<Double>) data.get("gravityList_z"));
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Arrays.asList(xIri, yIri, zIri);
        List<List<?>> dataValues = Arrays.asList(xList, yList, zList);

        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(timeList, dataIRIList, dataValues);
        ts = Downsampling.downsampleTS(ts, config.getGravityDSResolution(), config.getGravityDSType());

        List<Long> epochlist = ts.getTimes().stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        List<List<?>> downsampledValuesList = Arrays.asList(ts.getValuesAsDouble(xIri), ts.getValuesAsDouble(yIri),
                ts.getValuesAsDouble(zIri));

        clearData();
        return new TimeSeries<>(epochlist, dataIRIList, downsampledValuesList);
    }

    @Override
    public void initIRIs() {
        if (xIri != null && yIri != null && zIri != null) {
            // instantiated in previous call
            return;
        }

        getIrisFromKg();

        if (xIri == null && yIri == null && zIri == null) {
            isIriInstantiationNeeded = true;
            isRbdInstantiationNeeded = true;
        }
    }

    @Override
    public List<Class<?>> getDataClass() {
        return Collections.nCopies(getDataIRIs().size(), Double.class);
    }

    @Override
    public List<String> getDataIRIs() {
        return List.of(xIri, yIri, zIri);
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
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?gravitySensor")
                .addWhere("?gravitySensor", "rdf:type", "ontodevice:GravitySensor")
                .addWhere("?gravitySensor", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontodevice:GravityVector")
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
            queryResult = storeClient.executeQuery(sb.buildString());
        } catch (Exception e) {
            // ontop does not accept queries before any mapping is added
            return;
        }
        if (queryResult.isEmpty()) {
            return;
        }
        xIri = queryResult.getJSONObject(0).optString("x");
        yIri = queryResult.getJSONObject(0).optString("y");
        zIri = queryResult.getJSONObject(0).optString("z");
    }

    @Override
    public String getOntodeviceLabel() {
        return "GravitySensor";
    }
}
