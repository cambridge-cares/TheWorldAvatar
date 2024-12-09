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

public class AccelerometerProcessor extends SensorDataProcessor {
    private String xIri = null;
    private String yIri = null;
    private String zIri = null;

    private List<Double> xList = new ArrayList<>();
    private List<Double> yList = new ArrayList<>();
    private List<Double> zList = new ArrayList<>();

    public AccelerometerProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneNode) {
        super(config, storeClient, smartphoneNode);
        initIRIs();
    }

    @Override
    public void addData(HashMap<String, List<?>> data) {
        timeList.addAll((List<OffsetDateTime>) data.get("accel_tsList"));
        xList.addAll((List<Double>) data.get("accelList_x"));
        yList.addAll((List<Double>) data.get("accelList_y"));
        zList.addAll((List<Double>) data.get("accelList_z"));
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<List<?>> valueList = Arrays.asList(xList, yList, zList);
        List<String> iriList = Arrays.asList(xIri, yIri, zIri);

        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(timeList, iriList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getAccelDSResolution(), config.getAccelDSType());

        List<Long> epochlist = ts.getTimes().stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        List<List<?>> downsampledValuesList = Arrays.asList(ts.getValuesAsDouble(xIri), ts.getValuesAsDouble(yIri),
                ts.getValuesAsDouble(zIri));

        clearData();
        return new TimeSeries<>(epochlist, iriList, downsampledValuesList);
    }

    @Override
    public void initIRIs() {
        if (xIri != null && yIri != null && zIri != null) {
            // already instantiated in previous call
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
        return "Accelerometer";
    }
}
