package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;

import net.sf.jsqlparser.statement.select.Offset;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class AccelerometerProcessor extends SensorDataProcessor {

    private String xIri;
    private String yIri;
    private String zIri;

    private final List<Double> xList = new ArrayList<>();
    private final List<Double> yList = new ArrayList<>();
    private final List<Double> zList = new ArrayList<>();

    public AccelerometerProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneNode) {
        super(config, storeClient, smartphoneNode);
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

        List<Long> epochlist = ts.getTimes().stream().map(t -> t.toInstant().getEpochSecond())
                .collect(Collectors.toList());

        List<List<?>> downsampledValuesList = Arrays.asList(ts.getValuesAsDouble(xIri), ts.getValuesAsDouble(yIri),
                ts.getValuesAsDouble(zIri));

        clearData();
        return new TimeSeries<>(epochlist, iriList, downsampledValuesList);
    }

    @Override
    public void initIRIs() {
        getIrisFromKg();

        if ((xIri == null || xIri.isEmpty())
                && (yIri == null || yIri.isEmpty())
                && (zIri == null || zIri.isEmpty())) {
            xIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_x_" + UUID.randomUUID();
            yIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_y_" + UUID.randomUUID();
            zIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_accel_z_" + UUID.randomUUID();

            isIriInstantiationNeeded = true;
            isRbdInstantiationNeeded = true;
        }
    }

    @Override
    public List<Class<?>> getDataClass() {
        return Collections.nCopies(getDataIRIMap().size(), Double.class);
    }

    @Override
    public Map<String, String> getDataIRIMap() {
        Map<String, String> iriHashMap = new HashMap<>();
        iriHashMap.put("accel_x", xIri);
        iriHashMap.put("accel_y", yIri);
        iriHashMap.put("accel_z", zIri);
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
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?accelerometer")
                .addWhere("?accelerometer", "rdf:type", "ontodevice:Accelerometer")
                .addWhere("?accelerometer", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontoslma:AccelerationVector")
                .addWhere("?vector", "ontoslma:hasXComponent", "?quantityX")
                .addWhere("?quantityX", "om:hasValue", x)
                .addWhere("?vector", "ontoslma:hasYComponent", "?quantityY")
                .addWhere("?quantityY", "om:hasValue", y)
                .addWhere("?vector", "ontoslma:hasZComponent", "?quantityZ")
                .addWhere("?quantityZ", "om:hasValue", z);

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

}
