package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.json.JSONArray;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.DownSampleConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class MagnetometerDataProcessor extends SensorDataProcessor {
    private String xIri;
    private String yIri;
    private String zIri;

    private final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    private final List<Double> xList = new ArrayList<>();
    private final List<Double> yList = new ArrayList<>();
    private final List<Double> zList = new ArrayList<>();

    public MagnetometerDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("magnetometer_tsList"));
        xList.addAll((List<Double>) data.get("magnetometerList_x"));
        yList.addAll((List<Double>) data.get("magnetometerList_y"));
        zList.addAll((List<Double>) data.get("magnetometerList_z"));
    }

    @Override
    public TimeSeries getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Arrays.asList(xIri, yIri, zIri);
        List<List<?>> valueList = Arrays.asList(xList, yList, zList);
        TimeSeries ts = new TimeSeries(timeList, dataIRIList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getMagnetometerDSResolution(), config.getMagnetometerDSType());

        clearData();
        return ts;
    }

    @Override
    public void initIRIs() {
        xIri = getMagnetometerIriX();
        yIri = getMagnetometerIriY();
        zIri = getMagnetometerIriZ();

        if (xIri.isEmpty() && yIri.isEmpty() && zIri.isEmpty()) {
            xIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_x_" + UUID.randomUUID();
            yIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_y_" + UUID.randomUUID();
            zIri = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_magnetometer_z_" + UUID.randomUUID();

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
        iriHashMap.put("magnetometer_x", xIri);
        iriHashMap.put("magnetometer_y", yIri);
        iriHashMap.put("magnetometer_z", zIri);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        xList.clear();
        yList.clear();
        zList.clear();
    }

    String getMagnetometerIriX() {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoslma", ONTOSLMA)
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?magnetometer")
                .addWhere("?magnetometer", "rdf:type", "ontodevice:Magnetometer")
                .addWhere("?magnetometer", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontoslma:MagneticFluxDensityVector")
                .addWhere("?vector", "ontoslma:hasXComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", VAR_O);
        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return getIRIfromJSONArray(queryResult);
    }

    String getMagnetometerIriY() {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoslma", ONTOSLMA)
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?magnetometer")
                .addWhere("?magnetometer", "rdf:type", "ontodevice:Magnetometer")
                .addWhere("?magnetometer", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontoslma:MagneticFluxDensityVector")
                .addWhere("?vector", "ontoslma:hasYComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return getIRIfromJSONArray(queryResult);
    }

    String getMagnetometerIriZ() {

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontoslma", ONTOSLMA)
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?magnetometer")
                .addWhere("?magnetometer", "rdf:type", "ontodevice:Magnetometer")
                .addWhere("?magnetometer", "ontodevice:measures", "?vector")
                .addWhere("?vector", "rdf:type", "ontoslma:MagneticFluxDensityVector")
                .addWhere("?vector", "ontoslma:hasZComponent", "?quantity")
                .addWhere("?quantity", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return getIRIfromJSONArray(queryResult);
    }
}
