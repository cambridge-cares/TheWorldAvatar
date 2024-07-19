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

public class RelativeBrightnessProcessor extends SensorDataProcessor {
    private String relativeBrightnessIRI = "";

    private final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    private final List<Double> brightnessList = new ArrayList<>();

    public RelativeBrightnessProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("brightness_tsList"));
        brightnessList.addAll((List<Double>) data.get("brightnessList"));
    }

    @Override
    public TimeSeries getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Collections.singletonList(relativeBrightnessIRI);
        List<List<?>> valueList = Collections.singletonList(brightnessList);
        TimeSeries ts = new TimeSeries(timeList, dataIRIList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getRbDSResolution(), config.getRbDSType());

        return ts;
    }

    @Override
    public void initIRIs() {
        getIrisFromKg();

        if (relativeBrightnessIRI == null || relativeBrightnessIRI.isEmpty()) {
            relativeBrightnessIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/relativeBrightness_" + UUID.randomUUID();

            isIriInstantiationNeeded = true;
            isRbdInstantiationNeeded = true;
        }
    }

    @Override
    public List<Class> getDataClass() {
        return Collections.nCopies(getDataIRIMap().size(), Double.class);
    }

    @Override
    public Map<String, String> getDataIRIMap() {
        Map<String, String> iriHashMap = new HashMap<>();
        iriHashMap.put("relativeBrightness", relativeBrightnessIRI);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        brightnessList.clear();
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
                .addWhere(smartphoneIRINode, "ontodevice:hasScreenBrightness", "?relativeBrightness")
                .addWhere("?relativeBrightness", "rdf:type", "ontodevice:RelativeBrightness")
                .addWhere("?relativeBrightness", "rdf:type", "?om_ratio")
                .addWhere("?om_ratio", "rdf:type", "om:Ratio")
                .addWhere("?om_ratio", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        if (queryResult.isEmpty()) {
            return;
        }
        relativeBrightnessIRI = queryResult.getJSONObject(0).optString("o");
    }
}
