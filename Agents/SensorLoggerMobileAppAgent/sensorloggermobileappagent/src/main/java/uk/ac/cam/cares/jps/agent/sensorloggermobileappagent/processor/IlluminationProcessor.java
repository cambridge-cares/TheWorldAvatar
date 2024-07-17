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

public class IlluminationProcessor extends SensorDataProcessor {
    private String illuminationIri;

    private final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    private final List<Double> illuminationList = new ArrayList<>();

    public IlluminationProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("lightValue_tsList"));
        illuminationList.addAll((List<Double>) data.get("lightValueList"));
    }

    @Override
    public TimeSeries getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Collections.singletonList(illuminationIri);
        List<List<?>> valueList = Collections.singletonList(illuminationList);

        TimeSeries ts = new TimeSeries(timeList, dataIRIList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getLightValueDSResolution(), config.getLightValueDSType());

        clearData();
        return ts;
    }

    @Override
    public void initIRIs() {
        illuminationIri = getIlluminanceIRIArray();
        if (illuminationIri.isEmpty()) {
            illuminationIri = "https://www.theworldavatar.com/kg/sensorloggerapp/light_value_" + UUID.randomUUID();

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
        iriHashMap.put("light_value", illuminationIri);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        illuminationList.clear();
    }

    String getIlluminanceIRIArray() {

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

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        return getIRIfromJSONArray(queryResult);
    }
}
