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

public class DBFSDataProcessor extends SensorDataProcessor {
    private String dbfsIRI;
    private final List<Double> dBFSList = new ArrayList<>();

    public DBFSDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneNode) {
        super(config, storeClient, smartphoneNode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("dBFS_tsList"));
        dBFSList.addAll((List<Double>) data.get("dBFSList"));
    }

    @Override
    public TimeSeries<OffsetDateTime> getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Collections.singletonList(dbfsIRI);
        List<List<?>> valueList = Collections.singletonList(dBFSList);
        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(timeList, dataIRIList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getDbfsDSResolution(), config.getDbfsDSType());

        clearData();
        return ts;
    }

    @Override
    public void initIRIs() {
        getIrisFromKg();

        if (dbfsIRI == null || dbfsIRI.isEmpty()) {
            dbfsIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_dbfs_" + UUID.randomUUID();

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
        iriHashMap.put("dbfs", dbfsIRI);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        dBFSList.clear();
    }

    @Override
    void getIrisFromKg() {
        Var VAR_O = Var.alloc("o");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix ("saref",SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om",OM)
                .addWhere(smartphoneIRINode,"saref:consistsOf","?microphone")
                .addWhere("?microphone","rdf:type","ontodevice:Microphone")
                .addWhere("?microphone","ontodevice:measures","?om_soundPressureLevel")
                .addWhere("?om_soundPressureLevel", "om:hasValue", VAR_O);

        SelectBuilder sb = new SelectBuilder()
                .addVar(VAR_O).addWhere(wb);

        JSONArray queryResult=storeClient.executeQuery(sb.buildString());
        if (queryResult.isEmpty()) {
            return;
        }
        dbfsIRI = queryResult.getJSONObject(0).optString("o");
    }
}
