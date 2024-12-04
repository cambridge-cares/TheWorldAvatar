package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.Payload;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class DBFSDataProcessor extends SensorDataProcessor {
    private String dbfsIRI = null;
    private final List<Double> dBFSList = new ArrayList<>();

    public DBFSDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneNode) {
        super(config, storeClient, smartphoneNode);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getdBFSTs());
        dBFSList.addAll(data.getdBFSs());
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<String> dataIRIList = Collections.singletonList(dbfsIRI);
        List<List<?>> valueList = Collections.singletonList(dBFSList);

        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(timeList, dataIRIList, valueList);
        ts = Downsampling.downsampleTS(ts, config.getDbfsDSResolution(), config.getDbfsDSType());

        List<Long> epochlist = ts.getTimes().stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        List<List<?>> downsampledValuesList = Arrays.asList(ts.getValuesAsDouble(dbfsIRI));

        clearData();
        return new TimeSeries<>(epochlist, dataIRIList, downsampledValuesList);
    }

    @Override
    public List<Class<?>> getDataClass() {
        return Collections.nCopies(getDataIRIs().size(), Double.class);
    }

    @Override
    public List<String> getDataIRIs() {
        return List.of(dbfsIRI);
    }

    @Override
    void clearData() {
        timeList.clear();
        dBFSList.clear();
    }

    @Override
    void getIrisFromKg() {
        Var varO = Var.alloc("o");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?microphone")
                .addWhere("?microphone", "rdf:type", "ontodevice:Microphone")
                .addWhere("?microphone", "ontodevice:measures", "?om_soundPressureLevel")
                .addWhere("?om_soundPressureLevel", "om:hasValue", varO);

        SelectBuilder sb = new SelectBuilder().addVar(varO).addWhere(wb);

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
        dbfsIRI = queryResult.getJSONObject(0).optString("o");
    }

    @Override
    public String getOntodeviceLabel() {
        return "Microphone";
    }
}
