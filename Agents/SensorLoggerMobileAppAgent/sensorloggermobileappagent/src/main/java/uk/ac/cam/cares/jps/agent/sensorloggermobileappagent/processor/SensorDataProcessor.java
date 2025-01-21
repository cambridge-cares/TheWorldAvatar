package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

public abstract class SensorDataProcessor {
    AgentConfig config;
    RemoteStoreClient ontopClient;
    RemoteStoreClient blazegraphClient;
    Node smartphoneIRINode;
    final List<OffsetDateTime> timeList = new ArrayList<>();
    List<SensorData<?>> sensorData;
    String sensorName;
    String tsIri;
    Logger logger;

    public SensorDataProcessor(String sensorName, AgentConfig config, RemoteStoreClient ontopClient, RemoteStoreClient blazegraphClient, Node smartphoneIRINode) {
        this.sensorName = sensorName;
        this.config = config;
        this.ontopClient = ontopClient;
        this.blazegraphClient = blazegraphClient;
        this.smartphoneIRINode = smartphoneIRINode;

        initSensorData();
        initIRIs();
        getTimeSeriesIrisFromBlazegraph();
    }

    public abstract void addData(Payload data);

    public abstract TimeSeries<Long> getProcessedTimeSeries() throws Exception;
    abstract void initSensorData();

    public void initIRIs() {
        List<String> iris = getDataIRIs();
        if (iris.stream().allMatch(Objects::nonNull)) {
            return;
        }

        getDataIrisFromKg();

        // If all sensorData iris are null, device not instantiated, so all sensor data IRIs not instantiated with obda mapping

        // If have existing deviceIRI, but some sensor data are not linked with time series, which require check of the blazegraph.
        // This is handled in the SmartphoneRecordingTask.
    };

    public List<SensorData<?>> getSensorData() {
        return sensorData;
    };

    public List<Class<?>> getDataClass() {
        return getSensorData().stream().map(SensorData::getType).collect(Collectors.toList());
    };

    public List<String> getDataIRIs() {
        return getSensorData().stream().map(SensorData::getDataIri).toList();
    };

    public List<List<?>> getValues() {
        return getSensorData().stream().map(SensorData::getValues).collect(Collectors.toList());
    }

    void clearData() {
        timeList.clear();
        getSensorData().forEach(SensorData::clearData);
    };

    abstract void getDataIrisFromKg();

    public void getTimeSeriesIrisFromBlazegraph() {
        Var dataIRI = Var.alloc("dataIRI");
        Var tsIRI = Var.alloc("tsIRI");
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ontots", OntoConstants.ONTOTS)
                .addWhere(dataIRI, "ontots:hasTimeSeries", tsIRI);

        SelectBuilder sb = new SelectBuilder()
                .setDistinct(true)
                .addVar(dataIRI)
                .addVar(tsIRI)
                .addWhere(wb)
                .addValueVar(dataIRI);
        getDataIRIs().stream().map(iri -> String.format("<%s>", iri)).forEach(iri -> sb.addValueRow(iri));

        JSONArray queryResult;
        try {
            queryResult = blazegraphClient.executeQuery(sb.buildString());
        } catch (Exception e) {
            // ontop does not accept queries before any mapping is added
            logger.error("Error getting time series IRIs from blazegraph: " + e.getMessage());
            return;
        }
        if (queryResult.isEmpty()) {
            getSensorData().forEach(needToInitTimeSeries -> needToInitTimeSeries.setNeedToInitTimeSeries(true));
            return;
        }

        tsIri = queryResult.getJSONObject(0).optString("tsIRI");
        List<String> dataIrisWithTs = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            dataIrisWithTs.add(queryResult.getJSONObject(i).getString("dataIRI"));
        }

        for (SensorData sd : sensorData) {
            sd.setNeedToInitTimeSeries(!dataIrisWithTs.contains(sd.getDataIri()));
        }
    }

    public String getTsIri() {
        return tsIri;
    }

    public boolean isNeedToInstantiateDevice() {
        return getDataIRIs().stream().allMatch(iri -> iri == null);
    }

    public boolean isNeedToInitTimeSeries() {
        return tsIri == null;
    }

    public int getTimeSeriesLength() {
        return timeList.size();
    }

    public String getSensorName() {
        return sensorName;
    }

    public abstract String getOntodeviceLabel();
}
