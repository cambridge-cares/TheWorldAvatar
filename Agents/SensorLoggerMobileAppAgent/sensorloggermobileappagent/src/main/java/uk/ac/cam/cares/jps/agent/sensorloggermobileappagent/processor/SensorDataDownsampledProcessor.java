package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.graph.Node;
import uk.ac.cam.cares.downsampling.Downsampling;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.stream.Collectors;

public abstract class SensorDataDownsampledProcessor extends SensorDataProcessor{

    SensorData<Double> x;
    SensorData<Double> y;
    SensorData<Double> z;

    private Long dsResolution;
    private Downsampling.Type dsType;

    public SensorDataDownsampledProcessor(String sensorName, AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode,
                                          Long dsResolution,
                                          Downsampling.Type dsType) {
        super(sensorName, config, storeClient, smartphoneIRINode);
        this.dsResolution = dsResolution;
        this.dsType = dsType;
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() throws Exception {
        List<List<?>> valueList = getValues();
        List<String> iriList = getDataIRIs();

        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(timeList, iriList, valueList);
        ts = Downsampling.downsampleTS(ts, dsResolution, dsType);

        List<Long> epochlist = ts.getTimes().stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());

        List<List<?>> downsampledValuesList = getDataIRIs().stream().map(ts::getValuesAsDouble).collect(Collectors.toList());

        clearData();
        return new TimeSeries<>(epochlist, iriList, downsampledValuesList);
    }

}
