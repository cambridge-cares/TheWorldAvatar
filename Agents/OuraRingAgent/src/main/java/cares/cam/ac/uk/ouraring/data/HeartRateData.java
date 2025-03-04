package cares.cam.ac.uk.ouraring.data;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public class HeartRateData {
    private static final Logger LOGGER = LogManager.getLogger(HeartRateData.class);

    private List<Instant> timestamps = new ArrayList<>();
    private List<Integer> bpmList = new ArrayList<>();
    private List<String> sourceList = new ArrayList<>();

    private Instant lastUpdate = null;

    private String bpmIri;
    private String sourceIri;

    public void addValue(Instant timestamp, int bpm, String source) {
        if (timestamps.contains(timestamp)) {
            LOGGER.warn("duplicate timestamp");
            return;
        }
        timestamps.add(timestamp);
        bpmList.add(bpm);
        sourceList.add(source);
    }

    public void setBpmIri(String bpmIri) {
        this.bpmIri = bpmIri;
    }

    public void setSourceIri(String sourceIri) {
        this.sourceIri = sourceIri;
    }

    public String getBpmIri() {
        return bpmIri;
    }

    public String getSourceIri() {
        return sourceIri;
    }

    public TimeSeries<Instant> getHeartRateTimeSeries() {
        List<List<?>> values = new ArrayList<>();
        values.add(bpmList);
        values.add(sourceList);
        return new TimeSeries<>(timestamps, List.of(bpmIri, sourceIri), values);
    }

    public void setLastUpdate(Instant lastUpdate) {
        this.lastUpdate = lastUpdate;
    }

    public Instant getLastUpdate() {
        return lastUpdate;
    }
}
