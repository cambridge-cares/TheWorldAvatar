package cares.cam.ac.uk.ouraring.data;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

public class HeartRateData {
    private List<Instant> timestamps = new ArrayList<>();
    private List<Double> values = new ArrayList<>();

    public void addValue(Instant timestamp, double value) {
        timestamps.add(timestamp);
        values.add(value);
    }

    public List<Instant> getTimestamps() {
        return timestamps;
    }

    public List<Double> getValues() {
        return values;
    }
}
