package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TimeSeries {
    private final List<Long> time;
    private final List<Double> values;

    public TimeSeries(List<Long> time, List<Double> values) {
    	this.time = new ArrayList<>(time);
    	this.values= new ArrayList<>(values);
    }

	public List<Long> getTime() {
		return Collections.unmodifiableList(time);
	}
	public List<Double> getValues() {
		return Collections.unmodifiableList(values);
	}
}
