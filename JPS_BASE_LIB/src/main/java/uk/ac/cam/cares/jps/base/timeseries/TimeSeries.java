package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TimeSeries {
    private final List<Object> time;
    private final List<List<Object>> values;

    public TimeSeries(List<Object> time, List<List<Object>> values) {
    	this.time = new ArrayList<>(time);
    	
    	this.values= new ArrayList<>();
    	
    	for (int i=0; i<values.size(); i++) {
    		this.values.add(new ArrayList<>(values.get(i)));
    	}
    }

	public List<Object> getTime() {
		return Collections.unmodifiableList(time);
	}
	public List<List<Object>> getValues() {
		return Collections.unmodifiableList(values);
	}
}
