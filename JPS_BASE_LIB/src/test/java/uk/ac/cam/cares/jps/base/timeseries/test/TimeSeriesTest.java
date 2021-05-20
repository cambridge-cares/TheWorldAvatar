package uk.ac.cam.cares.jps.base.timeseries.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ArrayTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Table;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public class TimeSeriesTest extends TestCase{
	public void testTable() {
		List<Integer> timestamps = List.of(1, 2, 3);
        List<String> dataIRI = List.of("data1", "data2", "data3");
        List<Double> data1 = List.of(1.0,2.0,3.0);
        List<Double> data2 = List.of(1.0,2.0,3.0);
        List<Double> data3 = List.of(1.0,2.0,3.0);
        
        TimeSeries<Integer,Double> ts = new TimeSeries<Integer,Double>(timestamps,dataIRI,data1,data2,data3);
	}

}
