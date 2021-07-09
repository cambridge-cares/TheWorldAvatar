package uk.ac.cam.cares.jps.base.timeseries.test;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.junit.Before;
import org.junit.Assert;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class TimeSeriesTest {
	
	TimeSeries<Instant> ts;
	List<String> dataIRI = new ArrayList<>();
	List<Instant> timeList = new ArrayList<>();
	List<Double> data1 = new ArrayList<>();
	List<String> data2 = new ArrayList<>();
	List<Integer> data3 = new ArrayList<>();
	List<List<?>> dataToAdd = new ArrayList<>();
	
	@Before
	public void initialiseData() {
		dataIRI.add("http://data1"); dataIRI.add("http://data2"); dataIRI.add("http://data3"); 
		for (int i = 0; i < 10; i++) {
			timeList.add(Instant.now().plusSeconds(i));
			data1.add(Double.valueOf(i));
			data2.add(String.valueOf(i));
			data3.add(Integer.valueOf(i));
		}		
		dataToAdd.add(data1); dataToAdd.add(data2); dataToAdd.add(data3);
	}
	
	@Test (expected = JPSRuntimeException.class)
	public void testConstructorWithWrongDataIRIsize() {
		// drop last data series to create mismatch between number of DataIRIs and data series
		dataToAdd.remove(dataToAdd.size()-1);
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
	}
	
	@Test (expected = JPSRuntimeException.class)
	public void testConstructorWithWrongDataSeriesLength() {
		// drop last entry of each data series to create mismatch between length of time series and data series
		for (List l : dataToAdd) {
			l.remove(l.size()-1);
		}
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
	}
	
	@Test
    public void testGetTimes() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	Assert.assertEquals(ts.getTimes(), timeList);
    }
    
	@Test
    public void testGetDataIRI() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	for (String iri : ts.getDataIRI()) {
    		Assert.assertTrue(dataIRI.contains(iri));
    	}
    }
    
	@Test
    public void testGetValues() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	Assert.assertEquals(ts.getValues(dataIRI.get(0)),data1);
    	Assert.assertEquals(ts.getValues(dataIRI.get(1)),data2);
    	Assert.assertEquals(ts.getValues(dataIRI.get(2)),data3);
    }
    
    /**
     * to use this, the original data must be an instance of "Number"
     */
	@Test
    public void testGetValuesAsDouble() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	Assert.assertEquals(ts.getValuesAsDouble(dataIRI.get(0)).get(0).getClass(), Double.class);
    	Assert.assertEquals(ts.getValuesAsDouble(dataIRI.get(2)).get(0).getClass(), Double.class);
    }
    
    /** 
     * if the original class has a toString() method, this should work
     */
	@Test
    public void testGetValuesAsString() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	for (String data : dataIRI) {
    		Assert.assertEquals(ts.getValuesAsString(data).get(0).getClass(), String.class);
    		// mh807: include test for String time series
    	}
    }
    
    /**
     * to use this, the original data must be an instance of "Number"
     */
	@Test
    public void testGetValuesAsInteger() {
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
    	Assert.assertEquals(ts.getValuesAsInteger(dataIRI.get(0)).get(0).getClass(), Integer.class);
    	Assert.assertEquals(ts.getValuesAsInteger(dataIRI.get(2)).get(0).getClass(), Integer.class);
    }
}
