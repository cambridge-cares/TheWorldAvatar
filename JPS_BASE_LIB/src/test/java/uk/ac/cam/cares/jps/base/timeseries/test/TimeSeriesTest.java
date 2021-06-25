package uk.ac.cam.cares.jps.base.timeseries.test;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public class TimeSeriesTest extends TestCase {
	TimeSeries<Instant> ts;
	List<String> dataIRI = new ArrayList<>();
	List<Instant> timeList = new ArrayList<>();
	List<Double> data1 = new ArrayList<>();
	List<String> data2 = new ArrayList<>();
	List<Integer> data3 = new ArrayList<>();
	
	public void testInitialise() {
		dataIRI.add("http://data1"); dataIRI.add("http://data2"); dataIRI.add("http://data3"); 
		for (int i = 0; i < 10; i++) {
			timeList.add(Instant.now().plusSeconds(i));
			data1.add(Double.valueOf(i));
			data2.add(String.valueOf(i));
			data3.add(Integer.valueOf(i));
		}
		List<List<?>> dataToAdd = new ArrayList<>();
		dataToAdd.add(data1); dataToAdd.add(data2); dataToAdd.add(data3);
		// the constructor for the TimeSeries object takes in the time column, dataIRIs, and the corresponding values in lists
		ts = new TimeSeries<Instant>(timeList, dataIRI, dataToAdd);
	}
	
    public void testGetTimes() {
    	testInitialise();
    	assertEquals(ts.getTimes(), timeList);
    }
    
    public void testGetDataIRI() {
    	testInitialise();
    	for (String iri : ts.getDataIRI()) {
    		assertTrue(dataIRI.contains(iri));
    	}
    }
    
    public void testGetValues() {
    	testInitialise();
    	assertEquals(ts.getValues(dataIRI.get(0)),data1);
    	assertEquals(ts.getValues(dataIRI.get(1)),data2);
    	assertEquals(ts.getValues(dataIRI.get(2)),data3);
    }
    
    /**
     * to use this, the original data must be an instance of "Number"
     */
    public void testGetValuesAsDouble() {
    	testInitialise();
    	assertEquals(ts.getValuesAsDouble(dataIRI.get(0)).get(0).getClass(), Double.class);
    	assertEquals(ts.getValuesAsDouble(dataIRI.get(2)).get(0).getClass(), Double.class);
    }
    
    /** 
     * if the original class has a toString() method, this should work
     */
    public void testGetValuesAsString() {
    	testInitialise();
    	for (String data : dataIRI) {
    		assertEquals(ts.getValuesAsString(data).get(0).getClass(), String.class);
    	}
    }
    
    /**
     * to use this, the original data must be an instance of "Number"
     */
    public void testGetValuesAsInteger() {
    	testInitialise();
    	assertEquals(ts.getValuesAsInteger(dataIRI.get(0)).get(0).getClass(), Integer.class);
    	assertEquals(ts.getValuesAsInteger(dataIRI.get(2)).get(0).getClass(), Integer.class);
    }
}
