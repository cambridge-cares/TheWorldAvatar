package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * <T> is the class for your time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 * @author Kok Foong Lee
 */
public class TimeSeries<T> {

    private final List<T> times;
    private final Map<String, List<?>> values;

    /**
     * standard constructor
     * @param times
     * @param dataIRI
     * @param data
     */
	public TimeSeries(List<T> times, List<String> dataIRI, List<List<?>> values) {
        this.times = times;
        this.values = new HashMap<String, List<?>>();
        
        if (dataIRI.size() == 0) {
        	throw new JPSRuntimeException("TimeSeries: No data IRI has been provided.");
        }
        
        if (dataIRI.size() != values.size()) {
        	throw new JPSRuntimeException("TimeSeries: Length of data IRI is different from provided data.");
        }
        
        for (List<?> v : values) {
        	if (v.size() != times.size()) {
        		throw new JPSRuntimeException("TimeSeries: Number of time steps does not match number of values for all series.");
        	}
        }
    
        for (int i = 0; i < dataIRI.size(); i++) {
            this.values.put(dataIRI.get(i), values.get(i));
        }
    }
    
	public List<T> getTimes() {
    	return times;
    }
	
    /**
     * Various methods to get values in a specific class
     * @param dataIRI
     * @return
     */
    public List<Double> getValuesAsDouble(String dataIRI) {
    	return values.get(dataIRI).stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
    }
    public List<Integer> getValuesAsInteger(String dataIRI) {
    	return values.get(dataIRI).stream().map(value -> ((Number) value).intValue()).collect(Collectors.toList());
    }
    public List<String> getValuesAsString(String dataIRI) {
    	return values.get(dataIRI).stream().map(value -> ((Object) value).toString()).collect(Collectors.toList());
    }
    
    /**
     * this will return the values column in whatever form returned from the jooq API, not recommended
     * @param dataIRI
     * @return
     */
    public List<?> getValues(String dataIRI) {
    	return values.get(dataIRI);
    }
    
    public List<String> getDataIRI() {
        Collection<String> keys = values.keySet();
        List<String> dataIRI = new ArrayList<String>();
        
        for (String s : keys) {
        	dataIRI.add(s);
        }
        
        return dataIRI;
    }
}
