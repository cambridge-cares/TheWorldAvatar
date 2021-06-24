package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * 
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
        
        if (dataIRI.size() != values.size()) {
        	throw new JPSRuntimeException("TimeSeries: Length of data IRI is different from provided data.");
        }
        
        for (int i = 0; i < dataIRI.size(); i++) {
            this.values.put(dataIRI.get(i), values.get(i));
        }
    }
    
    /**
     * Returns values as double
     * @param dataIRI
     * @return
     */
    public List<Double> getValuesAsDouble(String dataIRI) {
    	return values.get(dataIRI).stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
    }
    
    public List<?> getValues(String dataIRI) {
    	return values.get(dataIRI);
    }
    
    public List<T> getTimes() {
    	return times;
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
