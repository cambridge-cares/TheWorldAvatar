package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.jooq.Record;
import org.jooq.Result;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * 
 * @author Kok Foong Lee
 *
 * @param <T>
 * @param <V>
 */
public class TimeSeries<T, V> {

    private final List<T> times;
    private final Map<String, List<V>> values;

    /**
     * standard constructor
     * @param times
     * @param dataIRI
     * @param data
     */
    @SafeVarargs
	public TimeSeries(List<T> times, List<String> dataIRI, List<V>... data) {
        this.times = times;
        this.values = new HashMap<String, List<V>>();
        
        if (dataIRI.size() != data.length) {
        	throw new JPSRuntimeException("TimeSeries: Length of data IRI is different from provided data.");
        }
        
        for (int i = 0; i < dataIRI.size(); i++) {
            this.values.put(dataIRI.get(i), data[i]);
        }
    }

    /**
     * constructor for results given by jooq
     * @param data
     * @param timeColName
     */
    public TimeSeries(Result<? extends Record> data, String timeColName, Map<String,String> dataColumnNames) { 
        values = data.fieldsRow().fieldStream()
                // Skip the time column
                .filter(field -> !field.getName().equals(timeColName))
                .collect(
                        Collectors.toMap(
                                // Use the field Name as the column name
                                field -> dataColumnNames.get(field.getName()),
                                // Get the values for the column
                                field -> data.getValues(field).stream()
                                        // Cast the values to the value type (V) stored in this TimeSeries
                                        .map(value -> (V) value)
                                        .collect(Collectors.toList())
                        )
                );
        times = data.getValues(timeColName).stream()
                // Cast the times to the time type (T) stored in this TimeSeries
                .map(value -> (T) value)
                .collect(Collectors.toList());
    }
    
    public List<T> getTimes() {
        return times;
    }

    public Class<?> getTimeClass() {
    	return times.get(0).getClass();
    }
    
    public Class<?> getValueClass(String dataIRI) {
    	return values.get(dataIRI).get(0).getClass();
    }
    
    public List<V> getValues(String dataIRI) {
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
