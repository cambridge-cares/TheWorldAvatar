package uk.ac.cam.cares.jps.base.timeseries;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.postgis.Point;
import org.postgis.PGgeometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * <T> is the class for your time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 * @author Kok Foong Lee
 */

public class TimeSeries<T> {

    private final List<T> times;
    private final Map<String, List<?>> values;

    /**
     * Standard constructor
     * @param times list of timestamps
     * @param dataIRI list of data IRIs provided as string
     * @param values list of list of values containing the data for each data IRI
     */
	public TimeSeries(List<T> times, List<String> dataIRI, List<List<?>> values) {
        this.times = times;
        this.values = new HashMap<>();
        
        // Check validity of provided input parameters
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
    
	/**
	 *  Method to get timestamps of timeseries
	 */
	public List<T> getTimes() {
    	return times;
    }
	
    /**
     * Retrieve time series values for provided data IRI as Doubles
     * @param dataIRI data IRI provided as string
     */
    public List<Double> getValuesAsDouble(String dataIRI) {
    	List<?> v = getValues(dataIRI);
    	if (v == null) {
    		return null;
    	} else {
            try {
                return v.stream().map(value -> value == null ? null : ((Number) value).doubleValue()).collect(Collectors.toList());
            } catch (Exception e) {
                throw new JPSRuntimeException("TimeSeries: Values for provided dataIRI are not castable to \"Number\"");
            }
        }
    }    
    
    /**
     * Retrieve time series values for provided data IRI as Integers
     * @param dataIRI data IRI provided as string
     */
    public List<Integer> getValuesAsInteger(String dataIRI) {
    	List<?> v = getValues(dataIRI);
    	if (v == null) {
    		return null;
    	} else {
            try {
                return v.stream().map(value -> value == null ? null : ((Number) value).intValue()).collect(Collectors.toList());
            } catch (Exception e) {
                throw new JPSRuntimeException("TimeSeries: Values for provided dataIRI are not castable to \"Number\"");
            }
        }
    }

    /**
     * Retrieve time series values for provided data IRI as Strings
     * @param dataIRI data IRI provided as string
     */
    public List<String> getValuesAsString(String dataIRI) {
    	return values.get(dataIRI).stream().map(value -> value == null ? null : value.toString()).collect(Collectors.toList());
    }

    /*
     * Retrieve values as PostGIS points
     * @param dataIRI data IRI provided as string
     */
    public List<Point> getValuesAsPoint(String dataIRI) {
        List<?> v = getValues(dataIRI);
    	if (v == null) {
    		return null;
    	} else {
            return v.stream().map(value -> {
                if (value == null) {
                    return null;
                } else if (value.getClass() == Point.class) {
                    // this is for manually created TimeSeries objects
                    return (Point) value;
                } else if (value.getClass() == PGgeometry.class){
                    // this is for results queried from PostGIS
                    return ((Point) ((PGgeometry) value).getGeometry());
                } else {
                    throw new JPSRuntimeException("TimeSeries: Values for provided dataIRI are not castable to \"Point\"");
                }
            }).collect(Collectors.toList());
        }
    }
    
    /**
     * Method to get values column in whatever form returned from the jooq API (not recommended!)
     * @param dataIRI data IRI provided as string
     */
    public List<?> getValues(String dataIRI) {
    	return values.get(dataIRI);
    }
    
    /**
     * Method to get dataIRIs of timeseries
     * @return List of strings representing the data IRIs
     */    
    public List<String> getDataIRIs() {
        Collection<String> keys = values.keySet();
        return new ArrayList<>(keys);
    }
}
