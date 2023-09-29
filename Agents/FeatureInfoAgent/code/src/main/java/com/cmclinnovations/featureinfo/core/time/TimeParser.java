package com.cmclinnovations.featureinfo.core.time;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.core.meta.MetaParser;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This class is responsible for formatting and combining the raw TimeSeries instances
 * into a single JSONArray that can be returned to the visualisation.
 */
public class TimeParser {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaParser.class);

    /**
     * Allowable names for query result columns.
     */
    public static final String[] KEYS = new String[]{"Measurement", "Forecast", "Time Series", "Name", "Unit"};

	/**
	 * Constructor.
	 */
	private TimeParser() {
		// No.
	}
	
    /**
     * Find the JSON field for the input key in any case.
     * 
     * @param key JSON key.
     * @param entry JSONObject to search within.
     * 
     * @return Resulting value (or null).
     */
    public static Object findField(String key, JSONObject entry) {
        if(entry.has(key)) return entry.get(key);
        if(entry.has(key.toLowerCase())) return entry.get(key.toLowerCase());
        if(entry.has(key.toUpperCase())) return entry.get(key.toUpperCase());
        
        return null;
    }

    /**
     * Combine timeseries objects into a single timeseries objects. For some reason this is
     * required by the timeseries client before a conversion to JSON can happen.
     * 
     * Code taken from Kok Foong.
     * 
     * @param tsClient time series client
     * @param tsList list of time series instances
     * 
     * @return combined timeseries instance.
     */
    @SuppressWarnings("all")
    public static TimeSeries<Instant> getCombinedTimeSeries(TimeSeriesClient<Instant> tsClient, Collection<TimeSeries<Instant>> tsList) {
    	if (tsList.size() > 1) {
    		// this will sort in ascending order
    		List<TimeSeries<Instant>> ts_sorted = tsList.stream().sorted(Comparator.comparing(ts -> ts.getTimes().size())).collect(Collectors.toList());
    		List<Instant> longestTimeList = ts_sorted.get(ts_sorted.size()-1).getTimes();
    		
    		List<List<?>> valuesList = new ArrayList<>();
    		List<String> dataIRIs = new ArrayList<>();
    		
    		for (int i = 0; i < ts_sorted.size() ; i++) {
    			TimeSeries<Instant> ts = ts_sorted.get(i);
    			List<Double> values = new ArrayList<>();
    			// each time series has one column
    			String dataIRI = ts.getDataIRIs().get(0);
    			
    			// need to query data from the day before
    			Double valueBefore = null;
    			if (ts.getTimes().get(0).isAfter(longestTimeList.get(0))) {
    				TimeSeries<Instant> extraInfo = tsClient.getTimeSeriesWithinBounds(Arrays.asList(dataIRI), longestTimeList.get(0).minus(1, ChronoUnit.DAYS), longestTimeList.get(0));
    				
    				if (extraInfo.getTimes().size() == 0) {
    					// could potentially implement a while loop here
    					LOGGER.warn("getCombinedTimeSeries: no extra data obtained");
    					valueBefore = 0.0;
    				} else {
    					// get final value in the list
    					valueBefore = extraInfo.getValuesAsDouble(dataIRI).get(extraInfo.getValuesAsDouble(dataIRI).size()-1);
    				}
    			}
    			for (Instant time : longestTimeList) {
    				int index; // work out which index to extract value from
    				if (ts.getTimes().contains(time)) {
    					index = ts.getTimes().indexOf(time);
    					
    				} else if (time.isAfter(ts.getTimes().get(ts.getTimes().size()-1))) {
    					// get final index
    					index = ts.getTimes().size() - 1;
    				
    				} else {
    					// this gives the element right after the time point
    					Instant t1 = ts.getTimes().stream().filter(t -> t.isAfter(time)).findFirst().get();
    					index = ts.getTimes().indexOf(t1) - 1;
    				}
    				
    				if (index < 0) {
    					values.add(valueBefore);
    				} else {
    					values.add(ts.getValuesAsDouble(dataIRI).get(index));
    				}
    			}
    			
    			dataIRIs.add(dataIRI);
    			valuesList.add(values);
    		}
    		
    		return new TimeSeries<>(longestTimeList, dataIRIs, valuesList);
    	} else {
    		return tsList.iterator().next();
    	}
    }

	/**
	 * 
	 * @param timeseries
	 * @param measurableUnits
	 * @param measurableNames
	 * @return
	 */
	public static JSONArray convertToJSON(TimeSeries<Instant> timeseries, Map<String, String> measurableUnits, Map<String, String> measurableNames) {
		JSONArray result = new JSONArray();
		JSONObject parentObj = new JSONObject();

		// Add array of units
		JSONArray unitsArray = new JSONArray();
		measurableUnits.values().forEach(unit -> {
			unitsArray.put(unit);
		});
		parentObj.put("units", unitsArray);

		// Add array of names
		JSONArray nameArray = new JSONArray();
		measurableNames.values().forEach(name -> {
			nameArray.put(name);
		});
		parentObj.put("data", nameArray);

		// Add ID (never used in vis so can be any value)
		parentObj.put("id", "1");

		// Add time class (limited to Instant for now)
		parentObj.put("timeClass", "Instant");
		
		// Add time values
		parentObj.put("time", timeseries.getTimes());

		// Add value classes
		Set<String> dataIRIs = measurableUnits.keySet();
		JSONArray valueClassArray = new JSONArray();

		dataIRIs.forEach(dataIRI -> {
			List<?> values = timeseries.getValues(dataIRI);
			if(values != null && !values.isEmpty()) {
				Object firstValue = values.get(0);
				if (firstValue instanceof Number) {
					valueClassArray.put("Number");
				} else {
					valueClassArray.put(firstValue.getClass().getSimpleName());
				}	
			}
		});
		parentObj.put("valuesClass", valueClassArray);

		// Add values
		JSONArray valuesArray = new JSONArray();
		dataIRIs.forEach(dataIRI -> {
			List<?> values = timeseries.getValues(dataIRI);
			if(values != null && !values.isEmpty()) {
				valuesArray.put(values);
			}
		});
		parentObj.put("values", valuesArray);

		// Return
		result.put(parentObj);
		return result;
	}
}
// End of class.