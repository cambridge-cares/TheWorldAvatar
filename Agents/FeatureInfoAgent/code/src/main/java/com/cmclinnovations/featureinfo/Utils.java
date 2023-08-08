package com.cmclinnovations.featureinfo;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Misc utilities.
 */
public final class Utils {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(Utils.class);

    /**
     * Constructor.
     */
    private Utils() {
        // No
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

}
// End of class.