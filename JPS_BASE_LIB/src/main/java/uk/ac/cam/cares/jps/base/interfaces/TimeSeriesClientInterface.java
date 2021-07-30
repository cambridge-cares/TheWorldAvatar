package uk.ac.cam.cares.jps.base.interfaces;

import java.util.List;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public interface TimeSeriesClientInterface<T> {
	/**
	 * appends given time series data to your database
	 * @param ts
	 */
	void addTimeSeriesData(TimeSeries<T> ts);
	
	/**
	 * returns the entire time series
	 * @param dataIRI
	 * @return
	 */
	TimeSeries<T> getTimeSeries(List<String> dataIRI);
	
	/**
	 * returns time series within the given bounds
	 * @param dataIRI
	 * @param lowerbound
	 * @param upperbound
	 * @return
	 */
	TimeSeries<T> getTimeSeriesWithinBounds(List<String> dataIRI, T lowerbound, T upperbound);
	
	/**
	 * deletes data between the given bounds
	 * @param dataIRI
	 * @param lowerBound
	 * @param upperBound
	 */
	void deleteTimeSeriesHistory(String dataIRI, T lowerBound, T upperBound);
	
	/**
	 * deletes the entire time series data
	 * @param dataIRI
	 */
	void deleteTimeSeries(String dataIRI);
}
