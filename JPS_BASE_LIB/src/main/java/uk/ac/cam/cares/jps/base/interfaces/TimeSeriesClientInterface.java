package uk.ac.cam.cares.jps.base.interfaces;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public interface TimeSeriesClientInterface {
	/** 
	 * sets the KnowledgeBaseClient so that the TimeSeriesClient knows where to execute queries/updates
	 * @param kbClient
	 */
	void setKBClient(KnowledgeBaseClientInterface kbClient);
	
	/**
	 * Instantiates time series IRI in your endpoint
	 * @param TimeSeries
	 * @param timeUnit
	 */
	void init(TimeSeries<?, ?> ts);

	void addData(TimeSeries<?, ?> ts);
}
