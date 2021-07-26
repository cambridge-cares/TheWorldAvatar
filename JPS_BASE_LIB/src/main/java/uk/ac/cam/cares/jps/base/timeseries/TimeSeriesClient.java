package uk.ac.cam.cares.jps.base.timeseries;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class represents the MAIN entry point to interact with time series in The World Avatar
 * 
 * It uses the TimeSeriesRDBClient class to interact with the relational database and the
 * TimeSeriesSparql class to interact with the Triple Store.
 * 
 * @author Markus Hofmeister & Niklas Kasenburg
 * @param <T> is the class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
 */

public class TimeSeriesClient<T> {
	// Associated RDB and RDF/SPARQL clients
	private TimeSeriesRDBClient<T> rdbClient;
	private TimeSeriesSparql rdfClient;
	
    /**
     * Standard constructor
     * @param kbClient: Knowledge base client used to query and update the knowledge base containing timeseries information (potentially with already specified endpoint (triplestore/owl file))
     * @param timeClass: Class type for the time values, e.g. LocalDateTime, Timestamp, Integer, Double etc.
     */
    public TimeSeriesClient(StoreClientInterface kbClient, Class<T> timeClass) {
    	this.rdfClient = new TimeSeriesSparql(kbClient);
    	this.rdbClient = new TimeSeriesRDBClient<>(timeClass);    	
    }
    
    /**
     * 
     * @param propertiesfile
     */
    public void loadConfigs(String propertiesfile) {
    	
    }


}
