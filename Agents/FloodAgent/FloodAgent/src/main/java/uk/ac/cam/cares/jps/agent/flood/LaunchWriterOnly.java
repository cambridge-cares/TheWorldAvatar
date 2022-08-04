package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;
import java.time.LocalDate;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class LaunchWriterOnly {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(LaunchWriterOnly.class);
    
    private static RemoteStoreClient storeClient;
    private static FloodSparql sparqlClient;
    private static TimeSeriesClient<Instant> tsClient;
    		
    public static void main(String[] args) {
    	Config.initProperties();
    	storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
        sparqlClient = new FloodSparql(storeClient);
        tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
        
        // initialise stations in blazegraph and time series in postgres
        if (!sparqlClient.areStationsInitialised()) {
        	// arguments are not needed for the below function
        	LOGGER.error("Stations are not initialised, cannot write output files");
        	System.exit(1);
        } else {
        	writeOutputFiles();
        }
    }
    
    static void writeOutputFiles() {
        try {        	
        	// date to query
            LocalDate lastUpdate = sparqlClient.getLatestUpdate(tsClient);
            LOGGER.info("Last update is on " + lastUpdate);
            
            // write output files for visualisation
        	LOGGER.info("Writing output files for " + lastUpdate);
        	String[] input = new String[1];
        	input[0] = lastUpdate.toString();
        	WriteOutputs.main(input);
        } catch (Exception ex) {
            LOGGER.error(ex.getMessage());
            System.exit(1);
        }
    }
}