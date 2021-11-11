package uk.ac.cam.cares.jps.agent.flood;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.time.Instant;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class LaunchScheduledUpdaterAndWriter {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(LaunchScheduledUpdaterAndWriter.class);
    
    private final static ScheduledExecutorService scheduler = Executors
    		.newScheduledThreadPool(1);
    
    private static RemoteStoreClient storeClient;
    private static FloodSparql sparqlClient;
    		
    public static void main(String[] args) {
    	Config.initProperties();
    	storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
        sparqlClient = new FloodSparql(storeClient);
        
        // initialise stations in blazegraph and time series in postgres
        if (!sparqlClient.areStationsInitialised()) {
        	// arguments are not needed for the below function
        	LOGGER.info("Initialising stations");
        	InitialiseStations.main(args);
        }
        
        startScheduledTask();
    }
    
    static void startScheduledTask() {
    	scheduler.scheduleAtFixedRate(() -> {
            try {
            	Instant nextUpdate = Instant.now().plus(1, ChronoUnit.DAYS);
            	LOGGER.info("Launching scheduled task to update stations");
            	// date to query
                LocalDate yesterday = LocalDate.now().minusDays(1);

            	LOGGER.info("Calling UpdateStations with " + yesterday.toString());
            	String[] input = new String[1];
            	input[0] = yesterday.toString();
            	UpdateStations.main(input);
            	
            	LOGGER.info("Writing output files for "+ yesterday);
            	WriteOutputs.main(input);
                
                LOGGER.info("Next update will be at " + nextUpdate);
            } catch (Exception ex) {
                LOGGER.error(ex.getMessage());
            }
        }, 0, 1, TimeUnit.DAYS);
    }
}
