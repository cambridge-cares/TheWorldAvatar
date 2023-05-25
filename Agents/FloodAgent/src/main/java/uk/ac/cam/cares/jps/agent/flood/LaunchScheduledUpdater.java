package uk.ac.cam.cares.jps.agent.flood;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Executors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class LaunchScheduledUpdater {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(LaunchScheduledUpdater.class);
    
    private static final ScheduledExecutorService scheduler = Executors
    		.newScheduledThreadPool(1);
    		
    public static void main(String[] args) throws IOException, URISyntaxException {
    	EndpointConfig endpointConfig = new EndpointConfig();
    	RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(),endpointConfig.getKgurl());
        FloodSparql sparqlClient = new FloodSparql(storeClient);
        
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

            	LOGGER.info(String.format("Calling UpdateStations with %s", yesterday.toString()));
            	String[] input = new String[1];
            	input[0] = yesterday.toString();
            	UpdateStations.main(input);
                
                LOGGER.info(String.format("Next update will be at %s", nextUpdate));
            } catch (Exception ex) {
                LOGGER.error(ex.getMessage());
            }
        }, 0, 1, TimeUnit.DAYS);
    }
}
