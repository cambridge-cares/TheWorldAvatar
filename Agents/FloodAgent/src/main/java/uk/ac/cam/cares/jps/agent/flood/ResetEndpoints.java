package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * resets both Blazegraph and Postgres endpoints
 * @author Kok Foong Lee
 *
 */
public class ResetEndpoints {
	/**
	 * no input required
	 * @param args
	 */
	public static void main(String[] args) {
		EndpointConfig endpointConfig = new EndpointConfig();
		RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
		TimeSeriesClient<Instant> tsClient = 
				new TimeSeriesClient<>(storeClient, Instant.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
		storeClient.executeUpdate("drop all");
		tsClient.deleteAll();
	}
}
