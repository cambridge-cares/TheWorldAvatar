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
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = 
				new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		storeClient.executeUpdate("drop all");
		tsClient.deleteAll();
	}
}
