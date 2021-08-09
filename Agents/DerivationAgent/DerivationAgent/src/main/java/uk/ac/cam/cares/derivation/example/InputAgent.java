package uk.ac.cam.cares.derivation.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.derivation.config.Config;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This input agent adds a row of value in the table and updates its timestamp
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/InputAgent"}) 
public class InputAgent extends JPSAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
	    
	    RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
	    SparqlClient sparqlClient = new SparqlClient(storeClient);
    	DerivationClient devClient = new DerivationClient(storeClient);
		
		if (InstancesDatabase.Input == null) { 
			InstancesDatabase.Input = sparqlClient.getInputIRI();
	    }
	    
	    TimeSeriesClient<Integer> tsClient = new TimeSeriesClient<Integer>(storeClient, Integer.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	
    	// add random value to value column
    	Random rand = new Random();
    	List<Integer> time_column = Arrays.asList(tsClient.getMaxTime(InstancesDatabase.Input)+1);

    	List<List<?>> values = new ArrayList<>();
    	List<Integer> value_column = Arrays.asList(rand.nextInt());
    	values.add(value_column);
    	TimeSeries<Integer> ts = new TimeSeries<Integer>(time_column, Arrays.asList(InstancesDatabase.Input), values);
    	
    	tsClient.addTimeSeriesData(ts);

    	devClient.updateTimestamp(InstancesDatabase.Input);
		return requestParams;
	}
}
