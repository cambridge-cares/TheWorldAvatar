package uk.ac.cam.cares.derivedagent.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantityClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

/**
 * An agent to modify the input
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
		if (validateInput(requestParams)) {
		    String input_iri = requestParams.getString(DerivedQuantityClient.AGENT_INPUT_KEY);
		    
		    ExampleConfig.initProperties();
		    
		    TimeSeriesRDBClient<Integer> tsClient = new TimeSeriesRDBClient<Integer>(Integer.class);
	    	tsClient.setRdbURL(ExampleConfig.dburl);
	    	tsClient.setRdbUser(ExampleConfig.dbuser);
	    	tsClient.setRdbPassword(ExampleConfig.dbpassword);
	    	
	    	// create random time series
	    	Random rand = new Random();
	    	List<Integer> time_column = Arrays.asList(rand.nextInt());
	    	List<List<?>> values = new ArrayList<>();
	    	List<Integer> value_column = Arrays.asList(rand.nextInt());
	    	values.add(value_column);
	    	TimeSeries<Integer> ts = new TimeSeries<Integer>(time_column, Arrays.asList(input_iri), values);
	    	
	    	tsClient.addTimeSeriesData(ts);
	    	
	    	RemoteStoreClient storeClient = new RemoteStoreClient(ExampleConfig.kgurl,ExampleConfig.kgurl,ExampleConfig.kguser,ExampleConfig.kgpassword);
	    	DerivedQuantityClient devClient = new DerivedQuantityClient(storeClient);
	    	
	    	devClient.updateTimestamp(input_iri);
		}
		return requestParams;
	}
	
	public boolean validateInput(JSONObject requestParams) {
		return true;
	}
}
