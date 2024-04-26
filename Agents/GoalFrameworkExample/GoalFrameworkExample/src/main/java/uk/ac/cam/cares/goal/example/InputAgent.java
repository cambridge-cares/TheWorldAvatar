package uk.ac.cam.cares.goal.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.CloseableThreadContext;
import org.eclipse.rdf4j.query.algebra.Str;
import org.json.JSONObject;

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
	private static final long serialVersionUID = 1L;
    private static String input_type;
    private final String INPUT_KEY = "input";

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
        SparqlClient sparqlClient = new SparqlClient(storeClient);
        DerivationClient devClient = new DerivationClient(storeClient, InitialiseInstances.goalInstanceBaseURL);

        String input_iri;



        this.input_type = requestParams.getString(INPUT_KEY);

        if (InstancesDatabase.Input == null || InstancesDatabase.InputTruck ==null) {
            InstancesDatabase.Input = sparqlClient.getInputIRI(sparqlClient.BinInput);
            InstancesDatabase.InputTruck = sparqlClient.getInputIRI(sparqlClient.TruckInput);
        }

        if (input_type.equals("truck")){
             input_iri = InstancesDatabase.InputTruck;
        } else if (input_type.equals("bin")) {
            input_iri = InstancesDatabase.Input;
        }else
        {
            return new JSONObject().put("status", "Wrong input type given");
        }


        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);

        // add random value to value column
        Random rand = new Random();
        List<Instant> time_column = Arrays.asList(Instant.now());

        List<List<?>> values = new ArrayList<>();
        List<Integer> value_column = Arrays.asList(rand.nextInt(1000) + 1);
        values.add(value_column);

        TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(input_iri), values);

        tsClient.addTimeSeriesData(ts);

        devClient.updateTimestamps(Arrays.asList(input_iri));

        return new JSONObject().put("status", "Updated <" + input_iri + ">");
	}
}
