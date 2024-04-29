package uk.ac.cam.cares.goal.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import javax.servlet.annotation.WebServlet;
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
@WebServlet(urlPatterns = {InputAgent.URL_INPUTAGENT})
public class InputAgent extends JPSAgent {
	private static final long serialVersionUID = 1L;
    private static String input_type;
    private static Integer input_value;
    public static final String INPUT_KEY = "input";
    public static final String INPUT_NUMBER_KEY = "value";
    public static final String TRUCK_INSTANCE_KEY = "truck";
    public static final String BIN_INSTANCE_KEY = "bin";
    public static final String URL_INPUTAGENT = "/InputAgent";

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
        SparqlClient sparqlClient = new SparqlClient(storeClient);
        DerivationClient devClient = new DerivationClient(storeClient, InitialiseInstances.goalInstanceBaseURL);

        //Declare local input_iri variable
        String input_iri;

        this.input_type = requestParams.getString(INPUT_KEY);
        this.input_value = requestParams.getInt(INPUT_NUMBER_KEY);

        //Retrieve input IRI from InstanceDatabase
        if (InstancesDatabase.Input == null || InstancesDatabase.InputTruck ==null) {
            InstancesDatabase.Input = sparqlClient.getInputIRI(sparqlClient.BinInput);
            InstancesDatabase.InputTruck = sparqlClient.getInputIRI(sparqlClient.TruckInput);
        }
        TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);

        //Add value based on input type
        List<Integer> value_column;
        List<Instant> time_column = Arrays.asList(Instant.now());

        if(input_type.equals(TRUCK_INSTANCE_KEY)){
            input_iri = InstancesDatabase.InputTruck;
            value_column = Arrays.asList(input_value);
        }else if (input_type.equals(BIN_INSTANCE_KEY)) {
            input_iri = InstancesDatabase.Input;
            value_column = Arrays.asList(input_value);
        }else{
            return new JSONObject().put("status", "Wrong input type given");
            }
        List<List<?>> values = new ArrayList<>();
        values.add(value_column);
        TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(input_iri), values);
        tsClient.addTimeSeriesData(ts);
        devClient.updateTimestamps(Arrays.asList(input_iri));

        return new JSONObject().put("status", "Updated <" + input_iri + ">");
	}
}
