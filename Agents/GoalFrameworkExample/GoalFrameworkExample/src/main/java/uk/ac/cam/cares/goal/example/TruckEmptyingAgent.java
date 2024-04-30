package uk.ac.cam.cares.goal.example;
import java.time.Instant;
import java.util.UUID;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.json.JSONObject;
import uk.ac.cam.cares.goal.actuator.Actuator;
import uk.ac.cam.cares.goal.framework.Goal;
import uk.ac.cam.cares.goal.framework.GoalClient;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {TruckEmptyingAgent.URL_TRUCKEMPTYINGAGENT})
public class TruckEmptyingAgent extends DerivationAgent  {

    private static final long serialVersionUID = 1L;
    public static final String URL_TRUCKEMPTYINGAGENT = "/TruckEmptyingAgent";
    private static final Logger LOGGER = LogManager.getLogger(TruckEmptyingAgent.class);
    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;
    GoalClient goalClient;
    public TruckEmptyingAgent() {
        LOGGER.info("TruckEmptyingAgent is initialised.");
    }


    @Override
    public void init() throws ServletException {
        // initialise all clients
        Config.initProperties();
        this.storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
        this.sparqlClient = new SparqlClient(this.storeClient);
        this.goalClient = new GoalClient(this.storeClient, InitialiseInstances.goalInstanceBaseURL);
        super.devClient = new DerivationClient(this.storeClient, InitialiseInstances.goalInstanceBaseURL);
    }

    @Override
    public void processRequestParameters (DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

        //declare variable
        TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
        String res_msg;

        //retrieve iri
        String range_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.GoalRange)).get(0);
        String realstate_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.TruckInput)).get(0);
        String goal_iri = goalClient.getInputIRIGivenGoalRangeIRI(range_iri);

        //retrieve realstate value
        Integer realStateValue = (timeSeriesClient.getLatestData(realstate_iri)).getValuesAsInteger(realstate_iri).get(0);
        //retrieve current desired value
        Integer oldDesiredState_value = sparqlClient.getValue(goalClient.getDesiredStateIRI(goal_iri));


        //retrieve min and max value from condition
        Integer maxvalue = sparqlClient.getMaxValue(range_iri);
        Integer minvalue = sparqlClient.getMinValue(range_iri);

        //instantiate new desired state
        String desiredstate_iri = SparqlClient.namespace+"_"+ UUID.randomUUID().toString();
        goalClient.addHasDesiredState(goal_iri,desiredstate_iri);
        derivationOutputs.createNewEntity(desiredstate_iri, SparqlClient.getRdfTypeString(SparqlClient.Weight));
        derivationOutputs.addTriple(desiredstate_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
        String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
        derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));

        //check if realstate within goalrange
        if (realStateValue<maxvalue && realStateValue>minvalue)
        {
            res_msg = "<TRUCK> Truck operating weight is within range, do nothing.";
            derivationOutputs.addTriple(sparqlClient.addValueInstance(desiredstate_iri, value_iri, oldDesiredState_value));
            LOGGER.info(res_msg);
        }
        else{
            res_msg = "<TRUCK> Truck operating weight is out of range, call actuator to empty bins and truck.";
            derivationOutputs.addTriple(sparqlClient.addValueInstance(desiredstate_iri, value_iri, 0));
            LOGGER.info(res_msg);
            callActuator(goal_iri);
        }
        LOGGER.info("Created a new desired value instance <" + desiredstate_iri + "> for truck, and its value instance <" + value_iri + ">");
    }

    /** HTTP POST Request to call Actuator
     * @param goal
     */
    private void callActuator(String goal){
        JSONObject requestParams = new JSONObject();

        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            //Actuator to remove weight from truck
            requestParams.put(Actuator.FUNCTION_KEY,Actuator.FUNCTION_EMPTYTRUCK_VALUE);
            HttpPost post = new HttpPost(InitialiseInstances.baseURL+Actuator.URL_ACTUATOR);
            StringEntity stringEntity = new StringEntity(requestParams.toString(), ContentType.APPLICATION_JSON);
            post.setEntity(stringEntity);

            try (CloseableHttpResponse httpResponse = httpClient.execute(post)) {
                if (httpResponse.getStatusLine().getStatusCode() != 200) {
                    String msg = "Failed to update goal <" + goal + "> with original request: "
                            + requestParams;
                    String body = EntityUtils.toString(httpResponse.getEntity());
                    LOGGER.error(msg);
                    throw new JPSRuntimeException(msg + " Error body: " + body);
                }
                String response = EntityUtils.toString(httpResponse.getEntity());
                LOGGER.debug("Obtained http response from agent: " + response);
            }
        } catch (Exception e) {
            LOGGER.error("Failed to update goal <" + goal + "> with original request: " + requestParams, e);
            throw new JPSRuntimeException("Failed to update goal <" + goal + "> with original request: "
                    + requestParams, e);
        }
    }
}
