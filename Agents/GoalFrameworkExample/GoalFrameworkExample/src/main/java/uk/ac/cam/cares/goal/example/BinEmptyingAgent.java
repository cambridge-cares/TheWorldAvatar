package uk.ac.cam.cares.goal.example;
import java.sql.Time;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.jena.sparql.syntax.ElementService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import uk.ac.cam.cares.goal.framework.GoalClient;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import javax.servlet.annotation.WebServlet;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {BinEmptyingAgent.URL_BINEMPTYINGAGENT})
public class BinEmptyingAgent extends JPSAgent {

    private static final long serialVersionUID = 1L;
    public static final String URL_BINEMPTYINGAGENT = "/BinEmptyingAgent";
    private static final Logger LOGGER = LogManager.getLogger(BinEmptyingAgent.class);


    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;

    public BinEmptyingAgent() {
        LOGGER.info("BinEmptyingAGent is initialised.");
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
        SparqlClient sparqlClient = new SparqlClient(storeClient);
        GoalClient goalClient = new GoalClient(storeClient,InitialiseInstances.goalInstanceBaseURL);
        TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);

        String range_iri = requestParams.get(goalClient.GOALRANGE_KEY).toString();
        String realstate_iri = requestParams.get(goalClient.REALSTATE_KEY).toString();

        Integer realStateValue = (timeSeriesClient.getLatestData(realstate_iri)).getValuesAsInteger(realstate_iri).get(0);

        Integer maxvalue = sparqlClient.getMaxValue(range_iri);
        Integer minvalue = sparqlClient.getMinValue(range_iri);

        if (realStateValue<maxvalue && realStateValue>minvalue)
        {
            String res_msg = "Bin do nothing";
            LOGGER.info(res_msg);
        }
        else{
            String res_msg = "Bin do something";
            LOGGER.info(res_msg);
        }

        String res_msg = "Bin do something";
        LOGGER.info(res_msg);
        JSONObject response = new JSONObject();
        response.put("status", res_msg);
        return response;
    }


}