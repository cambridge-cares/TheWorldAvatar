package uk.ac.cam.cares.goal.example;
import java.time.Instant;
import java.util.UUID;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import uk.ac.cam.cares.goal.framework.Goal;
import uk.ac.cam.cares.goal.framework.GoalClient;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {BinEmptyingAgent.URL_BINEMPTYINGAGENT})
public class BinEmptyingAgent extends DerivationAgent  {

    private static final long serialVersionUID = 1L;
    public static final String URL_BINEMPTYINGAGENT = "/BinEmptyingAgent";
    private static final Logger LOGGER = LogManager.getLogger(BinEmptyingAgent.class);


    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;

    GoalClient goalClient;
    public BinEmptyingAgent() {
        LOGGER.info("BinEmptyingAGent is initialised.");
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

        TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);

        System.out.println("something");

        String range_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.GoalRange)).get(0);
        String realstate_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.Weight)).get(0);
        String goal_iri = goalClient.getInputIRIGivenGoalRangeIRI(range_iri);

        Integer realStateValue = (timeSeriesClient.getLatestData(realstate_iri)).getValuesAsInteger(realstate_iri).get(0);

        Integer maxvalue = sparqlClient.getMaxValue(range_iri);
        Integer minvalue = sparqlClient.getMinValue(range_iri);

        String res_msg;
        if (realStateValue<maxvalue && realStateValue>minvalue)
        {
            res_msg = "Bin do nothing";
            LOGGER.info(res_msg);
        }
        else{
            res_msg = "Bin do something";
            LOGGER.info(res_msg);
        }

        // write the output triples to derivationOutputs

        String desiredstate_iri = SparqlClient.namespace+"_"+ UUID.randomUUID().toString();
        goalClient.addHasDesiredState(goal_iri,desiredstate_iri);
        derivationOutputs.createNewEntity(desiredstate_iri, SparqlClient.getRdfTypeString(SparqlClient.Weight));
        derivationOutputs.addTriple(desiredstate_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
        String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
        derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
        derivationOutputs.addTriple(sparqlClient.addValueInstance(desiredstate_iri, value_iri, 50));
        LOGGER.info("Created a new min value instance <" + desiredstate_iri + ">, and its value instance <" + value_iri + ">");
    }


}