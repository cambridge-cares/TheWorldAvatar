package uk.ac.cam.cares.goal.example;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import uk.ac.cam.cares.goal.framework.GoalAgent;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@WebServlet(urlPatterns = {BinEmptyingAgent.URL_BINEMPTYINGAGENT})
public class BinEmptyingAgent extends GoalAgent {

    private static final long serialVersionUID = 1L;
    public static final String URL_BINEMPTYINGAGENT = "/BinEmptyingAgent";
    private static final Logger LOGGER = LogManager.getLogger(BinEmptyingAgent.class);


    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;

    public BinEmptyingAgent() {
        LOGGER.info("BinEmptyingAGent is initialised.");
    }

    @Override
    public void init() throws ServletException {
        // initialise all clients
        Config.initProperties();
        this.storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
        this.sparqlClient = new SparqlClient(this.storeClient);
        super.devClient = new DerivationClient(this.storeClient, InitialiseInstances.goalInstanceBaseURL);
    }


    @Override
    public void processRequestParameters (DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

        LOGGER.info("Received request: " + derivationInputs.toString());

    }
}