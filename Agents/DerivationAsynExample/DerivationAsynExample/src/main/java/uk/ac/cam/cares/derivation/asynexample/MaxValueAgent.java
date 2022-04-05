package uk.ac.cam.cares.derivation.asynexample;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This max value agent determines the maximum value given a list of random points.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {MaxValueAgent.API_PATTERN})
public class MaxValueAgent extends DerivationAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(MaxValueAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	static final String API_PATTERN = "/MaxValueAgent";
	
	StoreClientInterface kbClient;
	SparqlClient sparqlClient;
	
	public MaxValueAgent() {
		LOGGER.info("MaxValueAgent is initialised.");
	}
	
	public MaxValueAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public DerivationOutputs processRequestParameters(DerivationInputs derivationInputs) {
		// get the input from the KG
		String listOfRandomPoints_iri = derivationInputs
				.getIris(SparqlClient.getRdfTypeString(SparqlClient.ListOfRandomPoints)).get(0);
		
		// find the maximum value
		Integer maxvalue = sparqlClient.getExtremeValueInList(listOfRandomPoints_iri, true);
		
		// create new instances in KG
		String createdMaxValue = sparqlClient.createMaxValue();
		sparqlClient.addValueInstance(createdMaxValue, maxvalue);

		// create DerivationOutputs instance
		DerivationOutputs derivationOutputs = new DerivationOutputs(
				SparqlClient.getRdfTypeString(SparqlClient.MaxValue), createdMaxValue);

		return derivationOutputs;
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Max Value Agent has started ----------------------\n");
		System.out.println("\n---------------------- Max Value Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
		}
		MaxValueAgent maxAgent = new MaxValueAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				maxAgent.monitorDerivation(Config.agentIriMaxValue);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentMaxValue, Config.periodAgentMaxValue, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Max Value Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Max Value Agent is monitoring derivation instance ----------------------\n");
	}

	public static void main(String[] args) {
		String endpoint = "http://localhost:53226/blazegraph/namespace/kb/sparql";
		StoreClientInterface storeClient = new RemoteStoreClient(endpoint, endpoint);
		String derivationInstanceBaseURL = "https://www.derivationasynexample.com/triplestore/repository/";
		MaxValueAgent agent = new MaxValueAgent(storeClient, derivationInstanceBaseURL);
		String agentIRI = "http://www.theworldavatar.com/kb/agents/Service__MaxValue#Service";
		agent.monitorDerivation(agentIRI);
	}
}
