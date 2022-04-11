package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Random;
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
 * This random number generator (RNG) agent generates a list of random points given inputs.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {RNGAgent.API_PATTERN})
public class RNGAgent extends DerivationAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(RNGAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	static final String API_PATTERN = "/RNGAgent";
	
	StoreClientInterface kbClient;
	SparqlClient sparqlClient;
	
	public RNGAgent() {
		LOGGER.info("RNGAgent is initialised.");
	}
	
	public RNGAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public DerivationOutputs processRequestParameters(DerivationInputs derivationInputs) {
		LOGGER.debug("RNGAgent received derivationInputs: " + derivationInputs.toString());

		// get the input from the KG
		String upperLimitIRI = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.UpperLimit)).get(0);
		LOGGER.debug(upperLimitIRI);
		Integer upperLimit = sparqlClient.getValue(upperLimitIRI);
		String lowerLimitIRI = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.LowerLimit)).get(0);
		LOGGER.debug(lowerLimitIRI);
		Integer lowerLimit = sparqlClient.getValue(lowerLimitIRI);
		String numberOfPointsIRI = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.NumberOfPoints))
				.get(0);
		LOGGER.debug(numberOfPointsIRI);
		Integer numberOfPoints = sparqlClient.getValue(numberOfPointsIRI);
		
		if (upperLimit >= lowerLimit) {
			// generate a list of random points
			List<Integer> listOfRandomPoints = randomNumberGeneration(upperLimit, lowerLimit, numberOfPoints);
			// write the generated list of random points to the KG
			String listOfRandomPoints_iri = sparqlClient.createListOfRandomPoints(listOfRandomPoints);
			
			// respond with the created IRI
			DerivationOutputs derivationOutputs = new DerivationOutputs(
					SparqlClient.getRdfTypeString(SparqlClient.ListOfRandomPoints), listOfRandomPoints_iri);
			return derivationOutputs;
		} else {
			return null;
		}
	}
	
	private List<Integer> randomNumberGeneration(int upperLimit, int lowerLimit, int numberOfPoints) {
		List<Integer> listOfRandomPoints = new ArrayList<Integer>();
		Random rand = new Random();
		for (int i = 0; i < numberOfPoints; i++) {
			listOfRandomPoints.add(rand.nextInt(upperLimit-lowerLimit)+lowerLimit);
		}
		return listOfRandomPoints;
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Random Number Generator (RNG) Agent has started ----------------------\n");
		System.out.println("\n---------------------- Random Number Generator (RNG) Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
		}
		RNGAgent rngAgent = new RNGAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				rngAgent.monitorDerivation(Config.agentIriRNG);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentRNG, Config.periodAgentRNG, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Random Number Generator (RNG) Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Random Number Generator (RNG) Agent is monitoring derivation instance ----------------------\n");
	}

	public static void main(String[] args) {
		String endpoint = "http://localhost:53180/blazegraph/namespace/kb/sparql";
		StoreClientInterface storeClient = new RemoteStoreClient(endpoint, endpoint);
		String derivationInstanceBaseURL = "https://www.derivationasynexample.com/triplestore/repository/";
		RNGAgent agent = new RNGAgent(storeClient, derivationInstanceBaseURL);
		String agentIRI = "http://www.theworldavatar.com/kb/agents/Service__RNG#Service";
		agent.monitorDerivation(agentIRI);
	}
}
