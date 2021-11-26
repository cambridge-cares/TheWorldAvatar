package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.AsynAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class RNGAgent extends AsynAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(RNGAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	SparqlClient sparqlClient;
	
	public RNGAgent(StoreClientInterface storeClient) {
		super(storeClient);
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public List<String> setupJob(JSONObject requestParams) {
		List<String> createdInstances = new ArrayList<String>();
		
		// get the input from the KG
		String upperLimitIRI = requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.MaxValue.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace));
		Integer upperLimit = sparqlClient.getValue(upperLimitIRI);
		String lowerLimitIRI = requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.LowerLimit.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace));
		Integer lowerLimit = sparqlClient.getValue(lowerLimitIRI);
		String numberOfPointsIRI = requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.NumberOfPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace));
		Integer numberOfPoints = sparqlClient.getValue(numberOfPointsIRI);
		
		// generate a list of random points
		List<Integer> listOfRandomPoints = randomNumberGeneration(upperLimit, lowerLimit, numberOfPoints);
		
		// write the generated list of random points to the KG
		String listOfRandomPoints_iri = sparqlClient.createListOfRandomPoints(listOfRandomPoints);
		
		// respond with the created IRI
		createdInstances.add(listOfRandomPoints_iri);
		return createdInstances;
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
		
		RemoteStoreClient kbClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		RNGAgent rngAgent = new RNGAgent(kbClient);
		
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
}
