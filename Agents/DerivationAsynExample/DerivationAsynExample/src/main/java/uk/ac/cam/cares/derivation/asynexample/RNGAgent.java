package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
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
@WebServlet(urlPatterns = { RNGAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
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
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("RNGAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());

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

		// skip all the rest if the number of points is less than 1
		// this will make the derivation no outputs
		if (numberOfPoints < 1) {
			return;
		}

		if (upperLimit >= lowerLimit) {
			// generate a list of random points
			List<Integer> listOfRandomPoints = randomNumberGeneration(upperLimit, lowerLimit, numberOfPoints);
			// write the generated output triples to derivationOutputs
			Map<String, String> ptIRIs = new HashMap<>();
			Map<String, Integer> valuesMap = new HashMap<>();
			listOfRandomPoints.stream().forEach(val -> {
				String pt_iri = SparqlClient.namespace + UUID.randomUUID().toString();
				derivationOutputs.createNewEntity(pt_iri, SparqlClient.getRdfTypeString(SparqlClient.Point));
				String val_iri = SparqlClient.namespace + UUID.randomUUID().toString();
				derivationOutputs.createNewEntity(val_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
				ptIRIs.put(pt_iri, val_iri);
				valuesMap.put(val_iri, val);
			});
			derivationOutputs
					.addTriple(sparqlClient.createListOfRandomPoints(ptIRIs, valuesMap));
		} else {
			throw new JPSRuntimeException("The provided upper limit " + upperLimit
					+ " is smaller than the provided lower limit " + lowerLimit);
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
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		RNGAgent rngAgent = new RNGAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				rngAgent.monitorAsyncDerivations(Config.agentIriRNG, Config.periodAgentRNG);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentRNG, Config.periodAgentRNG, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Random Number Generator (RNG) Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Random Number Generator (RNG) Agent is monitoring derivation instance ----------------------\n");
	}
}
