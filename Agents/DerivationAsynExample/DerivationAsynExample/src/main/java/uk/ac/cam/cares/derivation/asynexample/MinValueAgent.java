package uk.ac.cam.cares.derivation.asynexample;

import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This min value agent determines the minimum value given a list of random points.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { MinValueAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
public class MinValueAgent extends DerivationAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(MinValueAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	static final String API_PATTERN = "/MinValueAgent";
	
	StoreClientInterface kbClient;
	SparqlClient sparqlClient;
	
	public MinValueAgent() {
		LOGGER.info("MinValueAgent is initialised.");
	}
	
	public MinValueAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("MinValueAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());

		// get the input from the KG
		String listOfRandomPoints_iri = derivationInputs
				.getIris(SparqlClient.getRdfTypeString(SparqlClient.ListOfRandomPoints)).get(0);
		
		// find the maximum value
		Integer minvalue = sparqlClient.getExtremeValueInList(listOfRandomPoints_iri, false);
		
		// write the output triples to derivationOutputs
		String min_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(min_iri, SparqlClient.getRdfTypeString(SparqlClient.MinValue));
		derivationOutputs.addTriple(min_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
		String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
		derivationOutputs.addTriple(sparqlClient.addValueInstance(min_iri, value_iri, minvalue));
		LOGGER.info(
				"Created a new min value instance <" + min_iri + ">, and its value instance <" + value_iri + ">");
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Min Value Agent has started ----------------------\n");
		System.out.println("\n---------------------- Min Value Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		MinValueAgent minAgent = new MinValueAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				minAgent.monitorAsyncDerivations(Config.agentIriMinValue, Config.periodAgentMinValue);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentMinValue, Config.periodAgentMinValue, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Min Value Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Min Value Agent is monitoring derivation instance ----------------------\n");
	}
}
