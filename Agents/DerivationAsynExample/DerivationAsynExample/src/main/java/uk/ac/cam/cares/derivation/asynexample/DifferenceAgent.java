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
 * This difference agent takes two inputs as maximum value and minimum value and compute their difference.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { DifferenceAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
public class DifferenceAgent extends DerivationAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(DifferenceAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	static final String API_PATTERN = "/DifferenceAgent";
	
	StoreClientInterface kbClient;
	SparqlClient sparqlClient;
	
	public DifferenceAgent() {
		LOGGER.info("DifferenceAgent is initialised.");
	}
	
	public DifferenceAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("DifferenceAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());

		if (!derivationInputs.containsRdfType(SparqlClient.getRdfTypeString(SparqlClient.MaxValue)) ||
				!derivationInputs.containsRdfType(SparqlClient.getRdfTypeString(SparqlClient.MinValue))) {
			LOGGER.info("DifferenceAgent did not receive enough information.");
			return;
		}

		// get the input from the KG
		String maxvalue_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue)).get(0);
		String minvalue_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue)).get(0);
		
		// compute difference
		Integer diff = sparqlClient.getValue(maxvalue_iri) - sparqlClient.getValue(minvalue_iri);
		
		// write the output triples to derivationOutputs
		String difference_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(difference_iri, SparqlClient.getRdfTypeString(SparqlClient.Difference));
		derivationOutputs.addTriple(difference_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
		String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
		derivationOutputs.addTriple(sparqlClient.addValueInstance(difference_iri, value_iri, diff));
		LOGGER.info("Created a new calculated difference instance <" + difference_iri
				+ ">, and its value instance <" + value_iri + ">");
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Difference Agent has started ----------------------\n");
		System.out.println("\n---------------------- Difference Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		DifferenceAgent diffAgent = new DifferenceAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				diffAgent.monitorAsyncDerivations(Config.agentIriDifference, Config.periodAgentDifference);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentDifference, Config.periodAgentDifference, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Difference Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Difference Agent is monitoring derivation instance ----------------------\n");
	}
}
