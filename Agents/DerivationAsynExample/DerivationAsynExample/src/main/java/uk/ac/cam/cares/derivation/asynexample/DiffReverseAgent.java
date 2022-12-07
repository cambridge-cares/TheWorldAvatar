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
 * This difference reverse agent takes two inputs as minimum value and maximum value and compute their difference.
 * Also note that this agent delays the response for Config.delayAgentDiffReverse seconds to simulate a long running process,
 * so that we can test the asynchronous monitorAsyncDerivations behaviour of the DerivationAgent.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { DiffReverseAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
public class DiffReverseAgent extends DerivationAgent {

	private static final Logger LOGGER = LogManager.getLogger(DiffReverseAgent.class);

	private static final long serialVersionUID = 1L;

	static final String API_PATTERN = "/DiffReverseAgent";

	StoreClientInterface kbClient;
	SparqlClient sparqlClient;

	public DiffReverseAgent() {
		LOGGER.info("DiffReverseAgent is initialised.");
	}

	public DiffReverseAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}

	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("DiffReverseAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());
		LOGGER.debug("DiffReverseAgent will sleep for (" + Config.delayAgentDiffReverse + ") seconds before it runs.");
		try {
			TimeUnit.SECONDS.sleep(Config.delayAgentDiffReverse);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		LOGGER.debug("DiffReverseAgent is running now.");

		// get the input from the KG
		String maxvalue_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue)).get(0);
		String minvalue_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue)).get(0);

		// compute difference reverse as (min value - max value)
		Integer diff_reverse = sparqlClient.getValue(minvalue_iri) - sparqlClient.getValue(maxvalue_iri);

		// write the output triples to derivationOutputs
		String diff_reverse_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(diff_reverse_iri, SparqlClient.getRdfTypeString(SparqlClient.DifferenceReverse));
		derivationOutputs.addTriple(diff_reverse_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
		String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
		derivationOutputs.addTriple(sparqlClient.addValueInstance(diff_reverse_iri, value_iri, diff_reverse));
		LOGGER.info("Created a new calculated difference reverse instance <" + diff_reverse_iri
				+ ">, and its value instance <" + value_iri + ">");
	}

	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Diff Reverse Agent has started ----------------------\n");
		System.out.println("\n---------------------- Diff Reverse Agent has started ----------------------\n");
		// initialise scheduled executor service with 2 threads
		ScheduledExecutorService exeService = Executors.newScheduledThreadPool(2);

		Config.initProperties();

		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		DiffReverseAgent diffReverseAgent = new DiffReverseAgent(this.kbClient, Config.derivationInstanceBaseURL);

		exeService.scheduleAtFixedRate(() -> {
			exeService.execute(() -> {
				try {
					diffReverseAgent.monitorAsyncDerivations(Config.agentIriDiffReverse, Config.periodAgentDiffReverse);
				} catch (JPSRuntimeException e) {
					e.printStackTrace();
				}
			});
		}, Config.initDelayAgentDiffReverse, Config.periodAgentDiffReverse, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Diff Reverse Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Diff Reverse Agent is monitoring derivation instance ----------------------\n");
	}
}
