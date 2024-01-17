package uk.ac.cam.cares.derivation.asynexample;

import java.util.List;
import java.util.Objects;
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
 * This max value agent determines the maximum value given a list of random points.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = { MaxValueAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
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
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("MaxValueAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());

		// get the input from the KG
		List<String> pts = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.Point));

		if (Objects.isNull(pts) || pts.isEmpty()) {
			LOGGER.info("No points found in the input IRIs.");
			return;
		}

		// find the maximum value
		Integer maxvalue = sparqlClient.getExtremeValueInList(pts, true);

		// write the output triples to derivationOutputs
		String max_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(max_iri, SparqlClient.getRdfTypeString(SparqlClient.MaxValue));
		derivationOutputs.addTriple(max_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
		String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
		derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
		// instead of derivationOutputs.addTriple(sparqlClient.addValueInstance(max_iri, value_iri, maxvalue));
		// we use below two lines to test both addTriple(String, String, String) and addLiteral(String, String, Number)
		derivationOutputs.addTriple(max_iri, SparqlClient.getPropertyString(SparqlClient.hasValue), value_iri);
		derivationOutputs.addLiteral(value_iri, SparqlClient.getPropertyString(SparqlClient.numericalValue), maxvalue);
		LOGGER.info(
				"Created a new max value instance <" + max_iri + ">, and its value instance <" + value_iri + ">");
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Max Value Agent has started ----------------------\n");
		System.out.println("\n---------------------- Max Value Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		MaxValueAgent maxAgent = new MaxValueAgent(this.kbClient, Config.derivationInstanceBaseURL);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				maxAgent.monitorAsyncDerivations(Config.agentIriMaxValue, Config.periodAgentMaxValue);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentMaxValue, Config.periodAgentMaxValue, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Max Value Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Max Value Agent is monitoring derivation instance ----------------------\n");
	}
}
