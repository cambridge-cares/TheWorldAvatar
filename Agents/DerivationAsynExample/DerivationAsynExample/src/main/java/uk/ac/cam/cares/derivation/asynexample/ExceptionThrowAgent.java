package uk.ac.cam.cares.derivation.asynexample;

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

@WebServlet(urlPatterns = { ExceptionThrowAgent.API_PATTERN }, loadOnStartup = 1) // agent init() once deployed
public class ExceptionThrowAgent extends DerivationAgent {

	private static final Logger LOGGER = LogManager.getLogger(ExceptionThrowAgent.class);

	private static final long serialVersionUID = 1L;

	static final String API_PATTERN = "/ThorwExceptionAgent";
	static final String EXCEPTION_MESSAGE = "Throw an JPSRuntimeException for testing purpose.";

	StoreClientInterface kbClient;
	SparqlClient sparqlClient;

	public ExceptionThrowAgent() {
		LOGGER.info("ExceptionThrowAgent is initialised.");
	}

	public ExceptionThrowAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.kbClient = storeClient;
		this.sparqlClient = new SparqlClient(storeClient);
	}

	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
		LOGGER.debug("ExceptionThrowAgent received derivationInputs: " + derivationInputs.toString() + "for derivation: " + derivationInputs.getDerivationIRI());
        throw new JPSRuntimeException(EXCEPTION_MESSAGE);
	}

	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- ExceptionThrow Agent has started ----------------------\n");
		System.out.println("\n---------------------- ExceptionThrow Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();

		Config.initProperties();

		if (this.kbClient == null) {
			this.kbClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
			this.sparqlClient = new SparqlClient(this.kbClient);
			super.devClient = new DerivationClient(this.kbClient, Config.derivationInstanceBaseURL);
		}
		ExceptionThrowAgent diffAgent = new ExceptionThrowAgent(this.kbClient, Config.derivationInstanceBaseURL);

		exeService.scheduleAtFixedRate(() -> {
			try {
				diffAgent.monitorAsyncDerivations(Config.agentIriExceptionThrow, Config.periodAgentExceptionThrow);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentExceptionThrow, Config.periodAgentExceptionThrow, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- ExceptionThrow Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- ExceptionThrow Agent is monitoring derivation instance ----------------------\n");
	}
}
