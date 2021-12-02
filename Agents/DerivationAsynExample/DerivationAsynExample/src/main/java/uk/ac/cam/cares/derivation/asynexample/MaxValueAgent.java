package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.AsynAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This max value agent determines the maximum value given a list of random points.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {MaxValueAgent.API_PATTERN})
public class MaxValueAgent extends AsynAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(MaxValueAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	static final String API_PATTERN = "/MaxValueAgent";
	
	SparqlClient sparqlClient;
	
	public MaxValueAgent() {
		LOGGER.info("MaxValueAgent is initialised.");
	}
	
	public MaxValueAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
		super(storeClient, derivationInstanceBaseURL);
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public List<String> setupJob(JSONObject requestParams) {
		List<String> createdInstances = new ArrayList<String>();
		
		// get the input from the KG
		String listOfRandomPoints_iri = requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.ListOfRandomPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace));
		
		// find the maximum value
		Integer maxvalue = sparqlClient.getExtremeValueInList(listOfRandomPoints_iri, true);
		
		// create new instances in KG
		createdInstances.add(sparqlClient.createMaxValue());
		sparqlClient.addValueInstance(createdInstances.get(0), maxvalue);
		
		return createdInstances;
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Max Value Agent has started ----------------------\n");
		System.out.println("\n---------------------- Max Value Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		RemoteStoreClient kbClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		MaxValueAgent maxAgent = new MaxValueAgent(kbClient, Config.derivationInstanceBaseURL);
		
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
}
