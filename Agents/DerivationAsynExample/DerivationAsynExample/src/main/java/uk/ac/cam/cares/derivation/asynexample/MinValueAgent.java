package uk.ac.cam.cares.derivation.asynexample;

import java.util.ArrayList;
import java.util.List;
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

public class MinValueAgent extends AsynAgent {
	
	private static final Logger LOGGER = LogManager.getLogger(MinValueAgent.class);
	
	private static final long serialVersionUID = 1L;
	
	SparqlClient sparqlClient;

	public MinValueAgent(StoreClientInterface storeClient) {
		super(storeClient);
		this.sparqlClient = new SparqlClient(storeClient);
	}
	
	@Override
	public List<String> setupJob(JSONObject requestParams) {
		List<String> createdInstances = new ArrayList<String>();
		
		// get the input from the KG
		String listOfRandomPoints_iri = requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.ListOfRandomPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace));
		
		// find the minimum value
		Integer minvalue = sparqlClient.getExtremeValueInList(listOfRandomPoints_iri, false);
		
		// create new instances in KG
		createdInstances.add(sparqlClient.createMinValue());
		sparqlClient.addValueInstance(createdInstances.get(0), minvalue);
		
		return createdInstances;
	}
	
	@Override
	public void init() throws ServletException {
		LOGGER.info("\n---------------------- Min Value Agent has started ----------------------\n");
		System.out.println("\n---------------------- Min Value Agent has started ----------------------\n");
		ScheduledExecutorService exeService = Executors.newSingleThreadScheduledExecutor();
		
		Config.initProperties();
		
		RemoteStoreClient kbClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		MinValueAgent minAgent = new MinValueAgent(kbClient);
		
		exeService.scheduleAtFixedRate(() -> {
			try {
				minAgent.monitorDerivation(Config.agentIriMinValue);
			} catch (JPSRuntimeException e) {
				e.printStackTrace();
			}
		}, Config.initDelayAgentMinValue, Config.periodAgentMinValue, TimeUnit.SECONDS);
		LOGGER.info("\n---------------------- Min Value Agent is monitoring derivation instance ----------------------\n");
		System.out.println("\n---------------------- Min Value Agent is monitoring derivation instance ----------------------\n");
	}
}
