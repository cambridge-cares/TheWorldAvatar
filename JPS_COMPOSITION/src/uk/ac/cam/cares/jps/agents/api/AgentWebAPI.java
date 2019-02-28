package uk.ac.cam.cares.jps.agents.api;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.agents.ontology.JSONConverter;
import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.executor.ExecutionLayer;
import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.executor.ExecutorProcessor;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.OptimalPathSearcher;

@WebServlet(urlPatterns = {"/execute", "/describe", "/agents", "/agentdescriptions"})
public class AgentWebAPI extends HttpServlet {

	private static final long serialVersionUID = -860332795160247610L;
	Logger logger = LoggerFactory.getLogger(AgentWebAPI.class);
	
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		logger.info( "path=" + path);
		
		JSONObject jo = AgentCaller.readJsonParameter(req);
		
		
		String result = null;
		try {
			if ("/execute".equals(path)) {
				String composedAgentIRI = (String) jo.remove("agent");
				logger.info("composing agent=" + composedAgentIRI + ", query=" + jo);
				result = composeAndExecute(composedAgentIRI, jo.toString());
				logger.info("result = " + result);
				
			} else if ("/describe".equals(path)) {
				String agent = jo.getString("agent");
				result = describeInJson(agent);
				
			} else if ("/agents".equals(path)) {
				result = getAgentsInJson();
			} else if ("/agentdescriptions".equals(path)) {
				result = getAgentDescriptionsInJson();
			}
		} catch(Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
		AgentCaller.printToResponse(result, resp);
	}

	public Object[] compose(Service compositeAgent, String hostPort) 
			throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
	
		ServiceCompositionEngine engine = new ServiceCompositionEngine(compositeAgent, hostPort);
		
		if(!engine.start()) { // If the composite service is unsolvable, the start() function would return false. 
			return null;
		}
		 	
		Graph graph = engine.getGraph();
		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
		ArrayList<String> eliminationList = new ArrayList<String>();
		for (Service service : serviceEliminationList) { 
			eliminationList.add(service.getUri().toASCIIString());
		}
		
		showComposedAgents(graph, eliminationList);
		
		return new Object[] {graph, eliminationList};
	}
	
	public String composeAndExecute(Service compositeAgent, String jsonInputValues) 
				throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
	
		String hostPort = "http://" + KeyValueServer.get(IKeys.HOST) + ":" + KeyValueServer.get(IKeys.PORT);
		Object[] result = compose(compositeAgent, hostPort);
		
		if (result == null) {
			throw new JPSRuntimeException("no composition result for host=" + hostPort + ", " + "compositeAgent=" + compositeAgent.httpUrl);
		}

		Graph graph = (Graph) result[0];
		ArrayList<String> eliminationList = (ArrayList<String>) result[1];
		
		ExecutorProcessor processor = new ExecutorProcessor(FormatTranslator.convertGraphJavaClassTOJSON(graph), eliminationList);
		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
		ExecutorNew executor = new ExecutorNew(executionChain); 
	
		return executor.execute(new JSONObject(jsonInputValues));
	}
	
	public String composeAndExecute(String composedAgentIRI, String jsonInputValues) 
			throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
		
		Service compositeAgent = ServiceDiscovery.getInstance().getServiceByUri(composedAgentIRI);
		return composeAndExecute(compositeAgent, jsonInputValues);
}
	
	private void showComposedAgents(Graph graph, ArrayList<String> eliminationList) {
		
		logger.info("service pool:");
		for (Service current : graph.servicePool) {
			logger.info("" + current.uri);
		}
		
		logger.info("elimination list:");
		for (String current : eliminationList) {
			logger.info(current);
		}
	}
	
	public String describeInJson(String agent) throws URISyntaxException {

		//the following line only works for agents not for scenarios created from agents
		//Service service = ServiceDiscovery.getInstance().getServiceByUri(agent);	
		
		String owl = new QueryBroker().readFile(agent);
		List<Service> serviceList = new ServiceReader().parse(owl, null);	
		Service service = serviceList.get(0);
		
		return JSONConverter.convertToSimplifiedJson(service);
	}
	
	public String getAgentsInJson() throws JSONException {
		
		JSONWriter writer = new JSONStringer().object().key("result").array();
		
		ArrayList<Service> services = ServiceDiscovery.getInstance().getServices();
		for (Service current : services) {
			String agentName = current.getUri().toString();
			writer.value(agentName);
		}
		
		writer.endArray().endObject();
		
		return writer.toString();
	}
	
	public String getAgentDescriptionsInJson() throws JSONException {
		
		JSONWriter writer = new JSONStringer().object().key("result").array();
		
		ArrayList<Service> services = ServiceDiscovery.getInstance().getServices();
		for (Service current : services) {
			String description = JSONConverter.convertToSimplifiedJson(current);
			JSONObject jo = new JSONObject(description);
			writer.value(jo);
		}
		
		writer.endArray().endObject();
		
		return writer.toString();		
	}
}
