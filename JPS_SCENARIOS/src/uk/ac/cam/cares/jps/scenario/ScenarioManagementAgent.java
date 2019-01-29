package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ScenarioKeys;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Operation;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

@WebServlet(urlPatterns = {"/scenariomanagement/*"})
public class ScenarioManagementAgent extends HttpServlet {

	private static final long serialVersionUID = 1733142247564226760L;
	private static Logger logger = LoggerFactory.getLogger(ScenarioManagementAgent.class);
	
	private static final String WORKING_DIR = AgentLocator.getPathToJpsWorkingDir() + "/JPS_SCENARIO";

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);

		String path = request.getPathInfo();
		logger.info("called for path=" + path);
		
		if ("/listagents".equals(path)) {
		
			
		} else if ("/listscenarios".equals(path)) {

			String result = listScenariosAndAgentsAsJsonFromFile();
			AgentCaller.printToResponse(result, response);
			
		} else {
			throw new JPSRuntimeException("unknown operation for scenario management agent");
		}
	}
	
	
	public static String getWorkingDir() {
		return WORKING_DIR;
	}
	
	public static void writeToFile(String content, String fileName) {
		FileOutputStream outputStream = null;
	    try {
		    outputStream = new FileOutputStream(fileName);
		    byte[] strToBytes = content.getBytes();
			outputStream.write(strToBytes);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			try {
				outputStream.close();
			} catch (IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}
	}
	
	public String getScenarioBucket(String scenarioName) {
		
		String scenarioBucket = ScenarioManagementAgent.getWorkingDir() + "/" + scenarioName;
		// TODO-AE SC URGENT copy-on-write not yet implemented
		// so far only copy-on-read is implemented and thus this method returns always the scenario file
		// for copy-on-write we can create the bucket later (instead of checking always whether the bucket already exists) - at least for read and query
		// for copy-on-write read and query have to return the original path or url (= attribute resource), for update always the scenario file has to be returned
		File directory = new File(scenarioBucket);
	    if (!directory.exists()){
	        directory.mkdirs();
	    }
		
	    return scenarioBucket;
	}
	
	public String listScenariosAndAgentsAsJsonFromFile() {
		String path = getWorkingDir() + "/xxxlist.json";
		return new QueryBroker().readFile(path);
	}
	
	public void writeListToFile() {
		JSONWriter writer = new JSONStringer().object().key("result").array();
		addAgents(writer);
		addScenarios(writer);
		writer.endArray().endObject();	
		
		String content = writer.toString();
		//String content = listAgentsAsJson();
		String path = getWorkingDir() + "/xxxlist.json";
		System.out.println(path);
		writeToFile(content, path);
	}
	
	public void addAgents(JSONWriter writer) {
		
		ArrayList<Service> services = ServiceDiscovery.getInstance().getServices();
		for (Service current : services) {
			addAgent(writer, current);
		}
	}
	
	private void addAgent(JSONWriter writer, Service service) {
		
		String name = service.getUri().toString();
		int i = name.lastIndexOf("/");
		name = name.substring(i+1);
		String type = service.isComposed()? "composed" : "agent";
		addEntry(writer, service.getUri().toString(), name, type);
		
		Operation op = service.getOperations().get(0);
		System.out.println(op.getHttpUrl());
		addOperation(writer, op.getHttpUrl());
		addParameterSection(writer, op, true);
		addParameterSection(writer, op, false);
		
		writer.endObject().endObject(); // both for addOperation
		writer.endArray().endObject();	// both for addEntry
	}
	
	private void addParameterSection(JSONWriter writer, Operation operation, boolean input) {
		List<String> params = new ArrayList<String>();
		
		Iterator<MessagePart> it = null;
		if (input) {
			params.add("hasInput");
			it = operation.getInputs().get(0).getMandatoryParts().iterator();
		} else {
			params.add("hasOutput");
			it = operation.getOutputs().get(0).getMandatoryParts().iterator();	
		}
		while (it.hasNext()) {
			MessagePart current = it.next();
			System.out.println("name=" + current.getName());
			params.add(current.getName());
			params.add(current.getType().toString());
		}
		String[] array = params.toArray(new String[0]);
		addParameterSection(writer, array);
	}
	
	public void addScenarios(JSONWriter writer) {
			
		List<String> list = getScenarioIRIs();
		for (String current : list) {
			
			int i = current.lastIndexOf('/');
			String name = current.substring(i+1);
			i = name.lastIndexOf('.');
			name = name.substring(0, i);
			addEntry(writer, current, name, "scenario");
			
			i = current.lastIndexOf('.');
			String path = current.substring(0, i);
			addOperation(writer, path + "/call");
			addParameterSection(writer, "hasInput", 
					ScenarioAgent.SCENARIO_AGENT, "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service", 
					ScenarioKeys.SCENARIO_AGENT_OPERATION, "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl");
			writer.endObject().endObject();
			
			addOperation(writer, path + "/read");
			addParameterSection(writer, "hasInput", 
					ScenarioKeys.SCENARIO_RESOURCE, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource");
			addParameterSection(writer, "hasOuput", 
					"", "http://www.w3.org/2001/XMLSchema#string");
			writer.endObject().endObject();
			
			addOperation(writer, path + "/query");
			addParameterSection(writer, "hasInput", 
					ScenarioKeys.SCENARIO_RESOURCE, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource",
					ScenarioKeys.QUERY_SPARQL_QUERY, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#SparqlQuery");
			addParameterSection(writer, "hasOuput", 
					"", "http://www.w3.org/2001/XMLSchema#string");
			writer.endObject().endObject();
			
			writer.endArray().endObject();
		}
	}
	
	private void addEntry(JSONWriter writer, String id, String name, String type) {
		writer.object();
		writer.key("id").value(id).key("name").value(name).key("type").value(type);
		writer.key("service").array();
	}
	
	private void addOperation(JSONWriter writer, String httpUrl) {
		writer.object().key("hasOperation").object();
		writer.key("hasHttpUrl").value(httpUrl);
	}
	
	private void addParameterSection(JSONWriter writer, String... s) {
		
		writer.key(s[0]).array();
		for (int i=1; i<s.length; i=i+2) {
			writer.object();
			writer.key("hasName").value(s[i]).key("hasType").value(s[i+1]);
			writer.endObject();
		}
		writer.endArray();
	}
	
	public List<String> getScenarioIRIs() {
		List<String> result = new ArrayList<String>();
		
		File dir = new File(getWorkingDir());
		for (File current : dir.listFiles()) {
			if (current.isFile()) {
				String iri = getServerAddress() + "/JPS_SCENARIO/scenario/" + current.getName();
				result.add(iri);
			}
		}
		
		return result;
	}
	
	public static String getServerAddress() {
		return "http://" + KeyValueServer.get(IKeys.HOST) + ":" + KeyValueServer.get(IKeys.PORT);
	}
	
	public static String getHttpUrl(String content) {
		// TODO-AE SC use SPARQL instead of String manipulation
		int start = content.indexOf("<msm:hasHttpUrl>") + "<msm:hasHttpUrl>".length();
		int end = content.indexOf("</msm:hasHttpUrl>");
		String httpUrl = content.substring(start, end);
		return httpUrl;
	}
	
	public void createScenarioDescription(String scenarioName, String agent) {
		
		int i = agent.lastIndexOf("/");
		String agentFileName = agent.substring(i+1);
		
		String content = new QueryBroker().readFile(agent);
		content = content.replaceAll("/kb/agents/"+agentFileName , "/JPS_SCENARIO/scenario/" + scenarioName + ".owl");
		
		// TODO-AE SC use SPARQL instead of String manipulation
//		int start = content.indexOf("<msm:hasHttpUrl>") + "<msm:hasHttpUrl>".length();
//		int end = content.indexOf("</msm:hasHttpUrl>");
//		String httpUrl = content.substring(start, end);
//		int lastslash = httpUrl.lastIndexOf("/");
//		String operation = httpUrl.substring(lastslash+1);
//		String newHttpUrl = getServerAddress() + "/JPS_SCENARIO/scenario/" + scenarioName + "/" + operation;
//		content = content.substring(0, start) + newHttpUrl + content.substring(end);
		

		String fullName = getWorkingDir() + "/" + scenarioName + ".owl";
		writeToFile(content, fullName);
	}
}
