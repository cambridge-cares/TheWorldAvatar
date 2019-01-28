package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
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

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ScenarioKeys;

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
		String content = listScenariosAndAgentsAsJson();
		String path = getWorkingDir() + "/xxxlist.json";
		System.out.println(path);
		writeToFile(content, path);
	}
	
	public String listScenariosAndAgentsAsJson() {
		
		JSONWriter writer = new JSONStringer().object().key("result").array();
		
		String ontAgentIri = "http://www.theworldavatar.com/ontology/OntoAgent#";
		
		List<String> list = getScenarioIRIs();
		int j = 0;
		for (String current : list) {
			int i = current.lastIndexOf('/');
			String name = current.substring(i+1);
			i = name.lastIndexOf('.');
			name = name.substring(0, i);
			writer.object();
			writer.key("id").value(current).key("name").value(name).key("type").value("scenario");
			
			// list operation
			writer.key("service").array();
			
			i = current.lastIndexOf('.');
			String path = current.substring(0, i);
			
			writer.object().key("hasOperation").object();
			writer.key("hasHttpUrl").value(path + "/call");
			addParameterSection(writer, "hasInput", 
					ScenarioAgent.SCENARIO_AGENT, "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service", 
					ScenarioKeys.SCENARIO_AGENT_OPERATION, "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl/hasHttpUrl");
			writer.endObject().endObject();
			
			writer.object().key("hasOperation").object();
			writer.key("hasHttpUrl").value(path + "/read");
			addParameterSection(writer, "hasInput", 
					ScenarioKeys.SCENARIO_RESOURCE, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource");
			addParameterSection(writer, "hasOuput", 
					"", "http://www.w3.org/2001/XMLSchema#string");
			writer.endObject().endObject();
			
			writer.object().key("hasOperation").object();
			writer.key("hasHttpUrl").value(path + "/query");
			addParameterSection(writer, "hasInput", 
					ScenarioKeys.SCENARIO_RESOURCE, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource",
					ScenarioKeys.QUERY_SPARQL_QUERY, "http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#SparqlQuery");
			addParameterSection(writer, "hasOuput", 
					"", "http://www.w3.org/2001/XMLSchema#string");
			writer.endObject().endObject();
			
			String[] operations = new String[] {"scenop1"};

			for (String currentOp : operations) {
				writer.object().key("hasOperation").object();
				writer.key("hasHttpUrl").value(path + "/"+currentOp);
				
				writer.key("hasInput").array();
				j++;
				for (i=1; i<=3; i++) {
					addParameter(writer, "inname"+j+"_"+i, "type"+j+"_"+i);
				}
				writer.endArray();
				
				writer.key("hasOutput").array();
				for (i=1; i<=2; i++) {
					addParameter(writer, "outname"+j+"_"+i, "type"+j+"_"+i);
				}
				writer.endArray();
				
				writer.endObject().endObject();
			}
			
			writer.endArray();
			
			// parameter details
//			writer.key("parameters").array();	
//			j++;
//			for (i=1; i<=3; i++) {
//				writer.object();
//				writer.key("name").value("name"+j+"_"+i).key("type").value("type"+j+"_"+i).key("input").value(true);
//				writer.endObject();
//			}
//			writer.endArray();
			
			writer.endObject();
		}
		
		writer.endArray().endObject();	
			
		return writer.toString();
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
	
	private void addParameter(JSONWriter writer, String name, String type) {
		writer.object();
		writer.key("hasName").value(name).key("hasType").value(type);
		writer.endObject();
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
