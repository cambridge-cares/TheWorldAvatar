package uk.ac.cam.cares.jps.episode.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.episode.EpisodeAgent;
import uk.ac.cam.cares.jps.episode.WeatherAgent;

public class EpisodeAgentTest extends TestCase {
	
	RepositoryConnection con = WeatherAgent.repo.getConnection();
	
	public void testRunPeriodic() {
		
		//RepositoryConnection con = WeatherAgent.repo.getConnection();
		String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-001.owl#WeatherStation-001";
		String context2="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002";
		
		try {
			new WeatherAgent().executeFunctionPeriodically(con,context,context2);
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("it is updated");
	}
	
	public void testextract() {
		List<String[]>result=new WeatherAgent().extractAvailableContext(con);
		System.out.println("size="+result.size());
		System.out.println(result.get(0)[0]);
		System.out.println(result.get(1)[0]);
	}
	
	public void testDirectCallingWeather() {
		List<String[]>result=new WeatherAgent().extractAvailableContext(con);
		try {
			new WeatherAgent().executeFunctionPeriodically(con,result.get(0)[0],result.get(1)[0]);
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void testepisodeweatherinput() {
		String dataPath = QueryBroker.getLocalDataPath();
		String filename="mcwind_input_singapore_20191118.txt";
		List<String>stniri= new ArrayList<String>();
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-001.owl#WeatherStation-001");
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002");
		new EpisodeAgent().createWeatherInput(dataPath,filename,stniri);
	}
	
	public void testEpisodeReceptorInput() {
		String dataPath = QueryBroker.getLocalDataPath();
		String filename="receptor_input.txt";
		JSONObject jo = new JSONObject();

		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:32648")
					.key("lowercorner").object()
						.key("lowerx").value("331600.0")
						.key("lowery").value("119000.0").endObject()
					.key("uppercorner").object()
						.key("upperx").value("401600.0")
						.key("uppery").value("189000.0").endObject()
				.endObject()
				.endObject(); 
		JSONObject region= new JSONObject(jsonInput.toString());
		jo.put("regioninput",region);
		
		new EpisodeAgent().createReceptorFile(jo,dataPath,filename);
	}
	
	public void testtemplate() throws IOException {
		JSONObject jo = new JSONObject();
		String dataPath = QueryBroker.getLocalDataPath();
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:32648")
					.key("lowercorner").object()
						.key("lowerx").value("331600.0")
						.key("lowery").value("119000.0").endObject()
					.key("uppercorner").object()
						.key("upperx").value("401600.0")
						.key("uppery").value("189000.0").endObject()
				.endObject()
				.endObject(); 
		JSONObject region= new JSONObject(jsonInput.toString());
		jo.put("regioninput",region);
		//String b=new EpisodeAgent().modifyTemplate("run_file.asc",jo);
		new EpisodeAgent().createControlWeatherFile(region, dataPath, "run_file.asc") ;
	}
	
	public void xxxtestAgentCalling() {
		JSONObject jo = new JSONObject();
		jo.put("city", "singapore");
		String resp=AgentCaller.executeGetWithJsonParameter("JPS_EPISODE/SensorWeatherAgent", jo.toString());
		System.out.println("response= "+resp.toString());
	}

}
