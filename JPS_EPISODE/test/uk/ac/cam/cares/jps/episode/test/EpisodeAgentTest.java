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
	
	public void testEpisodeEmissionInput() {
		double xmin=11560879/*.832*/;
		double ymin=140107/*.739*/;
		double xmax=11563323/*.926*/;
		double ymax=143305/*.896*/;
		
		//hk
//		 xmin=12693826.33/*.832*/;
//		 ymin=2535141.08 /*.739*/;
//		 xmax=12720831.57/*.926*/;
//		 ymax=2562311.02 /*.896*/;
		   JSONObject jo = new JSONObject();
		   
		   JSONObject scope = new JSONObject();
		   JSONObject low = new JSONObject();
		   JSONObject up = new JSONObject();
		   up.put("upperx", xmax);
		   up.put("uppery", ymax);
		   low.put("lowerx", xmin);
		   low.put("lowery", ymin);
		   scope.put("lowercorner", low);
		   scope.put("uppercorner", up);
		   jo.put("region",scope);
		   
		   System.out.println("json="+jo.toString());
		   String dataPath = QueryBroker.getLocalDataPath();
		   
		   String result = AgentCaller.executeGetWithURLAndJSON("http://www.theworldavatar.com:80/JPS_POSTGRESQL/getEntitiesWithinRegion", jo.toString());
		   System.out.println("result of the ship= "+result);
		   JSONObject shipdata = new JSONObject(result);
		   new EpisodeAgent().createEmissionInput(dataPath,"points_singapore_2019.csv",shipdata);
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
	
	public void testControlWeather() throws IOException {
		JSONObject jo = new JSONObject();
		String dataPath = QueryBroker.getLocalDataPath();
		List<String>stniri=new ArrayList<String>();
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002");
		stniri.add("http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-001.owl#WeatherStation-001");
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
		new EpisodeAgent().createControlWeatherFile(region, dataPath, "run_file.asc",stniri) ;
	}
	
	public void testControlTopology() throws IOException {
		JSONObject jo = new JSONObject();
		String dataPath = QueryBroker.getLocalDataPath();
		List<String>srtm=new ArrayList<String>();
		srtm.add("N01E105.tif");
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
		
		new EpisodeAgent().createControlTopologyFile(srtm,region, dataPath, "aermap.inp") ;
	}
	
	public void testhongkongaccuweather() throws URISyntaxException {
		JSONObject result=new WeatherAgent().extractedSingleDataFromAccuweather("pressure", "hongkong");
		System.out.println("result= "+result.toString());
	}
	public void xxxtestinsertdataContext() {// should be used when the context want to be attached with some info
		String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002";
		List<String>info= new ArrayList<String>();
		info.add("http://dbpedia.org/resource/singapore");
		info.add("clementi");
		new WeatherAgent().insertDataRepoContext(con,info,context);
	}
	
	public void xxxtestAgentCalling() {
		JSONObject jo = new JSONObject();
		jo.put("city", "singapore");
		String resp=AgentCaller.executeGetWithJsonParameter("JPS_EPISODE/SensorWeatherAgent", jo.toString());
		System.out.println("response= "+resp.toString());
	}

}
