package uk.ac.cam.cares.jps.dispersion.test;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.dispersion.episode.CalculationUtils;
import uk.ac.cam.cares.jps.dispersion.episode.WeatherAgent;

public class WeatherAgentTest extends TestCase {
	String cityiri= "http://dbpedia.org/resource/Singapore";
	String cityiri2= "http://dbpedia.org/resource/Hong_Kong";
	String cityiri3= "http://dbpedia.org/resource/The_Hague";
	String cityiri4= "http://dbpedia.org/resource/Berlin";
	
	public void testextract() {
		//for sg
		double proclowx = Double.valueOf("11560879.832");
		double procupx = Double.valueOf("11563323.926");
		double proclowy = Double.valueOf("140107.739");
		double procupy = Double.valueOf("143305.896");
		//for hk
//		double proclowx = Double.valueOf("12706630.262");
//		double procupx = Double.valueOf("12708200.45");
//		double proclowy = Double.valueOf("2545539.172");
//		double procupy = Double.valueOf("2546850.028");
		double[] center = CalculationUtils.calculateCenterPoint(procupx, procupy, proclowx, proclowy);
		double[] centerPointConverted = CRSTransformer.transform(CRSTransformer.EPSG_3857,CRSTransformer.EPSG_4326,
				center);

		List<String[]>result=new WeatherAgent().extractAvailableContext(cityiri,centerPointConverted[0],centerPointConverted[1]);
		System.out.println("xconverted="+centerPointConverted[0]);
		System.out.println("yconverted="+centerPointConverted[1]);
		System.out.println("size="+result.size());
		System.out.println(result.get(0)[0]);
		System.out.println("name1= "+result.get(0)[1]);
		System.out.println(result.get(1)[0]);
		System.out.println("name2= "+result.get(1)[1]);
	}
	
	public void testDirectCallingWeather() {
		double proclowx = Double.valueOf("11560879.832");
		double procupx = Double.valueOf("11563323.926");
		double proclowy = Double.valueOf("140107.739");
		double procupy = Double.valueOf("143305.896");
		//for hk:
//		double proclowx = Double.valueOf("12706630.262");
//		double procupx = Double.valueOf("12708200.45");
//		double proclowy = Double.valueOf("2545539.172");
//		double procupy = Double.valueOf("2546850.028");
		double[] center = CalculationUtils.calculateCenterPoint(procupx, procupy, proclowx, proclowy);
		double[] centerPointConverted = CRSTransformer.transform(CRSTransformer.EPSG_3857,CRSTransformer.EPSG_4326,
				center);
		//List<String[]>result=new WeatherAgent().extractAvailableContext(cityiri2,centerPointConverted[0],centerPointConverted[1]);
		try {
			//new WeatherAgent().executeFunctionPeriodically(result,cityiri2);
			new WeatherAgent().executePeriodicUpdate(cityiri);
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void testAgentCallWeatherAgent() {
		double xmin=11560879.832;
		double xmax=11563323.926;
		double ymin=140107.739;
		double ymax=143305.896;
		
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
		   scope.put("srsname","EPSG:3857");
		   jo.put("region",scope);
		   jo.put("city",cityiri);
		  
		   String resp=AgentCaller.executeGetWithJsonParameter("JPS_DMS/SensorWeatherAgent", jo.toString());
		System.out.println("result= "+resp);
	}
	
	public void testinsertdataContext() {// should be used when the context want to be attached with some info

		String inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/sensor weather reference.json");
//		int []indexchosen= {0,1,2,3,4,5,6,7,8,9,10,11,12,13}; 
//		JSONObject current= new JSONObject(inputRef);
//		for(int x=1;x<=indexchosen.length;x++) {
//			String index="0"+x;
//			if(x<10) {
//				index="00"+x;
//			}
//			String name = current.getJSONObject("metadata").getJSONArray("stations").getJSONObject(indexchosen[x-1])
//					.get("name").toString();
//			String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-"+index+".owl#WeatherStation-"+index;
//			List<String>info= new ArrayList<String>();
//			info.add("http://dbpedia.org/resource/Singapore");
//			info.add(name);
//			new WeatherAgent().insertDataRepoContext(info,context);
//		}
		
		
		//for hongkong case
//		inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/1hrweatherhistory.csv");
//		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(inputRef);
//		readingFromCSV.remove(0);
//		for(int x=1;x<=readingFromCSV.size();x++) {
//			String index="0"+x;
//			if(x<10) {
//				index="00"+x;
//			}
//			String context="http://www.theworldavatar.com/kb/hkg/hongkong/WeatherStation-"+index+".owl#WeatherStation-"+index;
//			String name=readingFromCSV.get(x-1)[0];	
//			List<String>info= new ArrayList<String>();
//			info.add(cityiri2);
//			info.add(name);
//			new WeatherAgent().insertDataRepoContext(info,context);
//		}
		
		//for TheHague
		inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/TheHagueTemplate.csv");
		List<String[]> readingFromCSV2 = MatrixConverter.fromCsvToArray(inputRef);
		readingFromCSV2.remove(0);
		for(int x=1;x<=readingFromCSV2.size();x++) {
			String index="0"+x;
			if(x<10) {
				index="00"+x;
			}
			String context="http://www.theworldavatar.com/kb/nld/thehague/WeatherStation-"+index+".owl#WeatherStation-"+index;
			String name=readingFromCSV2.get(x-1)[0];	
			List<String>info= new ArrayList<String>();
			info.add(cityiri3);
			info.add(name);
			new WeatherAgent().insertDataRepoContext(info,context);
		}
		
		//for Berlin
		inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/BerlinTemplate.csv");
		List<String[]> readingFromCSV3 = MatrixConverter.fromCsvToArray(inputRef);
		readingFromCSV3.remove(0);
		for(int x=1;x<=readingFromCSV3.size();x++) {
			String index="0"+x;
			if(x<10) {
				index="00"+x;
			}
			String context="http://www.theworldavatar.com/kb/deu/berlin/WeatherStation-"+index+".owl#WeatherStation-"+index;
			String name=readingFromCSV3.get(x-1)[0];	
			List<String>info= new ArrayList<String>();
			info.add(cityiri4);
			info.add(name);
			new WeatherAgent().insertDataRepoContext(info,context);
		}

	}
	public void testtime() throws URISyntaxException {
		System.out.println(new WeatherAgent().provideCurrentTime());

	}
	
	public void testAccuweatherAPI() throws URISyntaxException {
		String result= WeatherAgent.getWeatherDataFromAccuweatherAPI("thehague");
		System.out.println(result);
	}

	
}
