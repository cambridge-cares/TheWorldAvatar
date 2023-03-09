package uk.ac.cam.cares.jps.dispersion.test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
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
//		double proclowx = Double.valueOf("11560879.832");
//		double procupx = Double.valueOf("11564077.989");
//		double proclowy = Double.valueOf("140107.739");
//		double procupy = Double.valueOf("143305.896");
		//for hk
		double proclowx = Double.valueOf("12706653.262");
		double procupx = Double.valueOf("12711879.81");
		double proclowy = Double.valueOf("2545200.172");
		double procupy = Double.valueOf("2550426.72");
		double[] center = CalculationUtils.calculateCenterPoint(procupx, procupy, proclowx, proclowy);
		double[] centerPointConverted = CRSTransformer.transform(CRSTransformer.EPSG_3857,CRSTransformer.EPSG_4326,
				center);

		List<String[]>result=new WeatherAgent().extractAvailableContext(cityiri2,centerPointConverted[0],centerPointConverted[1]);
		System.out.println("xconverted="+centerPointConverted[0]);
		System.out.println("yconverted="+centerPointConverted[1]);
		System.out.println("size="+result.size());
		System.out.println(result.get(0)[0]);
		System.out.println("name1= "+result.get(0)[1]);
		System.out.println(result.get(1)[0]);
		System.out.println("name2= "+result.get(1)[1]);
		System.out.println("time= "+result.get(0)[2]);
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
		double xmin = Double.valueOf("11552101.832");
		double xmax = Double.valueOf("11572101.89");
		double ymin = Double.valueOf("131707.739");
		double ymax = Double.valueOf("151860.32");
		
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
		  
		   String resp=AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/SensorWeatherAgent", jo.toString());
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
		String result= WeatherAgent.getWeatherDataFromAccuweatherAPI("hongkong");
		System.out.println(result);
	}
	
	public void testextractcontext() {
		double[] center = CalculationUtils.calculateCenterPoint(11563323.926, 143305.896, 11560879.832, 140107.739);
		double[] centerPointConverted = CRSTransformer.transform("EPSG:3857",CRSTransformer.EPSG_4326,
				center);
		new WeatherAgent().extractAvailableContext(cityiri,centerPointConverted[0],centerPointConverted[1]);
	}
	
	public void testgovdata() {
		String weatherTemperature = WeatherAgent.getWeatherDataFromGovAPI("/v1/environment/air-temperature", null);
		JSONObject joTemperature = new JSONObject(weatherTemperature);//in celcius
		System.out.println(joTemperature);
	}
	
	public void testmakecsv() {
		String loc=cityiri2;
		 String querygraph = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT DISTINCT ?graph " 
					+ "{ graph ?graph " 
					+ "{ "
					+ "?graph j2:hasAddress <"+loc+"> ."
					+ "}" 
					+ "}Limit30";	
		List<String[]> listmap = queryFromClaudius(querygraph);
		//System.out.println("res= "+MatrixConverter.fromArraytoCsv(listmap));
		  List<String>time= new ArrayList<String>();
		  for(int x=0;x<listmap.size();x++) {
			  String context= listmap.get(x)[0];
			  String querydata = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
						+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
						+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
						+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
						+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
						+ "SELECT DISTINCT ?stnname ?xval ?yval ?proptimeval " 
						+ "{ graph <"+context+"> " 
						+ "{ "
						+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
		                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
		                + "?xent j2:hasValue ?vxent ."
		                + "?vxent   j2:numericalValue ?xval ."
		                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
		                + "?yent j2:hasValue ?vyent ."
		                + "?vyent   j2:numericalValue ?yval ."
						+ "?graph j2:enumerationValue ?stnname ."
						+ "?prop   j2:hasValue ?vprop ."
						+ " ?vprop   j6:hasTime ?proptime ."
						+ "?proptime   j6:inXSDDateTime ?proptimeval ."
						
						+ "}" 
						+ "}ORDER BY DESC(?proptimeval) Limit2";
			  
			  List<String[]> listsgstndata = queryFromClaudius(querydata); //it will give 30 data
			  String timelatest=listsgstndata.get(0)[3];
			  String timelatest2=listsgstndata.get(1)[3];
			  time.add(timelatest);		
			  time.add(timelatest2);	
		  }
		  Collections.sort(time, Collections.reverseOrder()); 
		  System.out.println("Sorted ArrayList "
                  + "in Descending order : "
                  + time);
		  List<String> time2 = time.stream().distinct().collect(Collectors.toList());
		  String timelatest=time2.get(0);
		  String timelatest2=time2.get(1);
		  System.out.println("time size="+time.size());
		  System.out.println("time latest= "+timelatest);
		  System.out.println("time latest2= "+timelatest2);
		  
		  List<String[]> listmapfinal=new ArrayList<String[]>();
		  for(int y=0;y<time2.size();y++) {
			  for(int x=0;x<listmap.size();x++) {
				  String context= listmap.get(x)[0];
				  String query = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
							+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
							+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
							+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
							+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
							+ "SELECT ?entity ?graph ?stnname ?xval ?yval " 
							+ "{ graph <"+context+"> " 
							+ "{ "
							+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
			                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
			                + "?xent j2:hasValue ?vxent ."
			                + "?vxent   j2:numericalValue ?xval ."
			                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
			                + "?yent j2:hasValue ?vyent ."
			                + "?vyent   j2:numericalValue ?yval ."
							+ "?graph j2:hasAddress <"+loc+"> ."
							+ "?graph j2:enumerationValue ?stnname ."
							+ "?entity j4:observes ?prop ."
							+ "?prop   j2:hasValue ?vprop ."
							+ " ?vprop   j6:hasTime ?proptime ."
							+ "?proptime   j6:inXSDDateTime \""+time2.get(y)+"\"^^xsd:dateTime ."				
							+ "}" 
							+ "}ORDER BY DESC(?proptimeval) Limit7";
				  List<String[]> listsgstndata = queryFromClaudius(query); //it will give 30 data
				  if(listsgstndata.size()==7) {
					  String[]res= {listsgstndata.get(0)[1],listsgstndata.get(0)[2],listsgstndata.get(0)[3],listsgstndata.get(0)[4],timelatest};
					  listmapfinal.add(res);
				  }
			  }
			  if(listmapfinal.size()>0) {
				  break;
			  }
			  
		  }
		  System.out.println("listmapfinal size= "+listmapfinal.size());
		  String mainstniri=listmapfinal.get(0)[0];
		  System.out.println("stniri 0= "+mainstniri);// (not considering the centre point closest yet)
	}

	private List<String[]> queryFromClaudius(String querygraph) {
		//String dataPath= QueryBroker.getLocalDataPath();
		
		Object[] a = KnowledgeBaseClient.createRequestUrl("http://www.theworldavatar.com/jps/data/weather", null, true);
		String requestUrl = "http://www.theworldavatar.com/jps/data/weather";
		JSONObject joparams = (JSONObject) a[1];
		if (joparams == null) {
			joparams = new JSONObject();
		}
		joparams.put(JPSConstants.QUERY_SPARQL_QUERY, querygraph);
		String resultfromrdf4j=Http.execute(Http.get(requestUrl, null, joparams));

		//String resultfromrdf4j = KnowledgeBaseClient.query("http://localhost:8080/jps/data/airquality", null, sensorinfo);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}
	
	public void testisupdate() {
		String timelatest="2020-06-10T11:28:47.388+08:00";
		boolean res=new WeatherAgent().isUpdateNeeded(timelatest);
		System.out.println("need update? "+res);
		
	}

	public void testvalidateInput() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException {
//		Any missing/invalid inputs should trigger an exception
//		The checkInput method will ensure that it throws a Runtime exception when the inputs are invalid. The method should throw a 
//		BadRequestException but the WeatherAgent object is currently causing it to throw a Runtime exception
		WeatherAgent wa = new WeatherAgent();
		Method validateInput = wa.getClass().getDeclaredMethod("validateInput",JSONObject.class);
		validateInput.setAccessible(true);
		
//		Create a valid input that should pass the test
		JSONObject jo = new JSONObject();
		JSONObject scope = new JSONObject();
		JSONObject low = new JSONObject();
		JSONObject up = new JSONObject();
		String keyUppercorner = "uppercorner";
		String keyLowercorner = "lowercorner";
		String keyUpperx = "upperx";
		String keyUppery = "uppery";
		String keyLowerx = "lowerx";
		String keyLowery = "lowery";
		String keyRegion = "region";
		String keyCity = "city";
		scope.put(keyUppercorner, up);
		scope.put(keyLowercorner, low);
		double xmin = Double.valueOf("11552101.832");
		double xmax = Double.valueOf("11572101.89");
		double ymin = Double.valueOf("131707.739");
		double ymax = Double.valueOf("151860.32");
		up.put(keyUpperx,xmax);
		up.put(keyUppery,ymax);
		low.put(keyLowerx,xmin);
		low.put(keyLowery,ymin);
		
		jo.put(keyRegion,scope);
		jo.put(keyCity,cityiri);
		assertTrue(checkInput(wa,validateInput,jo));
		
//		Now each key in the input is removed, toggled to empty and to the wrong type to trigger an exception
//		Begin with the region key
		jo.remove(keyRegion);
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyRegion,"");
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyRegion,"abc");
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyRegion,scope);
		assertTrue(checkInput(wa,validateInput,jo));
		
//		City IRI. 
		jo.remove(keyCity);
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyCity,"");
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyCity,"abc");
		assertFalse(checkInput(wa,validateInput,jo));
		jo.put(keyCity,cityiri);
		assertTrue(checkInput(wa,validateInput,jo));
		
//		Coordinates
		up.remove(keyUpperx);
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUpperx,"");
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUpperx,"abc");
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUpperx,xmax);
		assertTrue(checkInput(wa,validateInput,jo));
		
		up.remove(keyUppery);
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUppery, "");
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUppery, "abc");
		assertFalse(checkInput(wa,validateInput,jo));
		up.put(keyUppery, ymax);
		assertTrue(checkInput(wa,validateInput,jo));
		
		low.remove(keyLowerx);
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowerx, "");
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowerx, "abc");
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowerx, xmin);
		assertTrue(checkInput(wa,validateInput,jo));
		
		low.remove(keyLowery);
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowery, "");
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowery, "abc");
		assertFalse(checkInput(wa,validateInput,jo));
		low.put(keyLowery, ymin);
		assertTrue(checkInput(wa,validateInput,jo));
		
//		Optional srsname
//		Empty srsname is ok
		String keySrsname = "srsname";
		scope.put(keySrsname,"");
		assertTrue(checkInput(wa,validateInput,jo));
		scope.remove(keySrsname);
		assertTrue(checkInput(wa,validateInput,jo));
		scope.put(keySrsname,"abc");
		assertFalse(checkInput(wa,validateInput,jo));
		scope.put(keySrsname,"EPSG:3857");
		assertTrue(checkInput(wa,validateInput,jo));
	}
	
	private Boolean checkInput(WeatherAgent wa,Method validateInput,JSONObject jo) throws IllegalAccessException, IllegalArgumentException {
//		This method is used in the testvalidateInput method, returns true if all inputs are present
		try {
            validateInput.invoke(wa, jo);
            return true;
        } catch (InvocationTargetException e) {
//        	This should be modified to BadRequestException once the necessary fix is done in the JPSHttpServlet class
            assertEquals("javax.ws.rs.BadRequestException", e.getCause().getCause().getStackTrace()[14].getClassName());
            return false;
        }
	}
}
