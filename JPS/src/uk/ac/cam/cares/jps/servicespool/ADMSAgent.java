package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;


@WebServlet("/ADMSAgent")
public class ADMSAgent extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	
	private Logger logger = LoggerFactory.getLogger(ADMSAgent.class);

	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		/*
		 * This agent takes: region, plantIRI, city, weatherstate and later emission stream 
		 * Then writes input files for adms : apl + met+bgd
		 * Then starts ADMS and generates output file test.levels.gst
		 * Later it should returns data in the form of Tabular JSON
		 */
		
 		String value = request.getParameter("query");
		try {
			JSONObject input = new JSONObject(value);
			JSONObject region = input.getJSONObject("region");
			String cityIRI = input.getString("city");
			
			

			JSONObject weather = input.getJSONObject("weatherstate");
			String precipitation=weather.getJSONObject("hasprecipation").getString("hasintensity");
			
			//================== request agent GetBuildingDataForSimulation ===============
			// It was previously an independent agent, currently it is merged with ADMSAgent
//			JSONObject bundle = new JSONObject();
//			bundle.put("city", cityIRI);
//			
//			if (!(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong"))) {
//				bundle.put("plant", plantIRI); // Why is this here? Does GetBuildingDataFromSimulation use it
//			}
//			bundle.put("region", region);

			//TODO-AE URGENT this called is not needed any more
//			URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
//					.setPath("/JPS/GetBuildingDataForSimulation")
//					.setParameter("query", bundle.toString());
//			String buildingsInString = executeGet(builder);	 
			
						
			//String srsname = region.getString("srsname");
			double upperx = Double.parseDouble(""+region.getJSONObject("uppercorner").get("upperx"));
			double uppery = Double.parseDouble(""+region.getJSONObject("uppercorner").get("uppery"));
			double lowerx = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowerx"));
			double lowery = Double.parseDouble(""+region.getJSONObject("lowercorner").get("lowery"));
			
			

			
			//String srsname = region.getString("srsname");
			
			 
			String sourceCRSName = CRSTransformer.EPSG_3857; //for default universal coordinate system that is use in the input browser 
			
			String targetCRSName = getTargetCRS(cityIRI);
			
			String dataPath = QueryBroker.getLocalDataPath();
			String fullPath = dataPath + "/JPS_ADMS";	//only applies for ship at the moment		
			
			String plantIRI = getSourceData(input, cityIRI);
			
			String newBuildingData = getBuildingData(cityIRI, upperx, uppery, lowerx, lowery);
			
			JSONObject newRegion = getNewRegionData(upperx, uppery, lowerx, lowery, targetCRSName, sourceCRSName);	
			
			JSONObject bkgjson=new JSONObject(input.getJSONObject("weatherstate")); //temporary only to test 1/7,right now it is hardcoded in python and don't take any from the bkgjson
				
				if (input.has("ship")) {
					QueryBroker broker = new QueryBroker();
					broker.put(fullPath + "/arbitrary.txt", "text to assign something arbitrary");
					writeAPLFileShip(newBuildingData, plantIRI, newRegion, targetCRSName,fullPath,precipitation);
					writeMetFile(weather,fullPath);
					writeBkgFile(bkgjson,fullPath);
				} else {
					writeAPLFile(newBuildingData,plantIRI, newRegion, targetCRSName);
					fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
					writeMetFile(weather,fullPath);
				}
			
			// =================== Start ADMS when input files are written =======================
			
			String agentIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
			String timestamp=null;
			if(timestamp==null) {
				timestamp=MetaDataAnnotator.getTimeInXsdTimeStampFormat(System.currentTimeMillis());
			}
			
//			if(request.getServerName().contains("localhost")) {
//				//uncomment if tested in kevin's computer
//				startADMS(fullPath);
//				MetaDataAnnotator.annotateWithTimeAndAgent(fullPath + "/test.levels.gst", timestamp, agentIRI);
//			} else {
				startADMS(fullPath);
				MetaDataAnnotator.annotateWithTimeAndAgent(fullPath + "/test.levels.gst", timestamp, agentIRI);
//			}
			
				JSONObject result = new JSONObject();
			result.put("folder", fullPath);
			response.getWriter().write(result.toString()); // TODO: ZXC Read the output file and then return JSON
			// ====================================================================================
			
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
		}
	}

	private String getTargetCRS(String cityIRI) {
		String targetCRSName;
		if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.THE_HAGUE_IRI)) {
			//sourceCRSName = CRSTransformer.EPSG_3857; //added currently 23/4
			targetCRSName =  CRSTransformer.EPSG_28992;
		} 
		else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.BERLIN_IRI)) {
			//sourceCRSName = CRSTransformer.EPSG_3857; 
			targetCRSName = CRSTransformer.EPSG_25833;
		}
		else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.SINGAPORE_IRI)) {
			//sourceCRSName = CRSTransformer.EPSG_3857;
			targetCRSName = CRSTransformer.EPSG_3414;
		}
		 else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.HONG_KONG_IRI)) {
			//sourceCRSName = CRSTransformer.EPSG_3857;
			targetCRSName = CRSTransformer.EPSG_2326;
		}
		 else {
			 targetCRSName = CRSTransformer.EPSG_3857;
		 }
		return targetCRSName;
	}

	private JSONObject getNewRegionData(double upperx, double uppery, double lowerx, double lowery,
			String targetCRSName, String sourceCRSName) {
		double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {lowerx, lowery});
		String lx = String.valueOf(p[0]);
		String ly = String.valueOf(p[1]);
		p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] {upperx, uppery});
		String ux = String.valueOf(p[0]);
		String uy = String.valueOf(p[1]);
 
		String regionTemplate = "{\r\n" + 
				"	\"uppercorner\":\r\n" + 
				"    	{\r\n" + 
				"        	\"upperx\" : \"%s\",\r\n" + 
				"            \"uppery\" : \"%s\"      	\r\n" + 
				"        },\r\n" + 
				"          \r\n" + 
				"     \"lowercorner\":\r\n" + 
				"     {\r\n" + 
				"       \"lowerx\" : \"%s\",\r\n" + 
				"       \"lowery\" : \"%s\"\r\n" + 
				"     }\r\n" + 
				"}";
		

		JSONObject newRegion  = new JSONObject(String.format(regionTemplate, ux,uy,lx,ly));
		return newRegion;
	}

	private String getBuildingData(String cityIRI, double upperx, double uppery, double lowerx, double lowery) {
		double[] sourceXY = null;
		
		if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong")) {
			sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};				
		} else {
			//sourceXY = getPlantXY(plantIRI); //CHANGE EASY TEMPORARILY TO SEE IF IT WORKS WITH NEW COORDINATE
			sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};	
		}
		
		String newBuildingData = retrieveBuildingDataInJSONOLD(cityIRI, sourceXY[0], sourceXY[1], lowerx, lowery, upperx, uppery);
		//String newBuildingData = retrieveBuildingDataInJSON(input);  //23/4 the new version that remove the duplicate query, but the composition must be changed first
		newBuildingData = newBuildingData.replace('\"', '\'');
		return newBuildingData;
	}

	private String getSourceData(JSONObject input, String cityIRI) {
		String plantIRI = null;
		JSONArray shipIRIs = null;
		if (!(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong"))) {
			plantIRI = input.getString("plant"); //
		} else {
			shipIRIs = input.getJSONArray("ship");
//				plantIRI = shipIRIs.toString();
			
			List<String> list = new ArrayList<String>();
			for (int i = 0; i < shipIRIs.length(); i++) {
				String shipIRI = shipIRIs.getString(i);
				list.add(shipIRI);
				//system.out.println(i);
				//system.out.println(shipIRI);
			}
			
			Gson g = new Gson();
			plantIRI = g.toJson(g.toJson(list.toArray()));
			//system.out.println("SHIP IRIS in String: " + plantIRI);
			
		}
		return plantIRI;
	}

	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		//doGet(request, response);
		String value = request.getParameter("query");
		JSONObject input = new JSONObject(value);
		JSONObject region = input.getJSONObject("region");
		String cityIRI = input.getString("city");
		JSONObject weather = input.getJSONObject("weatherstate");
		String precipitation = weather.getJSONObject("hasprecipation").getString("hasintensity");
		double upperx = Double.parseDouble("" + region.getJSONObject("uppercorner").get("upperx"));
		double uppery = Double.parseDouble("" + region.getJSONObject("uppercorner").get("uppery"));
		double lowerx = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowerx"));
		double lowery = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowery"));

		String sourceCRSName = CRSTransformer.EPSG_3857; // for default universal coordinate system that is use in the
															// input browser

		String targetCRSName = getTargetCRS(cityIRI);

		String dataPath = QueryBroker.getLocalDataPath();
		String fullPath = dataPath + "/JPS_ADMS"; // only applies for ship at the moment

		String plantIRI = getSourceData(input, cityIRI);

		String newBuildingData = getBuildingData(cityIRI, upperx, uppery, lowerx, lowery);

		JSONObject newRegion = getNewRegionData(upperx, uppery, lowerx, lowery, targetCRSName, sourceCRSName);

		JSONObject bkgjson = new JSONObject(input.getJSONObject("weatherstate")); // temporary only to test 1/7,right
																					// now it is hardcoded in python and
																					// don't take any from the bkgjson

		if (input.has("ship")) {
			QueryBroker broker = new QueryBroker();
			broker.put(fullPath + "/arbitrary.txt", "text to assign something arbitrary");
			writeAPLFileShip(newBuildingData, plantIRI, newRegion, targetCRSName, fullPath, precipitation);
			writeMetFile(weather, fullPath);
			writeBkgFile(bkgjson, fullPath);
		} else {
			writeAPLFile(newBuildingData, plantIRI, newRegion, targetCRSName);
			fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
			writeMetFile(weather, fullPath);
		}

		String agentIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
		String timestamp = null;
		if (timestamp == null) {
			timestamp = MetaDataAnnotator.getTimeInXsdTimeStampFormat(System.currentTimeMillis());
		}
		startADMS(fullPath);
		MetaDataAnnotator.annotateWithTimeAndAgent(fullPath + "/test.levels.gst", timestamp, agentIRI);

		JSONObject result = new JSONObject();
		result.put("folder", fullPath);
		response.getWriter().write(result.toString());
		
	}


	public void writeMetFile(JSONObject weatherInJSON,String fullPath) {
		
			// fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
			String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
			
			ArrayList<String> args = new ArrayList<String>();
			args.add("python");
			args.add("admsMetWriter.py"); 
			args.add(fullPath);
			// TODO-AE replacing " by $, maybe better by ' as is done in method writeAPLFile
			args.add(weatherInJSON.toString().replace("\"", "$"));
			
			CommandHelper.executeCommands(targetFolder, args);
	}
	
	public void writeBkgFile(JSONObject bkgInJSON,String fullPath) {
		
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsBgdWriter.py"); 
		args.add(fullPath);
		args.add(bkgInJSON.toString().replace("\"", "$"));
		
		CommandHelper.executeCommands(targetFolder, args);
}
	
	public String writeAPLFile(String buildingInString, String plantIRI, JSONObject regionInJSON,String targetCRSName) {
		String fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
		//system.out.println("==================== full path ====================");
		//system.out.println(fullPath);
		//system.out.println("===================================================");
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTest.py"); 
  		
		args.add(buildingInString.replace("\"", "'"));
  		logger.info(buildingInString.replace("\"", "'"));
  		  		
 		args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all
 		logger.info(regionInJSON.toString().replace("\"", "'"));
 		
 		args.add(plantIRI.replace("\"", "'"));
 		logger.info(plantIRI.replace("\"", "'"));
 		
 		args.add(fullPath);
 		logger.info(fullPath);
 		args.add(targetCRSName);
 		// TODO-AE use PythonHelper instead of CommandHelper
  		String result = CommandHelper.executeCommands(targetFolder, args);
  		logger.info("ARGUMENTS");
  		////system.out.println(args.toString());
  		logger.info(result);
		return result;		
	}
	
	public String writeAPLFileShip (String buildingInString, String plantIRI, JSONObject regionInJSON, String targetCRSName,String fullPath,String precipitation) {

		//fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
		
		//system.out.println("==================== full path ====================");
		//system.out.println(fullPath);
		//system.out.println("===================================================");
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTestShip.py"); 
  		args.add(buildingInString.replace("\"", "'"));
  		logger.info(buildingInString.replace("\"", "'"));
  		  		
 		args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all
 		logger.info(regionInJSON.toString().replace("\"", "'"));
// 		args.add(plantIRI.replace("\"", "'"));
 		args.add(plantIRI);
// 		//system.out.println(plantIRI.replace("\"", "'"));
 		logger.info(plantIRI);
 		args.add(fullPath);
 		logger.info(fullPath);
 		
 		args.add(targetCRSName);
 		logger.info(targetCRSName);
 		args.add(precipitation);
 		// TODO-AE use PythonHelper instead of CommandHelper
  		String result = CommandHelper.executeCommands(targetFolder, args);
  		logger.info("ARGUMENTS");
  		logger.info(args.toString());
  		logger.info("APL FOR SHIP WRITTEN= "+result);
		return result;		
	}
	
	private String retrieveBuildingDataInJSONOLD(String city, double plantx, double planty, double lowerx, double lowery, double upperx, double uppery) {
		
		logger.info("retrieveBuildingDataInJSON, city=" + city + ", plantx=" + plantx + ", planty=" + planty
				+ ", lowerx=" + lowerx + ", lowery=" + lowery + ", upperx=" + upperx + ", uppery=" + uppery);
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(city, plantx, planty, 25, lowerx, lowery, upperx, uppery);
		logger.info("building iris in ADMS Agent: " + buildingIRIs.toString());
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	private String retrieveBuildingDataInJSON(JSONObject input) {
		
		String city=input.getString("city");
		int buildingnum=input.getJSONArray("building").length();
		List<String> buildingIRIs = new ArrayList<String>();
		for(int a=0;a<buildingnum;a++) {
			buildingIRIs.add(input.getJSONArray("building").getString(a));
		}
		
		String buildinglist=String.valueOf(input.getJSONArray("building").length());
		System.out.println("what is building list??? "+buildinglist);
		System.out.println("element-0??? "+buildingIRIs.get(0));
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	
	private void startADMS(String targetFolder) {
		String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
		CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
	}

}
