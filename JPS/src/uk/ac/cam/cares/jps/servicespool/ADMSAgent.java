package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;


@WebServlet("/ADMSAgent")
public class ADMSAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;
	
	private Logger logger = LoggerFactory.getLogger(ADMSAgent.class);

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		/*
		 * This agent takes: region, plantIRI, city, weatherstate and later emission stream 
		 * Then writes input files for adms : apl + met
		 * Then starts ADMS and generates output file test.levels.gst
		 * Later it should returns data in the form of Tabular JSON
		 */
		
 
		String myHost = request.getServerName();
		int myPort = request.getServerPort(); // Define the server name and port number without any hardcoding
		
 		String value = request.getParameter("query");
		try {
			JSONObject input = new JSONObject(value);
			input = new JSONObject(value);
			JSONObject region = input.getJSONObject("region");
			String cityIRI = input.getString("city");
			
			String plantIRI = null;
			if (!cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
				plantIRI = input.getString("plant"); //
			}
			
			JSONObject weather = input.getJSONObject("weatherstate");
			
			//================== request agent GetBuildingDataForSimulation ===============
			// It was previously an independent agent, currently it is merged with ADMSAgent
			JSONObject bundle = new JSONObject();
			bundle.put("city", cityIRI);
			
			if (!cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
				bundle.put("plant", plantIRI); // Why is this here? Does GetBuildingDataFromSimulation use it
			}
			bundle.put("region", region);

			URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath("/JPS/GetBuildingDataForSimulation")
					.setParameter("query", bundle.toString());
			String buildingsInString = executeGet(builder);	 	
			System.out.println("=========================== buildingsInString ===========================");
			System.out.println(buildingsInString);
			System.out.println("=============================================================");
			
			//==============================================================================
						
			//String srsname = region.getString("srsname");
			double upperx = Double.parseDouble(region.getJSONObject("uppercorner").getString("upperx"));
			double uppery = Double.parseDouble(region.getJSONObject("uppercorner").getString("uppery"));
			double lowerx = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowerx"));
			double lowery = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowery"));
			
			double[] sourceXY = null;
			if (!cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
				sourceXY = getPlantXY(plantIRI); //
			} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
				sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};
			}
			
			String newBuildingData = retrieveBuildingDataInJSON(cityIRI, sourceXY[0], sourceXY[1], lowerx, lowery, upperx, uppery);
			newBuildingData = newBuildingData.replace('\"', '\'');
			
			String srsname = region.getString("srsname");
			
			 
			String targetCRSName = CRSTransformer.EPSG_25833;
			if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.BERLIN_IRI)) {
				targetCRSName =  CRSTransformer.EPSG_28992;
			} else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.THE_HAGUE_IRI)) {
				targetCRSName = CRSTransformer.EPSG_28992;
			} // ???

			String sourceCRSName = CRSTransformer.EPSG_4326;

			System.out.println("============= src name ==============");
			System.out.println(srsname);
			if (srsname.equalsIgnoreCase("EPSG:28992")) {
				sourceCRSName = CRSTransformer.EPSG_28992;
//				writeAPLFile(newBuildingData, plantIRI, region);
			} else {
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
				writeAPLFile(newBuildingData,plantIRI, newRegion);
			}

 
				
 			//writeAPLFile(newBuildingData,plantIRI, newRegion);
			
			
			//writeAPLFile(buildingsInString,plantIRI, region);
			
			writeMetFile(weather);
			
			// =================== Start ADMS when input files are written =======================
			
			String targetFolder = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
			if(request.getServerName().contains("localhost")) {
				//uncomment if tested in kevin's computer
				startADMS(targetFolder);
			} else {
				startADMS(targetFolder);
			}
			JSONObject result = new JSONObject();
			result.put("folder", targetFolder);
			response.getWriter().write(result.toString()); // TODO: ZXC Read the output file and then return JSON
			// ====================================================================================
			
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
		}
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}


	public void writeMetFile(JSONObject weatherInJSON) {
		
			String fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
			String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
			
			ArrayList<String> args = new ArrayList<String>();
			args.add("python");
			args.add("admsMetWriter.py"); 
			args.add(fullPath);
			// TODO-AE replacing " by $, maybe better by ' as is done in method writeAPLFile
			args.add(weatherInJSON.toString().replace("\"", "$"));
			
			CommandHelper.executeCommands(targetFolder, args);
	}
	
	public String writeAPLFile(String buildingInString, String plantIRI, JSONObject regionInJSON) {
		String fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
		System.out.println("==================== full path ====================");
		System.out.println(fullPath);
		System.out.println("===================================================");
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTest.py"); 
  		args.add(buildingInString.replace("\"", "'"));
  		
 		args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all 
 		args.add(plantIRI.replace("\"", "'"));
 		args.add(fullPath);
 		// TODO-AE use PythonHelper instead of CommandHelper
  		String result = CommandHelper.executeCommands(targetFolder, args);		
		return result;		
	}

	public String executeGet(URIBuilder builder) { // TODO: ZXC: Put this function in utility
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	private String retrieveBuildingDataInJSON(String city, double plantx, double planty, double lowerx, double lowery, double upperx, double uppery) {
		
		logger.info("retrieveBuildingDataInJSON, city=" + city + ", plantx=" + plantx + ", planty=" + planty
				+ ", lowerx=" + lowerx + ", lowery=" + lowery + ", upperx=" + upperx + ", uppery=" + uppery);
		
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(city, plantx, planty, 25, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	private double[] getPlantXY(String plant) {
		// TODO-AE URGENT change hard-coded coordinates
		
		// "http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl";
		double plantx = 79831;
		double planty = 454766;
		
		if("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002".equals(plant)) {
			
			String sourceCRS = "";
			String targetCRS = "";
			
			double[] sourceCenter = new double[2];
			double[] targetCenter = new double[2];
			
			sourceCRS = CRSTransformer.EPSG_25833; // Berlin
			sourceCenter = new double[]{392825, 5819122};
			targetCRS = CRSTransformer.EPSG_28992; // The Hague
			targetCenter = CRSTransformer.transform(sourceCRS, targetCRS, sourceCenter);
			plantx = targetCenter[0];
			planty = targetCenter[1];
		} 
		
		return new double[] {plantx, planty};
	}
	
	private void startADMS(String targetFolder) {
		String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
		CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
	}

}
