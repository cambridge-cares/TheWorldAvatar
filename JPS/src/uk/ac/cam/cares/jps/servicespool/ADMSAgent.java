package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;

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

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;


@WebServlet("/ADMSAgent")
public class ADMSAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;

    public ADMSAgent() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		/*
		 * This agent takes: region, plantIRI, city, weatherstate and later emission stream 
		 * Then writes input files for adms : apl + met
		 * Then starts ADMS and generates output file test.levels.gst
		 * Later it should returns data in the form of Tabular JSON
		 */
		
 
		String myHost = request.getServerName();
		int myPort = request.getServerPort(); // Define the server name and port number without any hardcoding
		
 		String value = request.getParameter("value");
		try {
			JSONObject input = new JSONObject(value);
			input = new JSONObject(value);
			JSONObject region = input.getJSONObject("region");
		 	String plantIRI = input.getString("plant");
		 	String cityIRI = input.getString("city");
			JSONObject weather = input.getJSONObject("weatherstate");
			
			//================== request agent GetBuildingDataForSimulation ===============
			// It was previously an independent agent, currently it is merged with ADMSAgent
			JSONObject bundle = new JSONObject();
			bundle.put("city", cityIRI);
			bundle.put("plant", plantIRI);
			bundle.put("region", region);

			URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath("/JPS/GetBuildingDataForSimulation")
					.setParameter("value", bundle.toString());
			String buildingsInString = executeGet(builder);	 	
			System.out.println("=========================== buildingsInString ===========================");
			System.out.println(buildingsInString);
			System.out.println("=============================================================");
			
			//==============================================================================
			
			writeAPLFile(buildingsInString,plantIRI, region);
			writeMetFile(weather);
			
			// =================== Start ADMS when input files are written =======================
			String path = "/JPS/ADMSStarter";
			URIBuilder builder2 = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(path)
					.setParameter("targetFolder",AgentLocator.getPathToWorkingDir(this));
			String finalResult = executeGet(builder2);
			response.getWriter().write(finalResult.toString()); // TODO: ZXC Read the output file and then return JSON
			// ====================================================================================
			
		} catch (JSONException e) {
			e.printStackTrace();
		}

	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}


	public String writeMetFile(JSONObject weatherInJSON) {
		
			String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
			String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
			ArrayList<String> args = new ArrayList<String>();
			args.add("python");
			args.add("admsMetWriter.py"); 
			args.add(fullPath);
			args.add(weatherInJSON.toString().replace("\"", "$"));
			String result = CommandHelper.executeCommands(targetFolder, args);
			return result;
	}
	
	public String writeAPLFile(String buildingInString, String plantIRI, JSONObject regionInJSON)
	{
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsTest.py"); 
  		args.add(buildingInString.replace("\"", "'"));
 		args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all 
 		args.add(plantIRI.replace("\"", "'"));
 		args.add(fullPath.replace("/", "\\"));
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

	
}
