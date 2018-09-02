package uk.ac.cam.cares.jps.servicespool;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.semantic.QueryWarehouse;
import uk.ac.cam.cares.jps.servicespool.test.TestBuildingData;


@WebServlet("/ADMSAgent")
public class ADMSAgent extends HttpServlet {
	private static final long serialVersionUID = 1L;

    public ADMSAgent() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// ADMSAgent takes the following parameters 
		
		// 1. Buildings with building parameters ... 
		// 2. Emission source ---> Emission coordinate + Emission Substance
		// 3. Weather in semantic 
		
		// The requester passes a whole bundle of data in the form of JSON, where the keys to be the IRI of something ... e.g. http://www.theworldavatar.com/Weather
		// Plan 1, get the JSON and convert it to rdf, the perform a query upon it. 
		// Plan 2, get the JSON and retrieve the info directly.
		
		// Lets do both of them and then discuss later.
		// Receive the IRI of the source emission and then make the query ... 
		// 
		String value = request.getParameter("value").replace("$", "#");
		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);
	
		JSONObject regionInJSON = QueryWarehouse.getRegionCoordinates(model);
		JSONObject weatherInJSON = QueryWarehouse.getWeatherData(model);
		String buildingsInString = QueryWarehouse.getBuildingData(model);
	 	String plantIRI = QueryWarehouse.getPlantIRI(model);
	 	 
		writeAPLFile(buildingsInString,plantIRI, regionInJSON);
		writeMetFile(weatherInJSON);
		
		String myHost = request.getServerName();
		int myPort = request.getServerPort();
		String path = "/JPS/ADMSStarter";
		
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(path)
				.setParameter("targetFolder",AgentLocator.getPathToWorkingDir(this));
		
		response.getWriter().write(executeGet(builder));
		response.getWriter().write("\nInput files are at : " + AgentLocator.getPathToWorkingDir(this));
		
		
	
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
			args.add(weatherInJSON.toString().replace("\"", "\\\""));
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
 		args.add(regionInJSON.toString().replace("\"", "'"));
 		args.add(plantIRI.replace("\"", "'"));
 		args.add(fullPath.replace("/", "\\"));
 	 
 		
  		String result = CommandHelper.executeCommands(targetFolder, args);
		return result;		
	}

	public String convertBuildingData(JSONObject buildings) {
 		Model model = ModelFactory.createDefaultModel();
		try {
			RDFDataMgr.read(model, new ByteArrayInputStream(buildings.toString().getBytes("UTF-8")), Lang.RDFJSON);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		return 	QueryWarehouse.getBuildingData(model);
	}

	public String executeGet(URIBuilder builder) {
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
