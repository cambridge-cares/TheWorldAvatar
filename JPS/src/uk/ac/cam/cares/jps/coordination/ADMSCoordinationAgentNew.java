package uk.ac.cam.cares.jps.coordination;

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

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

/**
 * Servlet implementation class ADMSCoordinationAgentNew
 */
@WebServlet("/ADMSCoordinationAgentNew")
public class ADMSCoordinationAgentNew extends HttpServlet {
	private static final long serialVersionUID = 1L;
      
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSCoordinationAgentNew() { 
        super();
     }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		
		String cityIRI = request.getParameter("cityIRI"); // ZHOUXIAOCHI:  Thy shall be semantic 
		String lowerx = request.getParameter("lowerx");   
		String lowery = request.getParameter("lowery");
		String upperx = request.getParameter("upperx"); 
		String uppery = request.getParameter("uppery");
		
		
		// ZHOUXIAOCHI: Thy shall be executed by the semantic agent executor
		
		String myHost = request.getServerName();
		int myPort = request.getServerPort();
		String myPathGenerateInput = "/JPS/CoordinatesAndCityToADMSOutput";
		
		
		URIBuilder builderGenerateInput = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPathGenerateInput)
				.setParameter("cityIRI", cityIRI)
				.setParameter("lowerx", lowerx )
				.setParameter("lowery", lowery)
				.setParameter("upperx", upperx)
				.setParameter("uppery", uppery);
		
		String result1  = executeGet(builderGenerateInput);
		

		String myPathADMSStarter = "/JPS/ADMSStarter";
		URIBuilder buildermyPathADMSStarter = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPathADMSStarter)
				.setParameter("targetFolder", result1);
	
		JSONObject result = new JSONObject();
		String ADMSOutput = executeGet(buildermyPathADMSStarter);
		
		String myPathBuildingList = "/JPS/buildings/fromregion";
		URIBuilder builderBuildingList = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPathBuildingList)
				.setParameter("cityiri", cityIRI)
				.setParameter("buildinglimit", "25")
				.setParameter("lowerx", lowerx )
				.setParameter("lowery", lowery)
				.setParameter("upperx", upperx)
				.setParameter("uppery", uppery);
		
		String buildingList = executeGet(builderBuildingList);
		response.getWriter().write(buildingList);
		
	}
 
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 		doGet(request, response);
	}

	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			//request.setHeader(HttpHeaders.ACCEPT, "application/sparql-results+json");
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
