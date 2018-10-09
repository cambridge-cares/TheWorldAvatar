package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;

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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.CRSTransformer;


@WebServlet("/GetBuildingListFromRegion")
public class GetBuildingListFromRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
   
    public GetBuildingListFromRegion() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
		/*
		 * This agent takes region and city and returns a list of building IRIs
		 */
		String value = request.getParameter("value");
		String city = null;
		JSONObject region = new JSONObject();
		JSONObject input = new JSONObject();
		try {
			input = new JSONObject(value);
			region = input.getJSONObject("region"); // The name of the key should corresponds its own agent description 
		    city = input.getString("city");
		} catch (JSONException e1) {
			e1.printStackTrace();
		} 
		
		String myHost = request.getServerName();
		int myPort = request.getServerPort();
		String myPathBuildingList = "/JPS/buildings/fromregion";
		URIBuilder builderBuildingList;
		
		String upperx = null;
		String uppery = null;
		String lowerx = null;
		String lowery = null;
		try {
			 
 			String sourceCRS = CRSTransformer.EPSG_4326; // Google
			String targetCRS = CRSTransformer.EPSG_28992; // Berlin and Den Hague
	 
			double[] upperPointOld = new double[] {Double.parseDouble(region.getJSONObject("uppercorner").getString("upperx")),Double.parseDouble(region.getJSONObject("uppercorner").getString("uppery"))};
			double[] lowerPointOld = new double[] {Double.parseDouble(region.getJSONObject("lowercorner").getString("lowerx")),Double.parseDouble(region.getJSONObject("lowercorner").getString("lowery"))};
			
			double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
			double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
			
			upperx= String.valueOf(upperPointNew[0]);
			uppery= String.valueOf(upperPointNew[1]);
			lowerx= String.valueOf(lowerPointNew[0]);
			lowery= String.valueOf(lowerPointNew[1]);
 
		    System.out.println(upperx + "|" + lowerx + "|" + uppery + "|" + lowery); 
			 
			builderBuildingList = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath(myPathBuildingList)
					.setParameter("cityiri", city.trim())
					.setParameter("buildinglimit", "25")
					.setParameter("lowerx", lowerx) 
					.setParameter("lowery", lowery) 
					.setParameter("upperx", upperx) 
					.setParameter("uppery", uppery);
			String buildingList = executeGet(builderBuildingList);
			JSONObject result = new JSONObject();
			result.put("building", buildingList); // The result is wrapped into an JSONObject with key "building", which is indicated in its description. 
			response.getWriter().write(result.toString());
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
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
