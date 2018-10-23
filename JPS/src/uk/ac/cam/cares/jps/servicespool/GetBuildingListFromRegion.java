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
		JSONObject input = new JSONObject();
		try {
			input = new JSONObject(value);
		    city = input.getString("city");
		} catch (JSONException e1) {
			e1.printStackTrace();
		} 
		
		String myHost = request.getServerName();
		int myPort = request.getServerPort();
		String myPathBuildingList = "/JPS/buildings/fromregion";
		URIBuilder builderBuildingList;
		
	 
		try {
			 
			double[][] coords = convertCoordinate(input,city);
			String lowerx = String.valueOf(coords[1][0]);
			String lowery = String.valueOf(coords[1][1]);
			String upperx = String.valueOf(coords[0][0]);
			String uppery = String.valueOf(coords[0][1]);

			System.out.println(lowerx + " | " + lowery + " | " + upperx + " | " + uppery);
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
			System.out.println("============ Building List ============");
			System.out.println(buildingList);
			System.out.println("=======================================");

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
	
	public double[][] convertCoordinate(JSONObject region, String cityIRI) throws NumberFormatException, JSONException{
		
		System.out.println("city: " + cityIRI);
		double[][] result = new double[3][2];
		region = region.getJSONObject("region");
		double upperx = Double.parseDouble(region.getJSONObject("uppercorner").getString("upperx"));
		double uppery = Double.parseDouble(region.getJSONObject("uppercorner").getString("uppery"));
		double lowerx = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowerx"));
		double lowery = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowery"));
		
		if(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
			String sourceCRS = CRSTransformer.EPSG_4326; 
			String targetCRS = CRSTransformer.EPSG_28992; // The Hague
			double[] upperPointOld = new double[] {upperx,uppery};
			double[] lowerPointOld = new double[] {lowerx,lowery};
			double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
			double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
			result[0] = upperPointNew;
			result[1] = lowerPointNew;
			result[2] = new double[] {699583.49, 532938.39};
		}
		else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {			
			
			double plantx = 79831;
			double planty = 454766;
			
			String sourceCRS = CRSTransformer.EPSG_4326; 
			String targetCRS = CRSTransformer.EPSG_28992; // The Hague
			double[] upperPointOld = new double[] {upperx,uppery};
			double[] lowerPointOld = new double[] {lowerx,lowery};
			double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
			double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
			result[0] = upperPointNew;
			result[1] = lowerPointNew;
			result[2] = new double[]{plantx, planty};
		}
		return result;
	}

}
