package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.net.URI;
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
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;


@WebServlet("/GetBuildingDataForSimulation")
public class GetBuildingDataForSimulation extends HttpServlet {
	private static final long serialVersionUID = 1L;

	// This agent takes selected region, plant IRI and CityIRI
	// returns BuildingDataForSimulation
	// It is currently merged with ADMSAgent. 
	
	
	public GetBuildingDataForSimulation() {
        super();
    }


	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
 
		String value = request.getParameter("query");	
		

		JSONObject input = null;
		try {
			input = new JSONObject(value);
		} catch (JSONException e1) {
			e1.printStackTrace();
		}
		try {		 
			String city = input.getString("city");			
			double[][] result = convertCoordinate(input, city);
			double plantx = result[2][0];
			double planty = result[2][1];
			double lowerx = result[1][0];
			double lowery = result[1][1];
			double upperx = result[0][0];
			double uppery = result[0][1];
			
			
			//system.out.println("================ result =================");
			//system.out.println(plantx + "|" + planty + "|" + lowerx + "|" + lowery + "|" + upperx + "|" + uppery);
			//system.out.println("=========================================");
 			String data = retrieveBuildingDataInJSON(city, plantx, planty, 25, lowerx, lowery, upperx, uppery);
			response.getWriter().write(data);

		} catch (JSONException e) {
			e.printStackTrace();
		}
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
 
	
	
	public String retrieveBuildingDataInJSON(String cityIRI, double plantx, double planty, int buildingLimit, double lowerx, double lowery, double upperx, double uppery) {
		List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(cityIRI, plantx, planty, buildingLimit, lowerx, lowery, upperx, uppery);
		SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(cityIRI, buildingIRIs);
		String argument = new Gson().toJson(result);
		return argument;
	}
	
	/* 
	 * This function queries the sparql endpoint to get the coordinates 
	 * @param plantIRI The IRI of the plant 
	 * @return double[] x and y of the plant in doubl
	 */
	public static double[] queryPlantXY(String plantIRI) throws JSONException {

		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/damecoolquestion/composition/query";
		// This specific endpoint loads kb of two plants. 
		
		String plantXYQuery =
					"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
				+   "PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\n"
				+ 	"PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>" + 
				"SELECT ?plantX ?plantY WHERE \n"
				+ "{"
				+ "		<%s>      space_and_time_extended:hasGISCoordinateSystem  ?coor . \n"
				+ "		?coor     space_and_time_extended:hasProjectedCoordinate_x ?x .  \n"
				+ "		?coor     space_and_time_extended:hasProjectedCoordinate_y ?y .  \n"
				+ "		?x        system:hasValue ?vx .  \n"
				+ "     ?vx   	  system:numericalValue ?plantX . \n"
				+ "     ?y		  system:hasValue ?vy .  \n"
				+ "     ?vy       system:numericalValue ?plantY . \n"
				+ "}";
		
		String finalQuery = String.format(plantXYQuery, plantIRI);

		URIBuilder builder;
		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setParameter("query", finalQuery)
				.setParameter("output", "json");
		
		String result = executeGet(builder);
		JSONObject resultInJSON = new JSONObject(result);
		JSONArray bindings = resultInJSON.getJSONObject("results").getJSONArray("bindings");
		Double X = 0.0,Y=0.0; 
		for(int i = 0; i < bindings.length();i++) {
			JSONObject binding = bindings.getJSONObject(i);
			Y = (binding.getJSONObject("plantY").getDouble("value"));
			X = (binding.getJSONObject("plantX").getDouble("value"));
		}
			
		return new double[]{X,Y};	
	}

	
	public static String executeGet(URIBuilder builder) {
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
	
	public double[] getCenterLatLon(double xmin, double xmax, double ymin, double ymax) {
		double xcenter = (xmax + xmin)/2;
		double ycenter = (ymax + ymin)/2;
				 
		return new double[] {xcenter, ycenter};
	}
	
	public double[][] convertCoordinate(JSONObject region, String cityIRI) throws NumberFormatException, JSONException{
		
		//system.out.println("city: " + cityIRI);
		double[][] result = new double[3][2];
		region = region.getJSONObject("region");
		String srsname = region.getString("srsname");
		double upperx = Double.parseDouble(region.getJSONObject("uppercorner").getString("upperx"));
		double uppery = Double.parseDouble(region.getJSONObject("uppercorner").getString("uppery"));
		double lowerx = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowerx"));
		double lowery = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowery"));
		if (!srsname.equalsIgnoreCase(CRSTransformer.EPSG_28992)) {
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
		}
		else {
			result[0] = new double[] {upperx,uppery};
			result[1] = new double[] {lowerx,lowery};
			if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
				result[2] = new double[] {79831, 454766};
			} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
				result[2] = new double[] {699583.49, 532938.39};
			} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
				result[2] = getCenterLatLon(lowerx, upperx, lowery, uppery);
			}
		}
		
		return result;
	}
	
}
