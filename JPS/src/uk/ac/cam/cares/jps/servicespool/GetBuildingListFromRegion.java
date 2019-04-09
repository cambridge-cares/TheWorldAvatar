package uk.ac.cam.cares.jps.servicespool;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;


@WebServlet("/GetBuildingListFromRegion")
public class GetBuildingListFromRegion extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(GetBuildingListFromRegion.class);   
   
    public GetBuildingListFromRegion() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
 
		/*
		 * This agent takes region and city and returns a list of building IRIs
		 */
		String value = request.getParameter("query");
//		System.out.println("QUERY HERE: " + value);
		try {
			JSONObject input = new JSONObject(value);
			String city = input.getString("city");
			logger.info("city from getbuildinglistfromregion: " + city);
			 
			double[][] coords = convertCoordinate(input,city);
//			System.out.println("source point: " + Arrays.deepToString(coords));
			double lowerx = coords[1][0];
			double lowery = coords[1][1];
			double upperx = coords[0][0];
			double uppery = coords[0][1];
			double plantx = coords[2][0];
			double planty = coords[2][1];
			
			if(city.toLowerCase().contains("singapore")||city.toLowerCase().contains("hong")) {
				upperx=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("uppercorner").get("upperx"));
				lowerx=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("lowercorner").get("lowerx"));
				uppery = Double.parseDouble(""+input.getJSONObject("region").getJSONObject("uppercorner").get("uppery"));
				lowery=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("lowercorner").get("lowery"));
				double[] sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};	
				plantx=sourceXY[0];
				planty=sourceXY[1];
			}
			
//			System.out.println(lowerx + " | " + lowery + " | " + upperx + " | " + uppery +  " | " + plantx + " | " + planty);
			logger.info(lowerx + " || " + lowery + " || " + upperx + " || " + uppery +  " || " + plantx + " || " + planty);
			
			BuildingQueryPerformer performer = new BuildingQueryPerformer();
			List<String> buildingIRIs = performer.performQueryClosestBuildingsFromRegion(city.trim(), plantx, planty, 25, 
					Double.valueOf(lowerx), Double.valueOf(lowery), Double.valueOf(upperx),  Double.valueOf(uppery));
//			
			JSONObject result = new JSONObject();
			result.put("building", buildingIRIs); // The result is wrapped into an JSONObject with key "building", which is indicated in its description. 
			response.getWriter().write(result.toString());
//			response.getWriter().write(coords.toString());
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}	
	}
  
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	public double[] getCenterLatLon(double xmin, double xmax, double ymin, double ymax) {
		double xcenter = (xmax + xmin)/2;
		double ycenter = (ymax + ymin)/2;
				 
		return new double[] {xcenter, ycenter};
	}
	
	public double[][] convertCoordinate(JSONObject region, String cityIRI) throws NumberFormatException, JSONException {
		
		logger.debug("city: " + cityIRI);
		double[][] result = new double[3][2];
		region = region.getJSONObject("region");
		double upperx = Double.parseDouble(String.valueOf(region.getJSONObject("uppercorner").get("upperx")));
		double uppery = Double.parseDouble(String.valueOf(region.getJSONObject("uppercorner").get("uppery")));
		double lowerx = Double.parseDouble(String.valueOf(region.getJSONObject("lowercorner").get("lowerx")));
		double lowery = Double.parseDouble(String.valueOf(region.getJSONObject("lowercorner").get("lowery")));
		
		String sourceCRS = CRSTransformer.EPSG_4326; // default value
		if (!region.isNull("srsname")) {
			sourceCRS = region.getString("srsname");
		}
		
		String targetCRS = null;
		if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
			targetCRS = CRSTransformer.EPSG_28992; // The Hague
		} else {
			targetCRS = CRSTransformer.EPSG_3857; // for Singapore and HK
		}
		double[] upperPointOld = new double[] {upperx,uppery};
		double[] lowerPointOld = new double[] {lowerx,lowery};
		double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
		double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
		result[0] = upperPointNew;
		result[1] = lowerPointNew;
		
		if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
			// TODO-AE URGENT the plant coordinates must be read from the powerplant OWL file
			// maybe add plant parameter to this agent for reading the coordinates or additional plant coordinate parameters
			result[2] = new double[] {699583.49, 532938.39};
		} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
			result[2] = new double[] {79831., 454766.};
		} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
			result[2] = getCenterLatLon(lowerx, upperx, lowery, uppery);
		}
		return result;
	}
}
