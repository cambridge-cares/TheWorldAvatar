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
		String value = request.getParameter("value");
		
		try {
			JSONObject input = new JSONObject(value);
			String city = input.getString("city");
			 
			double[][] coords = convertCoordinate(input,city);
			double lowerx = coords[1][0];
			double lowery = coords[1][1];
			double upperx = coords[0][0];
			double uppery = coords[0][1];
			double plantx = coords[2][0];
			double planty = coords[2][1];

			logger.debug(lowerx + " | " + lowery + " | " + upperx + " | " + uppery +  " | " + plantx + " | " + planty);
			
			BuildingQueryPerformer performer = new BuildingQueryPerformer();
			List<String> buildingIRIs = performer.performQueryClosestBuildingsFromRegion(city.trim(), plantx, planty, 25, 
					Double.valueOf(lowerx), Double.valueOf(lowery), Double.valueOf(upperx),  Double.valueOf(uppery));
			
			JSONObject result = new JSONObject();
			result.put("building", buildingIRIs); // The result is wrapped into an JSONObject with key "building", which is indicated in its description. 
			response.getWriter().write(result.toString());
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}	
	}
  
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	public double[][] convertCoordinate(JSONObject region, String cityIRI) throws NumberFormatException, JSONException{
		
		logger.debug("city: " + cityIRI);
		double[][] result = new double[3][2];
		region = region.getJSONObject("region");
		double upperx = Double.parseDouble(region.getJSONObject("uppercorner").getString("upperx"));
		double uppery = Double.parseDouble(region.getJSONObject("uppercorner").getString("uppery"));
		double lowerx = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowerx"));
		double lowery = Double.parseDouble(region.getJSONObject("lowercorner").getString("lowery"));
		
		String sourceCRS = CRSTransformer.EPSG_4326; // default value
		if (!region.isNull("srsname")) {
			sourceCRS = region.getString("srsname");
		}
		
		String targetCRS = CRSTransformer.EPSG_28992; // The Hague
		double[] upperPointOld = new double[] {upperx,uppery};
		double[] lowerPointOld = new double[] {lowerx,lowery};
		double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
		double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
		result[0] = upperPointNew;
		result[1] = lowerPointNew;
		
		if(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
			// TODO-AE URGENT the plant coordinates must be read from the powerplant OWL file
			// maybe add plant parameter to this agent for reading the coordinates or additional plant coordinate parameters
			result[2] = new double[] {699583.49, 532938.39};
		}
		else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
			result[2] = new double[]{79831., 454766.};
		}
		return result;
	}
}
