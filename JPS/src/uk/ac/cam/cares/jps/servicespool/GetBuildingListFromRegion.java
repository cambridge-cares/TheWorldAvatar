package uk.ac.cam.cares.jps.servicespool;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;



@WebServlet(urlPatterns ={"/GetBuildingListFromRegion","/BuildingsData"})

public class GetBuildingListFromRegion extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
	//private Logger logger = LoggerFactory.getLogger(GetBuildingListFromRegion.class);   
	
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(GetBuildingListFromRegion.class);
    }
	Logger logger = LoggerFactory.getLogger(GetBuildingListFromRegion.class);
    public GetBuildingListFromRegion() {
        super();
    }
    
	 @Override
	    protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	    	String path = request.getServletPath();
	    	 System.out.println("path= " + path);
	    	JSONObject result = new JSONObject();
	    	String city = requestParams.get("city").toString();
	    	JSONObject region = requestParams.getJSONObject("region");
			double upperx = Double.parseDouble(String.valueOf(region.getJSONObject("uppercorner").get("upperx")));
			double uppery = Double.parseDouble(String.valueOf(region.getJSONObject("uppercorner").get("uppery")));
			double lowerx = Double.parseDouble(String.valueOf(region.getJSONObject("lowercorner").get("lowerx")));
			double lowery = Double.parseDouble(String.valueOf(region.getJSONObject("lowercorner").get("lowery")));
			double[] sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};	
			double plantx=sourceXY[0];
			double planty=sourceXY[1];
			BuildingQueryPerformer performer = new BuildingQueryPerformer();
			List<String> buildingIRIs = performer.performQueryClosestBuildingsFromRegion(city.trim(), plantx, planty, 25, 
					Double.valueOf(lowerx), Double.valueOf(lowery), Double.valueOf(upperx),  Double.valueOf(uppery));
		if ("/GetBuildingListFromRegion".equals(path)) {
			result.put("building", buildingIRIs);

		} else if ("/BuildingsData".equals(path)) {
			SimpleBuildingData resultdata = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
			String argument = new Gson().toJson(resultdata);
			result=new JSONObject(argument);
		}
		 return result;
		 
		 
	 }

//	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
// 
//		/*
//		 * This agent takes region and city and returns a list of building IRIs
//		 */
//		String value = request.getParameter("query");
////		System.out.println("QUERY HERE: " + value);
//		try {
//			//JSONObject input = new JSONObject(value);
//			logger.info("query for building= "+value);
//			JSONObject input=AgentCaller.readJsonParameter(request);
//			String city = input.getString("city");
//			logger.info("city from getbuildinglistfromregion: " + city);
//			 
//			double[][] coords = convertCoordinate(input,city);//it is not used anymore
//			double lowerx = coords[1][0];//it is not used anymore, later is replaced
//			double lowery = coords[1][1];//it is not used anymore, later is replaced
//			double upperx = coords[0][0];//it is not used anymore, later is replaced
//			double uppery = coords[0][1];//it is not used anymore, later is replaced
//			double plantx = coords[2][0]; //it is not used anymore, later is replaced by the center point
//			double planty = coords[2][1];//it is not used anymore, later is replaced by the center point
//			
//				upperx=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("uppercorner").get("upperx"));
//				lowerx=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("lowercorner").get("lowerx"));
//				uppery = Double.parseDouble(""+input.getJSONObject("region").getJSONObject("uppercorner").get("uppery"));
//				lowery=Double.parseDouble(""+input.getJSONObject("region").getJSONObject("lowercorner").get("lowery"));
//				
//				//assume center point of the scene to pick building is middle point scope instead of the plant
//				double[] sourceXY = new double[] {(lowerx + upperx)/2, (lowery + uppery)/2};	
//				plantx=sourceXY[0];
//				planty=sourceXY[1];
//				
//			logger.info(lowerx + " || " + lowery + " || " + upperx + " || " + uppery +  " || " + plantx + " || " + planty);
//			
//			BuildingQueryPerformer performer = new BuildingQueryPerformer();
//			List<String> buildingIRIs = performer.performQueryClosestBuildingsFromRegion(city.trim(), plantx, planty, 25, 
//					Double.valueOf(lowerx), Double.valueOf(lowery), Double.valueOf(upperx),  Double.valueOf(uppery));			
//			JSONObject result = new JSONObject();
//			result.put("building", buildingIRIs); // The result is wrapped into an JSONObject with key "building", which is indicated in its description. 
//			response.getWriter().write(result.toString());
//		} catch (JSONException e) {
//			logger.error(e.getMessage(), e);
//			throw new JPSRuntimeException(e.getMessage(), e);
//		}	
//	}
//  
//	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
//		doGet(request, response);
//	}
	

	
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
		if ( cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
			targetCRS = CRSTransformer.EPSG_28992; // The Hague
		}else if(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
			targetCRS = CRSTransformer.EPSG_25833; // Berlin
		}else {
			targetCRS = CRSTransformer.EPSG_4326; // for Singapore and HK
		}
		double[] upperPointOld = new double[] {upperx,uppery};
		double[] lowerPointOld = new double[] {lowerx,lowery};
		double[] upperPointNew = CRSTransformer.transform(sourceCRS, targetCRS, upperPointOld);
		double[] lowerPointNew = CRSTransformer.transform(sourceCRS, targetCRS, lowerPointOld);
		result[0] = upperPointNew;
		result[1] = lowerPointNew;
		
		// TODO-AE URGENT the plant coordinates must be read from the powerplant OWL file
		// maybe add plant parameter to this agent for reading the coordinates or additional plant coordinate parameters
		//Below is not used currently because it is replaced by the center point of scope
		if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Berlin")) {
			result[2] = new double[] {699583.49, 532938.39};
		} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/The_Hague")) {
			result[2] = new double[] {79831., 454766.};
		} else if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore")) {
			result[2] = getCenterLatLon(lowerx, upperx, lowery, uppery);
		}
		return result;
	}
}
