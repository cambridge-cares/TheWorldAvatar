package uk.ac.cam.cares.jps.semantic;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.building.CRSTransformer;

public class JSONFlattenTool {
	
	
	
	public static JSONObject flattenRegion (JSONObject input) throws JSONException {	
		
 
		
		JSONObject region = new JSONObject();
		
		System.out.println(input.toString());
		
		JSONObject lowercorner = input.getJSONObject("region").getJSONObject("lowercorner");
		JSONObject uppercorner = input.getJSONObject("region").getJSONObject("uppercorner");
		String lowerx = lowercorner.getString("lowerx");
		String lowery = lowercorner.getString("lowery");
		String upperx = uppercorner.getString("upperx");
		String uppery = uppercorner.getString("uppery");
		String srsname = input.getJSONObject("region").getString("srsname");
		
		System.out.println("srsname: " + srsname);
		 
		String targetCRS = CRSTransformer.EPSG_25833;

		double[] oldLowerPoint = new double[] {Double.parseDouble(lowerx),Double.parseDouble(lowery)};
		double[] newLowerPoint = CRSTransformer.transform(srsname, targetCRS, oldLowerPoint);

		double[] oldUpperPoint = new double[] {Double.parseDouble(upperx),Double.parseDouble(uppery)};
		double[] newUpperPoint = CRSTransformer.transform(srsname, targetCRS, oldUpperPoint);
		
		double xlower = newLowerPoint[0];
		double ylower = newLowerPoint[1];
		double xupper = newUpperPoint[0];
		double yupper = newUpperPoint[1];
		
//		System.out.println(xlower + "|" + ylower);
//		System.out.println(xupper + "|" + yupper);

		region.put("lowerx", String.valueOf(xlower));
		region.put("lowery", String.valueOf(ylower));
		region.put("upperx", String.valueOf(xupper));
		region.put("uppery", String.valueOf(yupper));
		region.put("srsname", srsname);
 
		return region;
	}
}
