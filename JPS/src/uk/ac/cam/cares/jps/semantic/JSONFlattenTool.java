package uk.ac.cam.cares.jps.semantic;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.building.CRSTransformer;

public class JSONFlattenTool {
	
	
	
	public static JSONObject flattenRegion (JSONObject input, boolean convert) throws JSONException {	
		
		JSONObject region = new JSONObject();
		JSONObject lowercorner = input.getJSONObject("region").getJSONObject("lowercorner");
		JSONObject uppercorner = input.getJSONObject("region").getJSONObject("uppercorner");
		String lowerx = lowercorner.getString("lowerx");
		String lowery = lowercorner.getString("lowery");
		String upperx = uppercorner.getString("upperx");
		String uppery = uppercorner.getString("uppery");
		String srsname = input.getJSONObject("region").getString("srsname");
		String targetCRS = CRSTransformer.EPSG_25833;
		double[] oldLowerPoint = new double[] {Double.parseDouble(lowerx),Double.parseDouble(lowery)};
		double[] newLowerPoint = CRSTransformer.transform(srsname, targetCRS, oldLowerPoint);

		double[] oldUpperPoint = new double[] {Double.parseDouble(upperx),Double.parseDouble(uppery)};
		double[] newUpperPoint = CRSTransformer.transform(srsname, targetCRS, oldUpperPoint);
		
		double xlower,ylower,xupper,yupper;
		
		if(convert) {
			 xlower = newLowerPoint[0];
			 ylower = newLowerPoint[1];
			 xupper = newUpperPoint[0];
			 yupper = newUpperPoint[1];
		}
		else {
			 xlower = Double.parseDouble(lowerx);
			 ylower = Double.parseDouble(lowery);
			 xupper = Double.parseDouble(upperx);
			 yupper = Double.parseDouble(uppery);
		}

		 
		region.put("lowerx", String.valueOf(xlower));
		region.put("lowery", String.valueOf(ylower));
		region.put("upperx", String.valueOf(xupper));
		region.put("uppery", String.valueOf(yupper));
		region.put("srsname", targetCRS);
 
		return region;
	}
}
