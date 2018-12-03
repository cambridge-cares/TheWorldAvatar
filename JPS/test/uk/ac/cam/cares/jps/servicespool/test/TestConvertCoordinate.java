package uk.ac.cam.cares.jps.servicespool.test;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Test;

import uk.ac.cam.cares.jps.building.CRSTransformer;

public class TestConvertCoordinate {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws JSONException {
		String input = "{\"region\":{\"lowercorner\":{\"lowerx\":\"4.283710282240122\",\"lowery\":\"52.07126465153749\"},\"uppercorner\":{\"upperx\":\"4.2946107796766455\",\"uppery\":\"52.07983708183632\"},\"srsname\":\"EPSG:4326\"}}";

//		input = new JSONStringer().object().
//				key("region").object()
//					.key("lowercorner").object()
//						.key("lowerx").value("13.4074096")
//						.key("lowery").value("52.5177665").endObject()
//					.key("uppercorner").object()
//						.key("upperx").value("13.409")
//						.key("uppery").value("52.52").endObject()
//					.key("srsname").value("EPSG:4326")
//				.endObject()
//				.endObject().toString(); 		
		
		JSONObject region = new JSONObject(input);
		String city = "http://dbpedia.org/resource/The_Hague";
		//city = "http://dbpedia.org/resource/Berlin";
		double[][] result = convertCoordinate(region, city);
		for(int i = 0; i < result.length; i++) {
			double[] point = result[i];
			System.out.print(point[0]);
			System.out.print(" | ");
			System.out.print(point[1]);
			System.out.println("---------");
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
