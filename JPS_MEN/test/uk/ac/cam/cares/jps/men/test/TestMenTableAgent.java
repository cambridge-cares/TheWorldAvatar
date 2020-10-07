package uk.ac.cam.cares.jps.men.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONArray;
import org.json.JSONObject;

//import org.apache.http.ParseException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.men.MenTableAgent;



public class TestMenTableAgent extends TestCase {

	private String getContextPathForJPSMen() {
		return "/JPS_MEN/MENTableAgent";
		//return "/MEN/MENAgent";
	}
	
	/*private String getTransportationFile() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		return baseDir + "/testres/transportation/Jr_Transportation_simplified.owl"; // location of the owl file that contains information for the transportation system
		
	}*/
	
	/*public String getChemicalPlants() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		List<String> result = new ArrayList<String>();
		String result2 = null;
		File[] files = new File("./testres/chemicalplant").listFiles();
		// If this pathname does not denote a directory, then listFiles() returns null.

		for (File file : files) {
			if (file.isFile() && !file.getName().equals("catalog-v001.xml")) {
				result.add(baseDir + "/testres/chemicalplant/" + file.getName());
				
			
			}
			
		}
		result2=String.join(",", result);
			 
		//return new Gson().toJson(result);
		 
		return result2;
	}
	*/
	
	private String getCarbonTax() {
		return "50"; // put hardcoded carbontax
		
	}
	
	private String getInterestFactor() {
		return "1";// put hardcoded interest
	}
	
	private String[] getAnnualCostFactor() {
		return new String[] {"1","10","50","100"}; // location of the owl file that contains information for the transportation system		
	}
	
	private String getInternationalMarketPriceFactor() {
			return "1.05"; // location of the owl file that contains international market price system
	}
	
	private String getInternationalMarketLowestPrice() {
		return "false"; // location of the owl file that contains information of international market lowest price application	
	}

	public void testCallMenTableAgent() throws IOException, URISyntaxException {
		
		JSONObject jo = new JSONObject();
		//jo.put("transportationModes", getTransportationFile());
		//jo.put("ChemicalPlants", getChemicalPlants());
		//jo.put("ecoindustrialpark", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIsland.owl");
		jo.put("CarbonTax", getCarbonTax());
		jo.put("InterestFactor", getInterestFactor());
		
		JSONArray annualCostFactor = new JSONArray();
		for (String current : getAnnualCostFactor()) {
			annualCostFactor.put(current);
		}

		jo.put("AnnualCostFactor", annualCostFactor);
		jo.put("InternationalMarketPriceFactor", getInternationalMarketPriceFactor());
		jo.put("InternationalMarketLowestPriceApplied", getInternationalMarketLowestPrice());
		
		System.out.println("request content in test agent= "+ jo);
		// add output parameter with key = test

		String resultAsString = AgentCaller.executeGetWithJsonParameter(getContextPathForJPSMen(), jo.toString());

		System.out.println("here is the reaching back of final result");
		System.out.println(resultAsString);
		
		System.out.println("it is connected");
	}
	/** validate input for TableAgent test
	 * 
	 * @throws IOException
	 * @throws URISyntaxException
	 */
	public void testValidInput() throws IOException, URISyntaxException {
		JSONObject jo = new JSONObject();
		jo.put("CarbonTax", getCarbonTax());
		jo.put("InterestFactor", getInterestFactor());JSONArray annualCostFactor = new JSONArray();
		for (String current : getAnnualCostFactor()) {
			annualCostFactor.put(current);
		}
		jo.put("AnnualCostFactor", annualCostFactor);
		jo.put("InternationalMarketPriceFactor", getInternationalMarketPriceFactor());
		jo.put("InternationalMarketLowestPriceApplied", getInternationalMarketLowestPrice());
		boolean tableBool = new MenTableAgent().validateInput(jo);
		assertTrue(tableBool);
	}
}
