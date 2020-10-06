package uk.ac.cam.cares.jps.men.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.men.MenAgent;
//import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;



public class TestMenAgent extends TestCase {

	private String getContextPathForJPSMen() {
		return "JPS_MEN//MENAgent";
	}
	
	private String getTransportationFile() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		//return baseDir + "/testres/transportation/Jr_Transportation_simplified.owl"; // location of the owl file that contains information for the transportation system
		//return "http://www.jparksimulator.com/Jr_Transportation_simplified.owl";
	return	"http://www.jparksimulator.com/kb/sgp/jurongisland/MaterialTransportMode.owl";
	}
	
	public String getChemicalPlants() {
		
//		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		String baseDir = AgentLocator.getProperty("absdir.jpsdata.workingdir.jps_men");
		
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
		return result2;
	}
	
	
	private String getCarbonTax() {
		return "50.0"; // location of the owl file that contains information for the transportation system
		
	}
	
	private String getInterestFactor() {
		return "1.0"; // location of the owl file that contains information for the transportation system
	}
	
	private String getAnnualCostFactor() {
		return "0.02"; // location of the owl file that contains information for the transportation system
	}
	
	private String getInternationalMarketPriceFactor() {
		return "1.05"; // location of the owl file that contains information for the transportation system
	}
	
	private String getInternationalMarketLowestPrice() {
		return "true"; // location of the owl file that contains information for the transportation system		
	}
	
	public void testCallAgent() throws IOException, URISyntaxException {
		
		JSONObject jo = new JSONObject();
		jo.put("transportationmodes", getTransportationFile());
		//jo.put("chemicalplants", getChemicalPlants());
		jo.put("ecoindustrialpark", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIsland.owl");
		jo.put("carbontax", getCarbonTax());
		jo.put("interestfactor", getInterestFactor());
		jo.put("annualcostfactor", getAnnualCostFactor());
		jo.put("internationalmarketpricefactor", getInternationalMarketPriceFactor());
		jo.put("internationalmarketlowestpriceapplied", getInternationalMarketLowestPrice());
		System.out.println ("jsonoverall= "+jo.toString());
		String resultAsString = AgentCaller.executeGetWithJsonParameter(getContextPathForJPSMen(), jo.toString());
	
		
		JSONObject result = new JSONObject(resultAsString);
		Double ans4 = Double.valueOf(result.getString("totaltransportationcost"));//totalTransportationCost
		Double ans5 = Double.valueOf(result.getString("totalco2emission"));//totalCO2Emission
		Double ans6 = Double.valueOf(result.getString("totalco2emissioncost"));//totalCO2EmissionCost
		Double ans7 = Double.valueOf(result.getString("totalinstallationcost"));//totalInstallationCost

		assertEquals(22539.661189, ans4, 1.);
		assertEquals(53.7127, ans5, 1.);
		assertEquals(2685.6338, ans6, 1.);
		assertEquals(1552885.9635, ans7, 1.);
	}
	/** validate Input for MENAgent
	 * 
	 */
	public void testValidInputs() {
		JSONObject jo = new JSONObject();
		jo.put("transportationmodes", getTransportationFile());
		//jo.put("chemicalplants", getChemicalPlants());
		jo.put("ecoindustrialpark", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIsland.owl");
		jo.put("carbontax", "HOLO");
		jo.put("interestfactor", getInterestFactor());
		jo.put("annualcostfactor", getAnnualCostFactor());
		jo.put("internationalmarketpricefactor", getInternationalMarketPriceFactor());
		jo.put("internationalmarketlowestpriceapplied", getInternationalMarketLowestPrice());
		assertTrue(new MenAgent().validateInput(jo));
	}
}
