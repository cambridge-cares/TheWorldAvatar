package uk.ac.cam.cares.jps.men.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
//import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;



public class TestMenAgent extends TestCase {

	private String getContextPathForJPSMen() {
		return "JPS_MEN//MENAgent";
	}
	
	private String getTransportationFile() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		return baseDir + "/testres/transportation/Jr_Transportation_simplified.owl"; // location of the owl file that contains information for the transportation system
		//return "http://www.jparksimulator.com/Jr_Transportation_simplified.owl";
	}
	
	public String getChemicalPlants() {
		
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
		return result2;
	}
	
	
	private String getCarbonTax() {
		return "50"; // location of the owl file that contains information for the transportation system
		
	}
	
	private String getInterestFactor() {
		return "1"; // location of the owl file that contains information for the transportation system
	}
		private String getAnnualCostFactor() {
			return "1."; // location of the owl file that contains information for the transportation system
		}
			private String getInternationalMarketPriceFactor() {
				return "1.05"; // location of the owl file that contains information for the transportation system
			}
				private String getInternationalMarketLowestPrice() {
					return "false"; // location of the owl file that contains information for the transportation system		
					}

	
	
	
	public void testCallAgent() throws IOException, URISyntaxException {


				
		AgentRequest request = new AgentRequest();

		
		// EIP --> one parameter
		Parameter param = new Parameter("transportationModes", getTransportationFile());
		request.getInputParameters().add(param);

		Parameter param2 = new Parameter("ChemicalPlants", getChemicalPlants());
		request.getInputParameters().add(param2);

		// additional Parameters --> five parameters
		Parameter param3 = new Parameter("CarbonTax", getCarbonTax());
		request.getInputParameters().add(param3);

		Parameter param4 = new Parameter("InterestFactor", getInterestFactor());
		request.getInputParameters().add(param4);

		Parameter param5 = new Parameter("AnnualCostFactor", getAnnualCostFactor());
		request.getInputParameters().add(param5);

		Parameter param6 = new Parameter("InternationalMarketPriceFactor", getInternationalMarketPriceFactor());
		request.getInputParameters().add(param6);

		Parameter param7 = new Parameter("InternationalMarketLowestPriceApplied", getInternationalMarketLowestPrice());
		request.getInputParameters().add(param7);
		

		// add output parameter with key = test

		AgentResponse resp = AgentCaller.callAgent(getContextPathForJPSMen(), request);

		// search for outputparameter with key = test
		Double ans1 = Double.valueOf((String) resp.getOutputParameters().get(0).getValue());
		Double ans2 = Double.valueOf((String) resp.getOutputParameters().get(1).getValue());
		Double ans3 = Double.valueOf((String) resp.getOutputParameters().get(2).getValue());
		Double ans4 = Double.valueOf((String) resp.getOutputParameters().get(3).getValue());
		Double ans5 = Double.valueOf((String) resp.getOutputParameters().get(4).getValue());
		Double ans6 = Double.valueOf((String) resp.getOutputParameters().get(5).getValue());
		Double ans7 = Double.valueOf((String) resp.getOutputParameters().get(6).getValue());
	

		//assertEquals(6.636958433E9, ans1, 1000.);
		//assertEquals(6.636902E9, ans2, 1000.);
		assertEquals(3.743276E9, ans3, 1000.);
		assertEquals(22539.661189, ans4, 1.);
		assertEquals(53.7127, ans5, 1.);
		assertEquals(2685.6338, ans6, 1.);
		assertEquals(1552885.9635, ans7, 1.);

	}
}
