package uk.ac.cam.cares.jps.men;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;

@WebServlet(urlPatterns = {"/MENTableAgent"})

public class MenTableAgent extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	Logger logger = LoggerFactory.getLogger(MenTableAgent.class);
	
	private String getContextPathForJPSMen() {
		return "JPS_MEN";
		//return "/MEN/MENAgent";
	}
	
	private String getAnnualCostFactor(int x) {
		String ACF[]= {"1.0","0.1","0.02","0.01"}; //from hard coded 1 year, 10 year, 50 year, 100 year
		return ACF[x]; 
	}
	
	private String getTransportationFile() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		return baseDir + "/testres/transportation/Jr_Transportation_simplified.owl"; // location of the owl file that contains information for the transportation system
		
	}
	
	public String getChemicalPlants() {
		
		String baseDir = AgentLocator.getProperty("absdir.jps_men");
		
		List<String> result = new ArrayList<String>();
		String result2 = null;
		//File[] files = new File("./testres/chemicalplant").listFiles();
		File[] files = new File(baseDir+"/testres/chemicalplant").listFiles();
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
	
	@Override
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		logger.info("MEN_Table Agent start");
		request.setCharacterEncoding("UTF-8");
		AgentRequest agentRequest = AgentCaller.getAgentRequest(request);
		
		
		String jsonString = request.getParameter("listOfInputs");
		
		System.out.println("jsonstring= "+jsonString);
		
		
		
		// read form fields
		String carbontax = jsonString.replaceAll("[()\\[\\]]", "").split(",")[0];
		String interestfactor = jsonString.replaceAll("\\\\[\\\\]","").split(",")[1];
		String intmarketpricefactor = jsonString.replaceAll("\\\\[\\\\]","").split(",")[2];
		String intmarketlowestprice = jsonString.replaceAll("[()\\[\\]]", "").split(",")[3];
		
		//from the test
//		String carbontax = agentRequest.getInputParameters().get(0).getValue();
//		String interestfactor = agentRequest.getInputParameters().get(1).getValue();
//		String intmarketpricefactor = agentRequest.getInputParameters().get(2).getValue();
//		String intmarketlowestprice = agentRequest.getInputParameters().get(3).getValue();
	
		String resultjson;
		JSONObject json = new JSONObject();
		String []index= {"one","ten","fifty","hundred"};
		
		for (int time = 0; time < 4; time++) {
		
			AgentRequest agrequest = new AgentRequest();
			Parameter param = new Parameter("transportationModes", getTransportationFile());
			agrequest.getInputParameters().add(param);

			Parameter param2 = new Parameter("ChemicalPlants", getChemicalPlants());
			agrequest.getInputParameters().add(param2);

			// additional Parameters --> five parameters
			Parameter param3 = new Parameter("CarbonTax", carbontax);
			agrequest.getInputParameters().add(param3);
			System.out.println("carbon tax value in mentable agent= " + carbontax);

			Parameter param4 = new Parameter("InterestFactor", interestfactor);
			agrequest.getInputParameters().add(param4);

			Parameter param5 = new Parameter("AnnualCostFactor", getAnnualCostFactor(time));
			agrequest.getInputParameters().add(param5);

			Parameter param6 = new Parameter("InternationalMarketPriceFactor", intmarketpricefactor);
			agrequest.getInputParameters().add(param6);

			Parameter param7 = new Parameter("InternationalMarketLowestPriceApplied", intmarketlowestprice);
			agrequest.getInputParameters().add(param7);

			AgentResponse resp = AgentCaller.callAgent(getContextPathForJPSMen(), agrequest);
			try {
				
				JSONArray array = new JSONArray();
				JSONObject item = new JSONObject();
				Double totalMaterialPurchaseCost= Double.valueOf((String) resp.getOutputParameters().get(1).getValue())/1000000000;
				Double totalTransportationCost= Double.valueOf((String) resp.getOutputParameters().get(3).getValue())/1000;
				Double totalInstallationCost= Double.valueOf((String) resp.getOutputParameters().get(6).getValue())/1000000;
				Double totalCO2Emission= Double.valueOf((String) resp.getOutputParameters().get(4).getValue());
				Double totalCO2EmissionCost= Double.valueOf((String) resp.getOutputParameters().get(5).getValue())/1000;
				Double totaltransportCost1yr=(1.0* totalTransportationCost*1000+totalInstallationCost*1000000)/1000000;
				Double totaltransportCost10yr= (10.0* totalTransportationCost*1000+totalInstallationCost*1000000)/1000000;
				Double totaltransportCost50yr= (50.0* totalTransportationCost*1000+totalInstallationCost*1000000)/1000000;
				Double totaltransportCost100yr= (100.0* totalTransportationCost*1000+totalInstallationCost*1000000)/1000000;
				
				
				
				//item.put("objectiveValue", resp.getOutputParameters().get(0).getValue());
				item.put("totalMaterialPurchaseCost", String.format("%.2f",totalMaterialPurchaseCost));
				//item.put("totalMaterialPurchaseCostInternationalMarket", resp.getOutputParameters().get(2).getValue());
				item.put("totalTransportationCost", String.format("%.2f", totalTransportationCost));
				item.put("totalCO2Emission", String.format("%.2f", totalCO2Emission));
				item.put("totalCO2EmissionCost", String.format("%.2f", totalCO2EmissionCost));
				item.put("totalInstallationCost", String.format("%.2f", totalInstallationCost));
				item.put("totalTransportCost1year", String.format("%.2f", totaltransportCost1yr));
				item.put("totalTransportCost10year", String.format("%.2f", totaltransportCost10yr));
				item.put("totalTransportCost50year", String.format("%.2f", totaltransportCost50yr));
				item.put("totalTransportCost100year", String.format("%.2f", totaltransportCost100yr));
				array.put(item);

				json.put("time" + index[time], array);

			} catch (JSONException e) {
				logger.error(e.getMessage(), e);
			}

		}
		resultjson = json.toString();
		String jsonstring = new Gson().toJson(json);
		System.out.println("response of the agent=" + resultjson);
		
	/*	String htmlRespone = "<html>";
		htmlRespone += "<h2>Hello World!!!<br/>";	
		
		for(int x=0;x<4;x++)
		{
			AgentRequest agrequest = new AgentRequest();
		Parameter param = new Parameter("transportationModes", getTransportationFile());
		agrequest.getInputParameters().add(param);

		Parameter param2 = new Parameter("ChemicalPlants", getChemicalPlants());
		agrequest.getInputParameters().add(param2);

		// additional Parameters --> five parameters
		Parameter param3 = new Parameter("CarbonTax", carbontax);
		agrequest.getInputParameters().add(param3);
		System.out.println("carbon tax value in mentable agent= "+carbontax);

		Parameter param4 = new Parameter("InterestFactor", interestfactor);
		agrequest.getInputParameters().add(param4);

		Parameter param5 = new Parameter("AnnualCostFactor", getAnnualCostFactor(x));
		agrequest.getInputParameters().add(param5);

		Parameter param6 = new Parameter("InternationalMarketPriceFactor", intmarketpricefactor);
		agrequest.getInputParameters().add(param6);

		Parameter param7 = new Parameter("InternationalMarketLowestPriceApplied", intmarketlowestprice);
		agrequest.getInputParameters().add(param7);
		
		
		AgentResponse resp = AgentCaller.callAgent(getContextPathForJPSMen(), agrequest);
				
		// build HTML code
		htmlRespone += "<h2>Your objectiveValue is: " + resp.getOutputParameters().get(0).getValue() + "<br/>";		
		htmlRespone += "Your totalMaterialPurchaseCost is: " + resp.getOutputParameters().get(1).getValue() + "</br>";		
		htmlRespone += "Your totalMaterialPurchaseCostInternationalMarket is: " + resp.getOutputParameters().get(2).getValue() + "</br>";		
		htmlRespone += "Your totalTransportationCost is: " + resp.getOutputParameters().get(3).getValue() + "</br>";		
		htmlRespone += "Your totalCO2Emission is: " + resp.getOutputParameters().get(4).getValue() + "</br>";		
		htmlRespone += "Your totalCO2EmissionCost is: " + resp.getOutputParameters().get(5).getValue() + "</br>";		
		htmlRespone += "Your totalInstallationCost is: " + resp.getOutputParameters().get(6).getValue() + "</h2>";	
		
		
		}
		htmlRespone += "</html>";*/
		

	
		// get response writer
		//PrintWriter writer = response.getWriter();
		//writer.println(htmlRespone);
		
		response.setContentType("application/json");
	
		response.getWriter().write(resultjson);

		logger.info("MEN_Table Agent stop");
	}
	
	 public void doPost(HttpServletRequest request, HttpServletResponse response)
			    throws IOException, ServletException
			    {
			        doGet(request, response);
			    }
	
	 
}
