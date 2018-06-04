package uk.ac.cam.cares.jps.men;

import java.io.File;
import java.io.IOException;

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
	}
	
	private String getAnnualCostFactor(int x, String[] annualcostfactor) {
		String ACF[]= new String[annualcostfactor.length];
		for (int a=0;a<annualcostfactor.length;a++)
		{
			ACF[a]=String.valueOf(1/Double.valueOf(annualcostfactor[a])); 
			if (1/Double.valueOf(annualcostfactor[a])>=1)
			{
			ACF[a]=String.format("%.1f",1/Double.valueOf(annualcostfactor[a]));
			}
		}
		
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
		
		String jsonString = request.getParameter("listOfInputs");
		
		System.out.println("jsonstring= "+jsonString);
		
		
		
		// read form fields	
		String[]parameterfromjson =jsonString.replaceAll("[()\\[\\]]", "").split(",");
		int sizeofparameterjson= parameterfromjson.length;
		
		String carbontax = parameterfromjson[0];
		String interestfactor = parameterfromjson[1];
		String intmarketpricefactor =parameterfromjson[2];
		String intmarketlowestprice = parameterfromjson[3];
		
		String[] annualcostfactor= new String[sizeofparameterjson-4];
		for (int timefactor=0;timefactor<sizeofparameterjson-4;timefactor++)
		{
			annualcostfactor[timefactor]=parameterfromjson[timefactor+4].replaceAll("\"", "");
		}
		
		//from the test
//		String carbontax = agentRequest.getInputParameters().get(0).getValue();
//		String interestfactor = agentRequest.getInputParameters().get(1).getValue();
//		String intmarketpricefactor = agentRequest.getInputParameters().get(2).getValue();
//		String intmarketlowestprice = agentRequest.getInputParameters().get(3).getValue();
	
		String resultjson;
		JSONObject json = new JSONObject();
		
		//Double[] totaltransportcost= new Double[annualcostfactor.length];
		Double[][] totaltransportcost= new Double[annualcostfactor.length][annualcostfactor.length];
		
		
		System.out.println(annualcostfactor);
		
		JSONArray tmpc = new JSONArray();
		JSONArray ttc = new JSONArray();
		JSONArray tic = new JSONArray();
		JSONArray tcec = new JSONArray();
		JSONArray tce = new JSONArray();
		JSONArray ttyc = new JSONArray();
		
		for (int time = 0; time < annualcostfactor.length; time++) {
		
			AgentRequest agrequest = new AgentRequest();
			Parameter param = new Parameter("transportationModes", getTransportationFile());
			agrequest.getInputParameters().add(param);

			Parameter param2 = new Parameter("ChemicalPlants", getChemicalPlants());
			agrequest.getInputParameters().add(param2);

			// additional Parameters --> five parameters
			Parameter param3 = new Parameter("CarbonTax", carbontax);
			agrequest.getInputParameters().add(param3);

			Parameter param4 = new Parameter("InterestFactor", interestfactor);
			agrequest.getInputParameters().add(param4);

			Parameter param5 = new Parameter("AnnualCostFactor", getAnnualCostFactor(time,annualcostfactor));
			agrequest.getInputParameters().add(param5);

			Parameter param6 = new Parameter("InternationalMarketPriceFactor", intmarketpricefactor);
			agrequest.getInputParameters().add(param6);

			Parameter param7 = new Parameter("InternationalMarketLowestPriceApplied", intmarketlowestprice);
			agrequest.getInputParameters().add(param7);

			AgentResponse resp = AgentCaller.callAgent(getContextPathForJPSMen(), agrequest);
			

			/*try {
				
				JSONArray array = new JSONArray();
				JSONObject item = new JSONObject();
				Double totalMaterialPurchaseCost= Double.valueOf((String) resp.getOutputParameters().get(1).getValue())/1000000000;
				Double totalTransportationCost= Double.valueOf((String) resp.getOutputParameters().get(3).getValue())/1000;
				Double totalInstallationCost= Double.valueOf((String) resp.getOutputParameters().get(6).getValue())/1000000;
				Double totalCO2Emission= Double.valueOf((String) resp.getOutputParameters().get(4).getValue());
				Double totalCO2EmissionCost= Double.valueOf((String) resp.getOutputParameters().get(5).getValue())/1000;
				
				
				//item.put("objectiveValue", resp.getOutputParameters().get(0).getValue());
				item.put("totalMaterialPurchaseCost", String.format("%.2f",totalMaterialPurchaseCost));
				//item.put("totalMaterialPurchaseCostInternationalMarket", resp.getOutputParameters().get(2).getValue());
				item.put("totalTransportationCost", String.format("%.2f", totalTransportationCost));
				item.put("totalCO2Emission", String.format("%.2f", totalCO2Emission));
				item.put("totalCO2EmissionCost", String.format("%.2f", totalCO2EmissionCost));
				item.put("totalInstallationCost", String.format("%.2f", totalInstallationCost));
			
				
				for (int count=0;count<annualcostfactor.length;count++)
				{
					 totaltransportcost[count]=(Double.valueOf(annualcostfactor[count])* totalTransportationCost*1000+totalInstallationCost*1000000)/1000000;

						item.put("totalTransportCost"+annualcostfactor[count]+"year", String.format("%.2f", totaltransportcost[count]));
				}
				array.put(item);
		

				json.put("time" + annualcostfactor[time], array);

			} */
			
			Double totalMaterialPurchaseCost= Double.valueOf((String) resp.getOutputParameters().get(1).getValue())/1000000000;
			Double totalTransportationCost= Double.valueOf((String) resp.getOutputParameters().get(3).getValue())/1000;
			Double totalInstallationCost= Double.valueOf((String) resp.getOutputParameters().get(6).getValue())/1000000;
			Double totalCO2Emission= Double.valueOf((String) resp.getOutputParameters().get(4).getValue());
			Double totalCO2EmissionCost= Double.valueOf((String) resp.getOutputParameters().get(5).getValue())/1000;
			
			tmpc.put(String.format("%.2f",totalMaterialPurchaseCost));
			ttc.put(String.format("%.2f", totalTransportationCost));
			tce.put(String.format("%.2f", totalCO2Emission));
			tcec.put(String.format("%.2f", totalCO2EmissionCost));
			tic.put(String.format("%.2f", totalInstallationCost));			 
			
		
		}
		for (int count=0;count<annualcostfactor.length;count++)
		{
			for (int count2=0;count2<annualcostfactor.length;count2++)
			{
				try {
					totaltransportcost[count][count2]=(Double.valueOf(annualcostfactor[count2])* ttc.getDouble(count)*1000+tic.getDouble(count)*1000000)/1000000;
				} catch (NumberFormatException | JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				ttyc.put(String.format("%.2f", totaltransportcost[count][count2]));
			}
				
		}

		try {
			json.put("totalMaterialPurchaseCost", tmpc);
			json.put("totalTransportationCost", ttc);
			json.put("totalCO2Emission", tce);
			json.put("totalCO2EmissionCost", tcec);
			json.put("totalInstallationCost", tic);
			json.put("totaltransportyearcost",ttyc);
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		resultjson = json.toString();
		System.out.println("response of the agent=" + resultjson);

		
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
