package uk.ac.cam.cares.jps.men;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.commons.validator.routines.DoubleValidator;
import org.apache.commons.validator.routines.UrlValidator;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;


@WebServlet(urlPatterns = {"/MENAgent"})

public class MenAgent extends JPSAgent{
	
	private static final long serialVersionUID = -4199209974912271432L;
	private List<String> cpirilist = new ArrayList<String>();
	
	Logger logger = LoggerFactory.getLogger(MenAgent.class);
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	    
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			
		
		logger.info("MENAgent start");
		boolean validInputs = validateInput(requestParams);
		if (validInputs == false) {
			throw new JSONException("Error caused by problematic inputs");
		}
		String transportationModes = requestParams.optString("transportationmodes", "http://www.jparksimulator.com/kb/sgp/jurongisland/MaterialTransportMode.owl");
		
		if (requestParams.has("chemicalplants")) {
			
			String chemicalPlants = requestParams.getString("chemicalplants");			
			String[] arr=chemicalPlants.split(",");
			cpirilist.addAll(Arrays.asList(arr));
		} else {
			String ecoindustrialpark = requestParams.getString("ecoindustrialpark");
			String chemicalplantInfo = new SelectBuilder()
					.addPrefix("cp", "http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#")
					.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
					.addVar("?iri").addWhere("?entity", "a", "cp:Eco-industrialPark")
					.addWhere("?entity", "j2:hasSubsystem", "?iri")
					.buildString();
			
	
			ResultSet rs_plant = MenDataProvider.sparql(ecoindustrialpark, chemicalplantInfo); 
			
			for (; rs_plant.hasNext();) {			
				QuerySolution qs_p = rs_plant.nextSolution();
				Resource cpiri = qs_p.getResource("iri");
				String irilist = cpiri.toString();
				System.out.println(irilist);
				irilist = irilist.replace(" ", "");
				irilist = irilist.replace("\n", "");
				irilist = irilist.replace("\r", "");
				irilist = irilist.replace("\t", "");
				
				cpirilist.add(irilist);
			}
		}
		
		String carbonTax = "" + requestParams.getDouble("carbontax");
		String interestFactor = "" +requestParams.getDouble("interestfactor");
		String annualCostFactor = "" + requestParams.getDouble("annualcostfactor");
		String internationalMarketPriceFactor = "" + requestParams.optDouble("internationalmarketpricefactor", 1.05);
		String internationalMarketLowestPriceApplied =requestParams.optString("internationalmarketlowestpriceapplied", "false");
		
		MenCalculationParameters parameters = new MenCalculationParameters(Double.parseDouble(carbonTax), Double.parseDouble(interestFactor), Double.parseDouble(annualCostFactor), Double.parseDouble(internationalMarketPriceFactor), Boolean.parseBoolean(internationalMarketLowestPriceApplied));
		MenDataProvider converter = new MenDataProvider();
		MenResult actual = converter.startCalculation(parameters,transportationModes,cpirilist);
		

		
		JSONObject result = new JSONObject();
		//result.put("objectivevalue",String.valueOf(actual.objValue));
		result.put("totalmaterialpurchasecost",String.valueOf(actual.totalMaterialPurchaseCost));
		result.put("totalmaterialpurchasecostinternationalmarket",String.valueOf(actual.totalMaterialPurchaseCostInternationalMarket));
		result.put("totaltransportationcost",String.valueOf(actual.totalTransportationCost));
		result.put("totalco2emission",String.valueOf(actual.totalCO2Emission));
		result.put("totalco2emissioncost",String.valueOf(actual.totalC02EmissionCost));
		result.put("totalinstallationcost",String.valueOf(actual.totalInstallationCost));
		
		double totalCost = actual.totalMaterialPurchaseCost + actual.totalMaterialPurchaseCostInternationalMarket 
				+ actual.totalTransportationCost + actual.totalC02EmissionCost + actual.totalInstallationCost;
		result.put("totalcost", totalCost);

		cpirilist.clear();
		logger.info("MENAgent exit");
		return result;
	}
	/** validate input for args in input from Table Agent
	 * 
	 * @param args JSONObject
	 * @return
	 */
	public boolean validateInput(JSONObject args) {
		if (args.isEmpty()) {
            throw new BadRequestException();
        }
        boolean transport_opt,chemical_opt,bool_opt,impf_opt, comp_opt; 
        transport_opt= chemical_opt =bool_opt= impf_opt = comp_opt= true; //set default value to true
        DoubleValidator doubleValidator = new DoubleValidator();
    	try {
        if (args.has("transportationmodes")) {//string of url representing transport
        	transport_opt = InputValidator.checkIfValidIRI(args.get("transportationmodes").toString() );
        	
        }
        if (args.has("chemicalplants")) {//optional value
        	String plantLists = args.get("chemicalplants").toString();
        	String[] arr = plantLists.split(",");
        	//test all members of array
        	for (int i = 0; i< arr.length; i++) {
        		if (InputValidator.checkIfValidIRI(arr[i]) == false) {
        			chemical_opt = false;
        		}
        	}
			
        	
        }
        if (args.has("internationalmarketpricefactor")) {
        	impf_opt = doubleValidator.isValid(args.get("internationalmarketpricefactor").toString());
        	
        }
        if (args.has("internationalmarketlowestpriceapplied")) {
        	//There isn't a method to check if a value is boolean. Only if it's true or false
        	bool_opt = InputValidator.checkBoolean(args.get("internationalmarketlowestpriceapplied"));
        }
       //these are the compulsory data; thus they're in the true category
        	boolean carbBool=  doubleValidator.isValid(args.get("carbontax").toString());
        	boolean intBool =  doubleValidator.isValid(args.get("interestfactor").toString());
        	boolean annuBool = doubleValidator.isValid(args.get("annualcostfactor").toString());
        	comp_opt = carbBool && intBool &&annuBool;
    	} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	 if (transport_opt &&chemical_opt&&bool_opt&&impf_opt&& comp_opt== true) {
    		 return  true;
     	}else {
     		return false;
     	}
    	
}
}