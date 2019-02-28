package uk.ac.cam.cares.jps.men;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;


@WebServlet(urlPatterns = {"/MENAgent"})

public class MenAgent extends JPSHttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	private List<String> cpirilist = new ArrayList<String>();
	
	Logger logger = LoggerFactory.getLogger(MenAgent.class);
	
	@Override
	protected void doGetJPS(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		
		logger.info("MENAgent start");
		
		JSONObject jo = AgentCaller.readJsonParameter(req);			
		String transportationModes = jo.optString("transportationmodes", "http://www.jparksimulator.com/kb/sgp/jurongisland/MaterialTransportMode.owl");
		
		if (jo.has("chemicalplants")) {
			
			String chemicalPlants = jo.getString("chemicalplants");			
			String[] arr=chemicalPlants.split(",");
			cpirilist.addAll(Arrays.asList(arr));
		} else {
			String ecoindustrialpark = jo.getString("ecoindustrialpark");
			
			String chemicalplantInfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> " 
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "SELECT ?iri "
					+ "WHERE {?entity  a  cp:Eco-industrialPark  ." 
					+ "?entity   j2:hasSubsystem ?cpl ."
					+ "?cpl  a  cp:ChemicalPlant  ."
					+ "?cpl   cp:hasConceptualModel ?cm ."
					+ "?cm   cp:hasIRI ?iri ." 
					+ "}"
					+ "ORDER BY ?product DESC(?added)";
	
			ResultSet rs_plant = MenDataProvider.sparql(ecoindustrialpark, chemicalplantInfo); 
			
			for (; rs_plant.hasNext();) {			
				QuerySolution qs_p = rs_plant.nextSolution();
	
				Literal cpiri = qs_p.getLiteral("iri");
				String irilist = cpiri.getString();
				irilist = irilist.replace(" ", "");
				irilist = irilist.replace("\n", "");
				irilist = irilist.replace("\r", "");
				irilist = irilist.replace("\t", "");
	
				cpirilist.add(irilist);
			}
		}
		
		String carbonTax = "" + jo.getDouble("carbontax");
		String interestFactor = "" + jo.getDouble("interestfactor");
		String annualCostFactor = "" + jo.getDouble("annualcostfactor");
		String internationalMarketPriceFactor = "" + jo.optDouble("internationalmarketpricefactor", 1.05);
		String internationalMarketLowestPriceApplied = jo.optString("internationalmarketlowestpriceapplied", "false");
		
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
			
		AgentCaller.printToResponse(result.toString(), resp);

		cpirilist.clear();
		logger.info("MENAgent exit");
	}
}