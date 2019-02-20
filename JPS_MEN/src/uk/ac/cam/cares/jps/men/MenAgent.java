package uk.ac.cam.cares.jps.men;


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;


@WebServlet(urlPatterns = {"/MENAgent"})

public class MenAgent extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	private List<String> cpirilist = new ArrayList<String>();
	
	Logger logger = LoggerFactory.getLogger(MenAgent.class);
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("MENAgent start");
		
		JSONObject jo = AgentCaller.readJsonParameter(req);			
		String transportationModes = jo.getString("transportationModes");
		
		if (jo.has("ChemicalPlants")) {
			
			String chemicalPlants = jo.getString("ChemicalPlants");
			
			logger.info("MYMY chemicalPlant =" + chemicalPlants);
			
			String[] arr=chemicalPlants.split(",");
			cpirilist.addAll(Arrays.asList(arr));
		} else {
			String ecoindustrialpark = jo.getString("ecoindustrialpark");
			
			logger.info("MYMY ecoindustrialpark =" + ecoindustrialpark);
			
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
	
				cpirilist.add(irilist);
			}
		}
		
		String carbonTax = jo.getString("CarbonTax");
		String interestFactor = jo.getString("InterestFactor");
		String annualCostFactor = jo.getString("AnnualCostFactor");
		String internationalMarketPriceFactor = jo.getString("InternationalMarketPriceFactor");
		String internationalMarketLowestPriceApplied = jo.getString("InternationalMarketLowestPriceApplied");
		
		MenCalculationParameters parameters = new MenCalculationParameters(Double.parseDouble(carbonTax), Double.parseDouble(interestFactor), Double.parseDouble(annualCostFactor), Double.parseDouble(internationalMarketPriceFactor), Boolean.parseBoolean(internationalMarketLowestPriceApplied));
		MenDataProvider converter = new MenDataProvider();
		MenResult actual = converter.startCalculation(parameters,transportationModes,cpirilist);
		
		JSONObject result = new JSONObject();
		result.put("objectiveValue",String.valueOf(actual.objValue));
		result.put("totalMaterialPurchaseCost",String.valueOf(actual.totalMaterialPurchaseCost));
		result.put("totalMaterialPurchaseCostInternationalMarket",String.valueOf(actual.totalMaterialPurchaseCostInternationalMarket));
		result.put("totalTransportationCost",String.valueOf(actual.totalTransportationCost));
		result.put("totalCO2Emission",String.valueOf(actual.totalCO2Emission));
		result.put("totalCO2EmissionCost",String.valueOf(actual.totalC02EmissionCost));
		result.put("totalInstallationCost",String.valueOf(actual.totalInstallationCost));
		
		
		
		logger.info("MYMY MenAgent result =" + result.toString());
		
		AgentCaller.printToResponse(result.toString(), resp);

		cpirilist.clear();
		logger.info("MENAgent exit");
	}
}