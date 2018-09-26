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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;


@WebServlet(urlPatterns = {"/MENAgent"})

public class MenAgent extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	private List<String> cpirilist = new ArrayList<String>();
	
	Logger logger = LoggerFactory.getLogger(MenAgent.class);
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("MENAgent start");
	
		AgentRequest agentRequest = AgentCaller.getAgentRequest(req);
				
		String value1 =(String) agentRequest.getInputParameters().get(0).getValue();
		
		String value2key =(String) agentRequest.getInputParameters().get(1).getKey();
		String value2 =(String) agentRequest.getInputParameters().get(1).getValue();
		
		if (value2key.equals("ChemicalPlants"))
		{
			String[] arr=value2.split(",");
			//cp=Arrays.asList(arr);
			 cpirilist.addAll(Arrays.asList(arr));
		}	
		
		else {
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

		ResultSet rs_plant = MenDataProvider.sparql(value2, chemicalplantInfo); 
		
		for (; rs_plant.hasNext();) {			
			QuerySolution qs_p = rs_plant.nextSolution();

			Literal cpiri = qs_p.getLiteral("iri");
			String irilist = cpiri.getString();

			cpirilist.add(irilist);
		}
		}
		
		String value3 =(String) agentRequest.getInputParameters().get(2).getValue();
		String value4 =(String) agentRequest.getInputParameters().get(3).getValue();
		String value5 =(String) agentRequest.getInputParameters().get(4).getValue();
		String value6 =(String) agentRequest.getInputParameters().get(5).getValue();
		String value7 =(String) agentRequest.getInputParameters().get(6).getValue();	
		
		AgentResponse agentResponse = new AgentResponse();
		//copy from the request stream of input and output parameter into the response stream
		AbstractAgentServiceDescription.copyParameters(agentRequest, agentResponse);
		
		MenCalculationParameters parameters = new MenCalculationParameters(Double.parseDouble(value3), Double.parseDouble(value4), Double.parseDouble(value5), Double.parseDouble(value6), Boolean.parseBoolean(value7));
		MenDataProvider converter = new MenDataProvider();
		MenResult actual = converter.startCalculation(parameters,value1,cpirilist);
		
		Parameter param1 = new Parameter("objectiveValue",String.valueOf(actual.objValue));
		agentResponse.getOutputParameters().add(param1);
		Parameter param2 = new Parameter("totalMaterialPurchaseCost",String.valueOf(actual.totalMaterialPurchaseCost));
		agentResponse.getOutputParameters().add(param2);
		Parameter param3 = new Parameter("totalMaterialPurchaseCostInternationalMarket",String.valueOf(actual.totalMaterialPurchaseCostInternationalMarket));
		agentResponse.getOutputParameters().add(param3);
		Parameter param4 = new Parameter("totalTransportationCost",String.valueOf(actual.totalTransportationCost));
		agentResponse.getOutputParameters().add(param4);
		Parameter param5 = new Parameter("totalCO2Emission",String.valueOf(actual.totalCO2Emission));
		agentResponse.getOutputParameters().add(param5);
		Parameter param6 = new Parameter("totalCO2EmissionCost",String.valueOf(actual.totalC02EmissionCost));
		agentResponse.getOutputParameters().add(param6);
		Parameter param7 = new Parameter("totalInstallationCost",String.valueOf(actual.totalInstallationCost));
		agentResponse.getOutputParameters().add(param7);
		
		AgentCaller.printToResponse(agentResponse, resp);
		cpirilist.clear();
		logger.info("MENAgent exit");
	}


}