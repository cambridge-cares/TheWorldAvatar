package uk.ac.cam.cares.jps.men;


import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentResponse;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;


@WebServlet(urlPatterns = {""})

public class MenAgent extends HttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	
	Logger logger = LoggerFactory.getLogger(MenAgent.class);
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {
		
		logger.info("MENAgent start");
	
		AgentRequest agentRequest = AgentCaller.getAgentRequest(req);
				
		String key1 =agentRequest.getInputParameters().get(0).getKey();
		String value1 =(String) agentRequest.getInputParameters().get(0).getValue();
		String value2 =(String) agentRequest.getInputParameters().get(1).getValue();
		String[] arr=value2.split(",");
		List<String> cp =  Arrays.asList(arr);
				
		String value3 =(String) agentRequest.getInputParameters().get(2).getValue();
		String value4 =(String) agentRequest.getInputParameters().get(3).getValue();
		String value5 =(String) agentRequest.getInputParameters().get(4).getValue();
		String value6 =(String) agentRequest.getInputParameters().get(5).getValue();
		String value7 =(String) agentRequest.getInputParameters().get(6).getValue();
		
		//String value8 =agentRequest.getInputParameters().get(0).getKey(); ????
		System.out.println("result value3= "+value3);
		System.out.println("result of agent calling= "+key1 +"and "+value1);
		//...
		
        
		AgentResponse agentResponse = new AgentResponse();
		//copy from the request stream of input and output parameter into the response stream
		AbstractAgentServiceDescription.copyParameters(agentRequest, agentResponse);
		
		//later put the value to be taken from the website of weather and put to the knowledge base of the weather		
		
		MenCalculationParameters parameters = new MenCalculationParameters(Double.parseDouble(value3), Double.parseDouble(value4), Double.parseDouble(value5), Double.parseDouble(value6), Boolean.parseBoolean(value7));
		MenDataProvider converter = new MenDataProvider();
		MenResult actual = converter.startCalculation(parameters,value1,cp);
		
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
		
		logger.info("MENAgent exit");
	}


}