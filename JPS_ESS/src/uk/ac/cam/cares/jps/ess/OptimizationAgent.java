package uk.ac.cam.cares.jps.ess;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/OptimizationAgent"})

public class OptimizationAgent extends JPSAgent {
	
	//suggesting the optimization model used based on storage technology chosen
	
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
		String path="JPS_ESS/LocateBattery"; //later can be queried from the agent descriptions
		
		String gencoordinate =  "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
				+ "SELECT ?entity ?class ?parent "
				+ "WHERE {?entity  a  ?class ."
				+ "?entity   j6:hasStateOfCharge ?dt ." 
				+ "?class rdfs:subClassOf ?parent ."
				+ "}";
		 
		
		String batIRI=requestParams.getString("storage");
		OntModel model=EnergyStorageSystem.readModelGreedy(batIRI);			
		List<String[]> resultList = EnergyStorageSystem.queryResult(model, gencoordinate);
		
		if(resultList.get(0)[2].toLowerCase().contains("battery")) {
			path="JPS_ESS/LocateBattery";
			}		
		JSONObject resultofOptimization=new JSONObject();
		resultofOptimization.put("optimization",path);
		return resultofOptimization;
		
		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String storageFormat = requestParams.getString("storage");
	        boolean q = InputValidator.checkIfValidIRI(storageFormat);
	        return q;
        }catch (Exception ex) {
        	ex.printStackTrace();
        }
        return false;
	}	
}
