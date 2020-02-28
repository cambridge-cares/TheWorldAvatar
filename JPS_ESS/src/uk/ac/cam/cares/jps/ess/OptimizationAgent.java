package uk.ac.cam.cares.jps.ess;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/OptimizationAgent"})

public class OptimizationAgent extends JPSHttpServlet {
	
	//suggesting the optimization model used based on storage technology chosen
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		
		String path="JPS_ESS/LocateBattery"; //later can be queried from the agent descriptions
		
		String gencoordinate = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
				+ "SELECT ?entity ?class ?parent "
				+ "WHERE {?entity  a  ?class ."
				+ "?entity   j6:hasStateOfCharge ?dt ." 
				+ "?class rdfs:subClassOf ?parent ."
				+ "}";
		 
		
		String batIRI=joforess.getString("storage");
		 String result = new QueryBroker().queryFile(batIRI, gencoordinate);
		 String[] keys = {"entity", "class","parent"};
		 List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		 System.out.println("classtype= "+resultList.get(0)[2]);
		
			if(resultList.get(0)[2].toLowerCase().contains("battery")) {
				path="JPS_ESS/LocateBattery";
			}		
//		if(batIRI.contains("VRB")) {
//			path="JPS_ESS/LocateBattery";
//		}
		
		
		
		JSONObject resultofOptimization=new JSONObject();
		resultofOptimization.put("optimization",path);
		AgentCaller.printToResponse(resultofOptimization, response);
		
		
	}

}
