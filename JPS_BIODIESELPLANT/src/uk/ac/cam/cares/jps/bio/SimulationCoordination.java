package uk.ac.cam.cares.jps.bio;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet("/SimCoord")
public class SimulationCoordination extends JPSHttpServlet{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Figure out if DoSimulation is to be done 
	 * @param: request containing PLANTIRI, the top node of the biodieselplant
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub

		JSONObject jo = AgentCaller.readJsonParameter(request);
		System.out.println(jo.toString());
		OntModel model = readModelGreedy(jo.optString("PLANTIRI", "http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl"));
		String reactorInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity " 
				+ "WHERE {?entity  a  j1:StirredTank  ."
				+ "?entity   j2:hasElectricalRepresentation  ?loadiri ."
				+ "}";
		List<String[]> reactList = getResultList(model,reactorInfo);
		if (reactList.size()>1) {
			 AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoSimulation", jo.toString());
		}else {
			 AgentCaller.executeGetWithJsonParameter("JPS_BIODIESELPLANT3/DoSimulation2", jo.toString());
		}
		
	}
	public void helperRequest() {
		OntModel model = readModelGreedy("http://theworldavatar.com/kb/sgp/jurongisland/biodieselplant2/BiodieselPlant2.owl");
		String reactorInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>"
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>"
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>"
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>"
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>"
				+ "SELECT ?entity " 
				+ "WHERE {?entity  a  j1:StirredTank  ."
				+ "?entity   j2:hasElectricalRepresentation  ?loadiri ."
				+ "}";
		List<String[]> reactList = getResultList(model,reactorInfo);
		if (reactList.size()>1) {
			 System.out.println("Reactor 3");
		}else {
			System.out.println("Reactor 2");
		}
	}
	public OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	public List<String[]> getResultList(OntModel model, String info){
	   ResultSet resultSet = JenaHelper.query(model, info);
	   String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	   String[] keys = JenaResultSetFormatter.getKeys(result);
	   List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	   return resultList;
   }
}
