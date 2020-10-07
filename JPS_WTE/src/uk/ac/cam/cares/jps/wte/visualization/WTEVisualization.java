package uk.ac.cam.cares.jps.wte.visualization;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

@WebServlet(urlPatterns = { "/WTEVisualization/createMarkers/*", "/WTEVisualization/queryOnsite/*","/WTEVisualization/readInputs/*"})
public class WTEVisualization extends JPSHttpServlet{
	/**gets the food court name, xy coordinates
	 */
	public static String FCQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT ?entity ?name ?xvalue ?yvalue " //YEAR IS NOT INCLUDED IF JUST USING SIMPLIFIED VERSION
			+ "WHERE {"
			+ "?entity  a j1:FoodCourt ."
			+ "?entity   j8:hasName ?name ." 
            + "?entity   j7:hasGISCoordinateSystem ?coorsys ."
            + "?coorsys   j7:hasProjectedCoordinate_x ?x ." 
            + "?x   j2:hasValue ?xval ."
            + "?xval   j2:numericalValue ?xvalue ."
            + "?coorsys   j7:hasProjectedCoordinate_y ?y ."
            + "?y   j2:hasValue ?yval ."
            + "?yval   j2:numericalValue ?yvalue ."
			+ "}";
	/**gets the OffsiteWasteTreatment entity, xy coordinates
	 */
	public static String WTquery="PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT DISTINCT ?entity ?xvalue ?yvalue "
			+ "WHERE {" 
			+ "?entity  a j1:OffsiteWasteTreatmentFacility ."			
			+ "?entity   j7:hasGISCoordinateSystem ?coorsys ." 
			+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
			+ "?x   j2:hasValue ?xval ." 
			+ "?xval   j2:numericalValue ?xvalue ."
			+ "?coorsys   j7:hasProjectedCoordinate_y ?y ." 
			+ "?y   j2:hasValue ?yval ."
			+ "?yval   j2:numericalValue ?yvalue ."
			+ "}";
	/**gets the OffsiteWasteTreatment entity, xy coordinates
	 */
	public static String OnWTquery="PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.w3.org/2006/time#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#> "
			+ "SELECT DISTINCT ?entity ?xvalue ?yvalue "
			+ "WHERE {" 
			+ "?entity  a j1:OnsiteWasteTreatmentFacility ."
			+ "?entity   j7:hasGISCoordinateSystem ?coorsys ." 
			+ "?coorsys   j7:hasProjectedCoordinate_x ?x ."
			+ "?x   j2:hasValue ?xval ." 
			+ "OPTIONAL{?xval   j2:numericalValue ?xvalue }"
			+ "?coorsys   j7:hasProjectedCoordinate_y ?y ." 
			+ "?y   j2:hasValue ?yval ."
			+ "OPTIONAL{?yval   j2:numericalValue ?yvalue }."
			+ "}";
	
	private Logger logger = LoggerFactory.getLogger(WTEVisualization.class);
	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getServletPath();
		JSONObject joforEN = AgentCaller.readJsonParameter(request);
		String iriofnetwork = joforEN.optString("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork); //because this is a static method
		logger.info("path called= "+path);
		 if ("/WTEVisualization/createMarkers".equals(path)) {
			logger.info("path called here= " + path);
			String g=createMarkers(model, joforEN);
			AgentCaller.printToResponse(g, response);
		}else if ("/WTEVisualization/readInputs".equals(path)) {
			logger.info("path called here= " + path);
			String g=readInputs(model);
			AgentCaller.printToResponse(g, response);
		}else if ("/WTEVisualization/readComp".equals(path)) {
			logger.info("path called here= " + path);
			String g=readComp(model);
			AgentCaller.printToResponse(g, response);
		}else if ("/WTEVisualization/queryOnsite".equals(path)) {
			logger.info("path called here= " + path);
			String g=searchOnsite(model, joforEN);
			AgentCaller.printToResponse(g, response);
		}
		
		System.gc();
	}

	/** create the onsite markers. 
	 * 
	 * @param model
	 * @return
	 * @throws IOException
	 */
	public String searchOnsite(OntModel model, JSONObject jo) throws IOException {
		ArrayList<String>textcomb=new ArrayList<String>();
		
		List<String[]> offsiteTechnologies = queryCoordinate(model, OnWTquery);
		for (int i = 0; i < offsiteTechnologies.size(); i++) {
			String content="{\"coors\": {\"lat\": "+offsiteTechnologies.get(i)[2]+", \"lng\": "+offsiteTechnologies.get(i)[1]
					+ "},  \"entity\": \""+offsiteTechnologies.get(i)[0]+"\"}";
			textcomb.add(content);
		}
		JSONArray jsArray = new JSONArray(textcomb);
	    jo.put("result", jsArray);
		return jo.toString();
	}
	/** create the food court markers and onsite/offsite markers. 
	 * 
	 * @param model
	 * @return
	 * @throws IOException
	 */
	public String createMarkers(OntModel model, JSONObject jo) throws IOException {
		ArrayList<String>textcomb=new ArrayList<String>();
		List<String[]> foodcourts = queryCoordinate(model, FCQuery); //hard assumption that there would be foodcourts all the time
		for (int i = 0; i < foodcourts.size(); i++) {
			String content="{\"coors\": {\"lat\": "+foodcourts.get(i)[3]+", \"lng\": "+foodcourts.get(i)[2]
					+ "},  \"entity\": \""+foodcourts.get(i)[0]+"\",  \"name\": \""+foodcourts.get(i)[1]
							+"\"}";
			textcomb.add(content);
		}
		List<String[]> offsiteTechnologies = queryCoordinate(model, WTquery);
		for (int i = 0; i < offsiteTechnologies.size(); i++) {
			String content="{\"coors\": {\"lat\": "+offsiteTechnologies.get(i)[2]+", \"lng\": "+offsiteTechnologies.get(i)[1]
					+ "},  \"entity\": \""+offsiteTechnologies.get(i)[0]+"\"}";
			textcomb.add(content);
		}
		JSONArray jsArray = new JSONArray(textcomb);
	    jo.put("result", jsArray);
		return jo.toString();
	}
	public static List<String[]> queryCoordinate(OntModel model, String query) {
		
			ResultSet resultSet = JenaHelper.query(model, query);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			return resultListfromquery;
	}
	/** read the values of the inputs according to the query 
	 * 
	 * @param model
	 * @return
	 */
	public String readInputs(OntModel model) {
		ResultSet resultSet = JenaHelper.query(model, new WastetoEnergyAgent().WTFTechQuery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        
        ResultSet resultSet2 = JenaHelper.query(model, new WastetoEnergyAgent().WTFTechOnsiteQuery);
		String result2 = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet2);
        String[] keys2 = JenaResultSetFormatter.getKeys(result2);
        List<String[]> resultList2 = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
        List<String> res1 = modifyOutputs(resultList);
        List<String> res2 = modifyOutputs(resultList2);
        
        JSONArray jsArray1 = new JSONArray(res1);
        JSONArray jsArray2 = new JSONArray(res2);
	    JSONObject jo = new JSONObject();
	    jo.put("offsite", jsArray1);
	    jo.put("onsite", jsArray2);
		return jo.toString();
	}
	/** helper function for readInputs
	 * stores tax, installation and operation costs per off site or onsite
	 * @param newList list<String[]>
	 * @return res List<String> {"tax":,"installationcost":,"operationcost":}
	 */
	public List<String> modifyOutputs(List<String[]> newList) {
		List<String> res = new ArrayList<String>();
		for (int i = 0; i < newList.size(); i++) {
        	JSONObject jo = new JSONObject();
        	jo.put("tax", newList.get(i)[0]);
        	jo.put("installationcost", newList.get(i)[2]);
        	jo.put("operationcost", newList.get(i)[3]);
        	jo.put("manpowercost", newList.get(i)[6]);
			res.add(jo.toString());
		}
		return res;
	}
	public String readComp(OntModel model) {
		ResultSet resultSet = JenaHelper.query(model, new WastetoEnergyAgent().wasteSystemOutputQuery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return null;
		
	}
}
