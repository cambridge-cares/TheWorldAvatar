package uk.ac.cam.cares.jps.wte.visualization;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

@WebServlet(urlPatterns = { "/WTEVisualization/createLine", "/WTEVisualization/createMarkers/*"})
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
	/**gets the OnsiteWasteTreatment name, xy coordinates
	 */
	public static String OnsiteQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#> "
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
	private Logger logger = LoggerFactory.getLogger(WTEVisualization.class);
	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getServletPath();
		JSONObject joforEN = AgentCaller.readJsonParameter(request);
		String iriofnetwork = joforEN.optString("wastenetwork",
				"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem");
		new 	WastetoEnergyAgent();
		OntModel model = WastetoEnergyAgent.readModelGreedy(iriofnetwork); //because this is a static method
		logger.info("path called= "+path);
		 if ("/WTEVisualization/createMarkers".equals(path)) {

			logger.info("path called here= " + path);
			String g=createMarkers(model);
			
			AgentCaller.printToResponse(g, response);
		}
		
		System.gc();
	}
	/** create the food court markers and onsite/offsite markers. 
	 * 
	 * @param model
	 * @return
	 * @throws IOException
	 */
	public String createMarkers(OntModel model) throws IOException {
		ArrayList<String>textcomb=new ArrayList<String>();
		List<String[]> foodcourts = queryCoordinate(model, FCQuery); //hard assumption that there would be foodcourts all the time
		for (int i = 0; i < foodcourts.size(); i++) {
			String content="{\"coors\": {\"lat\": "+foodcourts.get(i)[3]+", \"lng\": "+foodcourts.get(i)[2]
					+ "},  \"name\": \""+foodcourts.get(i)[0]+"\"}";
			textcomb.add(content);
		}
		List<String[]> onsiteTechnologies = queryCoordinate(model, OnsiteQuery);
		JSONArray jsArray = new JSONArray(textcomb);
	    JSONObject jo = new JSONObject();
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
}
