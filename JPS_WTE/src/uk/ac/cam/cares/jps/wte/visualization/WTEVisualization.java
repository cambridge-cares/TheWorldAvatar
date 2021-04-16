package uk.ac.cam.cares.jps.wte.visualization;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.wte.FCQuerySource;
import uk.ac.cam.cares.jps.wte.WTESingleAgent;

@WebServlet(urlPatterns = { "/WTEVisualization/createMarkers/*", "/WTEVisualization/queryOnsite/*","/WTEVisualization/readInputs/*"})
public class WTEVisualization extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private Logger logger = LoggerFactory.getLogger(WTEVisualization.class);
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request){
		if (!validateInput(requestParams)) {
			throw new BadRequestException("WTE:WTEVisualizationAgent: Input parameters not found.\n");
		}
		String iriofnetwork = requestParams.getString("wastenetwork");

		String path = requestParams.getString(JPSConstants.SCENARIO_AGENT_OPERATION);
		OntModel model = readModelGreedy(iriofnetwork); //Visualization needs to read from scenario folder
		String g = "";
		 if (path.contains("/WTEVisualization/createMarkers")) {
			logger.info("path called here= " + path);
			g=createMarkers(model, requestParams);
		}else if (path.contains("/WTEVisualization/readInputs")) {
			logger.info("path called here= " + path);
			g=readInputs(model);
		}else if (path.contains("/WTEVisualization/queryOnsite")) {
			logger.info("path called here= " + path);
			g=searchOnsite(model, requestParams);
		}
		JSONObject responseParams = new JSONObject(g);
		System.gc();
		return responseParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
        String iriofnetwork = requestParams.getString("wastenetwork");
        String path = requestParams.getString(JPSConstants.SCENARIO_AGENT_OPERATION);
        boolean relevant = path.contains("createMarkers") 
        		|| path.contains("readInputs") ||
        		path.contains("queryOnsite");
        return InputValidator.checkIfValidIRI(iriofnetwork) & relevant;
        } catch (JSONException ex) {
        	return false;
        
        }
    }
	/** get wastesite arrangement in input
	 * 
	 * @param model
	 * @param query queryString for search
	 * @return
	 */
	public ArrayList<String> generalWasteSiteTreatmentSearch(OntModel model, String query) {
		ArrayList<String>textcomb=new ArrayList<String>();
		
		List<String[]> siteTechnologies = FCQuerySource.queryResult(model, query);
		for (int i = 0; i < siteTechnologies.size(); i++) {
			JSONObject jo2 = new JSONObject();
			jo2.put("lat", siteTechnologies.get(i)[2]);
			jo2.put("lng", siteTechnologies.get(i)[1]);
			JSONObject jo3 = new JSONObject().put("coors", jo2).put("entity",siteTechnologies.get(i)[0]);
			textcomb.add(jo3.toString());
		}
		return textcomb;
		
	}
	/** create the onsite markers. 
	 * 
	 * @param model
	 * @return
	 * @throws IOException
	 */
	public String searchOnsite(OntModel model, JSONObject jo){
		
		ArrayList<String>textcomb=generalWasteSiteTreatmentSearch(model, FCQuerySource.getOnsiteWasteTreatmentQuery());
		
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
	public String createMarkers(OntModel model, JSONObject jo){
		ArrayList<String> textcomb=new ArrayList<String>();
		Query fcQu = FCQuerySource.getFCQuery().build();
		String FCQuery = fcQu.toString();
		List<String[]> foodcourts = FCQuerySource.queryResult(model, FCQuery); //hard assumption that there would be foodcourts all the time
		for (int i = 0; i < foodcourts.size(); i++) {
			JSONObject jo2 = new JSONObject();
			jo2.put("lat", foodcourts.get(i)[3]);
			jo2.put("lng", foodcourts.get(i)[2]);
			JSONObject jo3 = new JSONObject().put("coors", jo2)
					.put("entity", foodcourts.get(i)[0]).put("name", foodcourts.get(i)[1]);
			textcomb.add(jo3.toString());
		}
		ArrayList<String> textcomb2 = generalWasteSiteTreatmentSearch(model, FCQuerySource.getOffsiteWasteTreatmentQuery());
		textcomb.addAll(textcomb2);
		JSONArray jsArray = new JSONArray(textcomb);
	    jo.put("result", jsArray);
		return jo.toString();
	}
	
	/** read the values of the tech input costs
	 * returns a JSON object of onsite and offsite
	 * @param model
	 * @return
	 */
	public String readInputs(OntModel model) {
		String WTFTechOffsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OffsiteWasteTreatmentFacility").buildString();
		List<String[]> resultList = FCQuerySource.queryResult(model, WTFTechOffsiteQuery);
		String WTFTechOnsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility")
				.addWhere("?Tech1" ,"a", "j1:OnSiteDigester").buildString();

		List<String[]> resultList2 = FCQuerySource.queryResult(model, WTFTechOnsiteQuery);
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
	/** reads the topnode into an OntModel of all its subsystems. 
	 * @param iriofnetwork
	 * @return
	 */
	public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the offsite wtf, transportation and food court
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:hasSubsystem", "?component");
		String wasteInfo = sb.build().toString();
		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, wasteInfo);
	}
}
