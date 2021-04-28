package uk.ac.cam.cares.jps.ess;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;

@WebServlet(urlPatterns = { "/OptimizationAgent"})
/** returns appropriate Battery Agent based on criteria
 * There's only one return (based on total losses, given by locateBattery)
 * But more options should be given. 
 *
 */
public class OptimizationAgent extends JPSAgent {
	
	private static final long serialVersionUID = 1L;
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    if (!validateInput(requestParams)) {
			throw new BadRequestException("ESSOptimizationAgent: Input parameters not found.\n");
		}
		String path="JPS_ESS/LocateBattery"; //later can be queried from the agent descriptions
		String gencoordinate = new SelectBuilder()
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
				.addVar("?entity").addVar("?class").addVar("?parent")
				.addWhere("?entity", "a","?class")
				.addWhere("?entity","j6:hasStateOfCharge", "?dt")
				.addWhere("?class", "rdfs:subClassOf","?parent")
				.buildString();
		
		String batIRI=requestParams.getString("storage");		
		String localUrl = ScenarioHelper.cutHash(batIRI);
		localUrl = ResourcePathConverter.convert(localUrl);
		ResultSet resultSet = JenaHelper.queryUrl(localUrl, gencoordinate);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
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
            return false;
        }
        try {
	        String storageFormat = requestParams.getString("storage");
	        boolean q = InputValidator.checkIfValidIRI(storageFormat);
	        return q;
        }catch (JSONException ex) {
            return false;
        }
	}	
}
