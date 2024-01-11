package uk.ac.cam.cares.jps.agent.heat;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import com.jayway.jsonpath.JsonPath;

/** 
 * ---------------------------------- Heat emission agent ------------------------------------------
 * 
 * This agent is to return the heat emission information of different buildings/objects presented
 * in a given area. Therefore, the heat emission data can be automatically assigned to corresponding
 * buildings within a specific area. This is of interest to the Cooling Singapore 2.0 Project. 
 * 
 * This class file demonstrates (1) the feasibility of a cross-domain query and (2) the evaluation of 
 * the heat emission data in terms of emission values and respective coordinates within a bounding 
 * box in Jurong Island. To achieve this, it consists of four parts. First, we obtain all the chemical 
 * plants, plant items, IRIs and CO2 emission via query in "jibusinessunits"; Second, for a particular
 * chemical plant, its fuel CEI and efficiency are queried; then, all the heat emission coordinates 
 * are evaluated via query in "jriEPSG24500"; finally, the heat emission values are calculated with
 * CO2 emission, CEI and efficiency and assigned to the emission coordinates, after a filter based on
 * a boundary area specified. 
 * 
 * @author Hansong Xue
 *
 *------------------------------------------------------------------------------------------------
 */

/**
 * Servlet implementation class HeatEmissionAgent; URL pattern to execute the
 * agent: <http://
 * www.theworldavatar.com/Agents/HeatEmissionAgent/performheatquery>
 */

@WebServlet(urlPatterns = { HeatEmissionAgent.URL_PATH })

public class HeatEmissionAgent extends JPSAgent {

	public static final String URL_PATH = "/performheatquery";
	// Display messages
	private static final String BAD_INPUT = "Error in input parameters, please check the" +
			" input file";

	// Receive input as JSON objects, execute the agent and return the results as
	// JSON object as well
	// Pass the method "HeatEmissionQuery" to execute the actual query
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (validateInput(requestParams)) {
			if (requestParams.getString("ontology").equalsIgnoreCase("ontochemplant")) {
				JurongIsland ji = new JurongIsland();
				return ji.calculateHeat(requestParams);
			} else {
				Mainland ml = new Mainland(requestParams);
				return ml.calculateHeat();
			}

		} else {
			System.out.println("bad input.\n");
			throw new JPSRuntimeException(BAD_INPUT);
		}
	}

	// Validate the input parameters and check if all the necessary parameters are
	// provided or not
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
			throw new BadRequestException();
		}

		if (!requestParams.has("ontology"))
			throw new BadRequestException(
					"Either ontochemplant or ontocompany must be specified as the value of the ontology parameter.");

		if (requestParams.getString("ontology").equalsIgnoreCase("ontochemplant")) {
			String UPPER_LIMITS = JsonPath.read(requestParams.toString(), "$.upper_bounds");
			if (UPPER_LIMITS == null || UPPER_LIMITS.trim().isEmpty()) {
				throw new BadRequestException("Upper limits for the bounding box are missing.\n");
			}
			String LOWER_LIMITS = JsonPath.read(requestParams.toString(), "$.lower_bounds");
			if (LOWER_LIMITS == null || LOWER_LIMITS.trim().isEmpty()) {
				throw new BadRequestException("Lower limits for the bounding box are missing.\n");
			}
		} else if (!requestParams.getString("ontology").equalsIgnoreCase("ontocompany")) {
			throw new BadRequestException(
					"The value of the ontology parameter must be either ontocompany or ontochemplant");
		}

		if (!requestParams.has("endpoint") && !requestParams.has("namespace")) {
			throw new BadRequestException(
					"Either a blazegraph namespace or endpoint must be specified when running the heat emisson agent.\n");

		}

		return true;
	}

}
