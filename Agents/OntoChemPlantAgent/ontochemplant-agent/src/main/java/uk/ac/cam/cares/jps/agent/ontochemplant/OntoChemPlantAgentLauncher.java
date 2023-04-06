package uk.ac.cam.cares.jps.agent.ontochemplant;

import java.sql.SQLException;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


/**
 * This is the class with the main method, entry point of the compiled war file.
 */

@Controller
@WebServlet(urlPatterns = {OntoChemPlantAgentLauncher.URL_PATH})
public class OntoChemPlantAgentLauncher extends JPSAgent {

	public static final String URL_PATH = "/query";
	public static final String IRI = "iris";
	
	private static final String WRONG_INPUT_MSG = "Input parameters are incorrect.";
	private static final String NO_INPUT_MSG = "No input values found.";
	private static final String NO_IRI_MSG = "No CityObject IRI found in input.";

	/* 
	 * Input should CityObject IRI as a JSON object 
	 * Example input: {"iris":["http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/"]} 
	*/
	public JSONObject processRequestParameters(JSONObject requestParams){

		if (validateInput(requestParams)) {
			OntoChemPlantAgent ocp_agent = new OntoChemPlantAgent();
			try {
				return ocp_agent.createOntoChemPlantModel(requestParams);
			} catch (SQLException e){
				throw new JPSRuntimeException(e);
			}
		}
		else {
			throw new JPSRuntimeException(WRONG_INPUT_MSG);
		}
		
	}

	/* Validate the provided input*/
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
			throw new BadRequestException(NO_INPUT_MSG);
		}
		if (requestParams.has(IRI)) {
			String iri_value = (requestParams.getJSONArray(IRI)).getString(0);
			if (iri_value == null) {
				throw new BadRequestException(NO_IRI_MSG);
			}			
		}		
		return true;
	}
}


