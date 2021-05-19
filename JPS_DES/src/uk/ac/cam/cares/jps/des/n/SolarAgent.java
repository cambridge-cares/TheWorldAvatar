package uk.ac.cam.cares.jps.des.n;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/SolarAgent"})
public class SolarAgent extends JPSAgent {
	private static final String TWA_Ontology = "http://www.theworldavatar.com/ontology"; 
	private static final String TWA_upperlevel_system = TWA_Ontology+ "/ontocape/upper_level/system.owl#";
	private static final String TWA_Singapore = "http://www.theworldavatar.com/kb/sgp/singapore";
	private static final String TWA_POWSYSRealization = TWA_Ontology+ "/ontopowsys/PowSysRealization.owl#";
	private static final String TWA_POWSYSBEHAVIOR = TWA_Ontology + "/ontopowsys/PowSysBehavior.owl#";

	/** main method. Runs solar radiation data after creating solar constant csv
	 *
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject responseParams = requestParams;	
		if (!validateInput(requestParams)) {
			throw new BadRequestException();
		}
        String iriofnetwork = requestParams.optString("electricalnetwork", TWA_Singapore +"/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", TWA_Singapore +"/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", TWA_Singapore +"/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork); 
        provideGenlist( model, baseUrl);
        try {
        	String result = new DESAgentNew().runPythonScript("solarRadiation.py", baseUrl);
			
			responseParams.put("results", result);
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("");
		}
    	return requestParams;
    }
	
	/** uses Commercial Agent's validate Input method since they're 
	 * using the same variables
	 */
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
            return false;
        }
        try {
        String iriofnetwork = requestParams.getString("electricalnetwork");
        boolean q = InputValidator.checkIfValidIRI(iriofnetwork);

        String irioftempF=requestParams.getString("temperatureforecast");

        boolean e = InputValidator.checkIfValidIRI(irioftempF);
        String iriofirrF=requestParams.getString("irradiationforecast");
        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
        // Till now, there is no system independent to check if a file path is valid or not. 
        
        return q&e&r;
        } catch (JSONException ex) {
        	return false;
        }
    }
	
	/** Creates PVGenerator.csv which are constants for solar agent
	 * 
	 * @param model
	 * @param baseUrl
	 */
	public void provideGenlist(OntModel model, String baseUrl) { //for file "PV_parameters.csv"
		SelectBuilder sb = new SelectBuilder().addPrefix("j1",TWA_POWSYSRealization)
				.addPrefix("j2", TWA_upperlevel_system)
				.addPrefix("j8", TWA_Ontology +"/ontocape/material/phase_system/phase_system.owl#")
				.addPrefix("j9", TWA_POWSYSBEHAVIOR)
				.addVar("?alphaval").addVar("?aval").addVar("?ilval").addVar("?ioval")
				.addVar("?rsval").addVar("?rshval").addVar("?tcval").addVar("?gval")
				.addVar("?egval")
				.addWhere("?entity" ,"j1:hasTemperatureCoeffOfPower", "?tcoeff")
				.addWhere("?tcoeff" ,"j2:hasValue", "?vmax")
				.addWhere("?vmax" ,"j2:numericalValue", "?alphaval")
				.addWhere("?entity" ,"j2:hasProperty", "?a")
				.addWhere("?a" ,"j2:hasValue", "?va")
				.addWhere("?va" ,"j2:numericalValue", "?aval")
				
				.addWhere("?entity" ,"j1:hasRatedCurrent", "?il")
				.addWhere("?il" ,"a", "j1:OutputRatedCurrent")
				.addWhere("?il" ,"j2:hasValue", "?vil")
				.addWhere("?vil" ,"j2:numericalValue", "?ilval")				
				.addWhere("?entity" ,"j1:hasRatedCurrent", "?io")
				.addWhere("?io" ,"a", "j9:MinimumCurrent")
				.addWhere("?io" ,"j2:hasValue", "?vio")
				.addWhere("?vio" ,"j2:numericalValue", "?ioval")
				
				.addWhere("?entity" ,"j1:hasResistance", "?rs")
				.addWhere("?rs" ,"a", "j1:SeriesResistance")
				.addWhere("?rs" ,"j2:hasValue", "?vrs")
				.addWhere("?vrs" ,"j2:numericalValue", "?rsval")				
				.addWhere("?entity" ,"j1:hasResistance", "?rsh")
				.addWhere("?rsh" ,"a", "j1:ShuntResistance")
				.addWhere("?rsh" ,"j2:hasValue", "?vrsh")
				.addWhere("?vrsh" ,"j2:numericalValue", "?rshval")
				
				.addWhere("?entity" ,"j8:has_temperature", "?t")
				.addWhere("?t" ,"j2:hasValue", "?vt")
				.addWhere("?vt" ,"j2:numericalValue", "?tcval")
				
				.addWhere("?entity" ,"j1:hasBaseTestingIrradiance", "?g")
				.addWhere("?g" ,"j2:hasValue", "?vg")
				.addWhere("?vg" ,"j2:numericalValue", "?gval")
				.addWhere("?entity" ,"j1:hasMaterialBandGap", "?mbg")
				.addWhere("?mbg" ,"j2:hasValue", "?vmbg")
				.addWhere("?vmbg" ,"j2:numericalValue", "?egval")
				;
	
				
		Query q = sb.build();
		String newQ = q.toString();
		ResultSet resultSet = JenaHelper.query(model, newQ);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        String fuelCellcsv = MatrixConverter.fromArraytoCsv(resultList);
        new QueryBroker().putLocal(baseUrl + "/PVGenerator.csv", fuelCellcsv);
    }
}
