package uk.ac.cam.cares.jps.des.n;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.lang.ArrayUtils;
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
@WebServlet(urlPatterns = {"/CommercialAgent"})
public class CommercialAgent extends JPSAgent {
	/** Main Function for processing Commercial Agent. 
	 * Employs queryForWeather Forecast as well as building constants. 
	 * @param requestParams
	 * @param request
	 * @return
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject responseParams = requestParams;	
    	if (!validateInput(requestParams)) {
    		throw new BadRequestException();
    	}
        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
		
        queryForBuildingConstants(model, baseUrl);
        try {
			String res =  new DESAgentNew().runPythonScript("commercial.py", baseUrl);
			
			responseParams.put("results", res);
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("");
		}
		return responseParams;
    }
	
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
	
	/** queries dynamically the Electrical network for Commercial Constants to be used by model
	 * 
	 * @param model OntModel of Electrical Network
	 * @param baseUrl Folder in which constant.csv is dumped in
	 */
	public void queryForBuildingConstants(OntModel model, String baseUrl) {
        SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("plant", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#")
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addVar("?CpropVal").addWhere("?entity" ,"a", "j6:Building")
				.addWhere("?entity" ,"plant:hasCapacity", "?capacity").addWhere("?capacity" ,"j2:hasValue", "?vProp")
				.addWhere("?vProp" ,"j2:numericalValue", "?CpropVal").addOrderBy("?capacity");
		Query q = sb.build();
		ResultSet resultSetx = JenaHelper.query(model, q.toString());
		String resultListC = JenaResultSetFormatter.convertToCSV(resultSetx);
		
		sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("phase_system", "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#")
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
				.addVar("?KpropVal").addWhere("?entity" ,"a", "j6:Building")
				.addWhere("?entity" ,"j2:hasProperty", "?prop").addWhere("?prop" ,"j2:hasValue", "?vProp")
				.addWhere("?prop" ,"rdf:type", "phase_system:ThermalConductivity").addWhere("?vProp" ,"j2:numericalValue", "?KpropVal")
				.addOrderBy("?prop");
		q = sb.build();
		resultSetx = JenaHelper.query(model, q.toString());
		String resultListK = JenaResultSetFormatter.convertToCSV(resultSetx);
		String[] csv = resultListC.split("\r\n");
		String[] csv2 = resultListK.split("\r\n");
		List<String[]> csvConstant = new ArrayList<String[]>();
		String[] csvNew =  (String[]) ArrayUtils.addAll(Arrays.copyOfRange(csv, 1, csv.length), Arrays.copyOfRange(csv2, 1, csv2.length));
		csvConstant.add(csvNew);
		new QueryBroker().putLocal(baseUrl + "/constant.csv", MatrixConverter.fromArraytoCsv(csvConstant)); 
	}
}
