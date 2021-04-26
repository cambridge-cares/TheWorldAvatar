package uk.ac.cam.cares.jps.des.n;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
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
@WebServlet(urlPatterns = {"/IndustrialAgent"})
public class IndustrialAgent extends JPSAgent {
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    if (!validateInput(requestParams)) {
    		throw new BadRequestException("IndustrialAgent:  Input parameters not found.\n");
    	}
    	String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        
        //constants for Fuel Cell
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork); 
        queryForConstantsIndustrial(model, baseUrl);
        JSONObject responseParams = new JSONObject();
		try {
			String res =  new DESAgentNew().runPythonScript("industrial.py", baseUrl);
			
			responseParams.put("results", res);
			return responseParams;
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("Industrial Agent: Incomplete simulation.\n ");
		}
    }
	
	/** uses Commercial Agent's validate Input method since they're 
	 * using the same variables
	 */
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
        String iriofnetwork = requestParams.getString("electricalnetwork");
        boolean q = InputValidator.checkIfValidIRI(iriofnetwork);

        String irioftempF=requestParams.getString("temperatureforecast");

        boolean e = InputValidator.checkIfValidIRI(irioftempF);
        String iriofirrF=requestParams.getString("irradiationforecast");
        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
        return q&e&r;
        } catch (JSONException ex) {
        	return false;
        }
    }
	
	/** sub method to call on Chemical and Fuel Cell constants
	 * 
	 * @param model
	 * @param baseUrl
	 */
	public void queryForConstantsIndustrial(OntModel model, String baseUrl) {
		queryForChemicalConstants(model, baseUrl);
        queryForFuelCellConstants(model, baseUrl);
        
	}
	
	/** Creates ElectrolyzerConstant.csv for IndustrialAgent to run 
	 * Queries OntModel for electrolyzer parameters, switch to KBAgent if applicable
	 * @param model
	 * @param baseUrl
	 */
	public void queryForChemicalConstants(OntModel model, String baseUrl) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addVar("?max").addVar("?tvalmax")
				.addWhere("?entity" ,"a", "j1:Electrolizer")
				.addWhere("?entity" ,"j2:hasProperty", "?max")
				.addWhere("?max" ,"j2:hasValue", "?vmax").addWhere("?vmax" ,"j2:numericalValue", "?tvalmax")
				.addOrderBy("?max")
				.addUnion( new WhereBuilder()
						.addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
						.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
						.addWhere("?entity" ,"a", "j1:Electrolizer")
						.addWhere( "?entity" ,"j1:hasResistance", "?max")
				        .addWhere("?max" ,"j2:hasValue", "?vmax").addWhere("?vmax" ,"j2:numericalValue", "?tvalmax")
				    );
		Query q = sb.build();
		String groupInfo = q.toString();
		ResultSet resultSet = JenaHelper.query(model, groupInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		String electrolycsv = MatrixConverter.fromArraytoCsv(resultList);
		new QueryBroker().putLocal(baseUrl + "/ElectrolyzerConstant.csv", electrolycsv);
		
				
	}
	
	/** Creates FuelCell.csv for IndustrialAgent to run 
	 * Queries OntModel for fuel cell parameters, switch to KBAgent if applicable
	 * @param model
	 * @param baseUrl
	 */
	public void queryForFuelCellConstants(OntModel model, String baseUrl) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
				.addVar("?nocellval").addVar("?effval").addVar("?tvalmin").addVar("?tvalmax").addVar("?voltVal").addVar("?currVal")
				.addWhere("?entity" ,"a", "j1:FuelCell").addWhere("?entity" ,"j1:hasNumberOfCells", "?no")
				.addWhere("?no" ,"j2:hasValue", "?vno").addWhere("?vno" ,"j2:numericalValue", "?nocellval")
				
				.addWhere("?entity" ,"j9:hasEfficiency", "?eff")
				.addWhere("?eff" ,"j2:hasValue", "?veff").addWhere("?veff" ,"j2:numericalValue", "?effval")
				
				.addWhere("?entity" ,"j2:hasProperty", "?max").addWhere("?max" ,"a", "j2:MaximumDesignTemperature")
				.addWhere("?max" ,"j2:hasValue", "?vmax").addWhere("?vmax" ,"j2:numericalValue", "?tvalmax")
				
				.addWhere("?entity" ,"j2:hasProperty", "?min").addWhere("?min" ,"a", "j2:MinimumDesignTemperature")
				.addWhere("?min" ,"j2:hasValue", "?vmin").addWhere("?vmin" ,"j2:numericalValue", "?tvalmin")
				
				.addWhere("?entity" ,"j9:hasVoltageOutput", "?volt")
				.addWhere("?volt" ,"j2:hasValue", "?vVolt").addWhere("?vVolt" ,"j2:numericalValue", "?voltVal")
		
				.addWhere("?entity" ,"j9:hasVoltageOutput", "?curr")
				.addWhere("?curr" ,"j2:hasValue", "?vCurr").addWhere("?vCurr" ,"j2:numericalValue", "?currVal");
		Query q = sb.build();
		String groupInfo = q.toString();
		ResultSet resultSet = JenaHelper.query(model, groupInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList  = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

        String fuelCellcsv = MatrixConverter.fromArraytoCsv(resultList);
        new QueryBroker().putLocal(baseUrl + "/FuelCell.csv", fuelCellcsv);
	}
}
