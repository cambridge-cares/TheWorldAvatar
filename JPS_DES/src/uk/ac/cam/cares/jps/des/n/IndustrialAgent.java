package uk.ac.cam.cares.jps.des.n;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/IndustrialAgent"})
public class IndustrialAgent {

	private JSONObject responseParams;
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        
        //constants for Fuel Cell
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork); 
        queryForChemicalConstants(model, baseUrl);
        queryForFuelCellConstants(model, baseUrl);
        JSONObject responseParams = new JSONObject();
		try {
			String res =  new DESAgentNew().runPythonScript("industrial.py", baseUrl);
			
			responseParams.put("results", res);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
		return responseParams;
    }
	public void queryForChemicalConstants(OntModel model, String baseUrl) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
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
