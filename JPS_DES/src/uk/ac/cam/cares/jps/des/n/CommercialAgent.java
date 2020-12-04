package uk.ac.cam.cares.jps.des.n;

import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.ArrayUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/CommercialAgent"})
public class CommercialAgent {
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	JSONObject responseParams = requestParams;	
    	QueryBroker broker= new QueryBroker();  
        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        String iriofBuilding=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/district/building/CommercialBuilding-001.owl");
        
        String cityIRI = requestParams.optString("cityIRI", "http://dbpedia.org/page/Singapore");
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = new DESAgentNew().readModelGreedy(iriofnetwork);
        queryForBuildingConstants(model);
		return responseParams;
    }
	
	public void queryForBuildingConstants(OntModel model ) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("plant", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#")
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addVar("?propVal").addWhere("?entity" ,"a", "j6:Building")
				.addWhere("?entity" ,"plant:hasCapacity", "?capacity").addWhere("?capacity" ,"j2:hasValue", "?vProp")
				.addWhere("?vProp" ,"j2:numericalValue", "?propVal").addOrderBy("?capacity");
		Query q = sb.build();
		ResultSet resultSetx = JenaHelper.query(model, q.toString());
		String resultListC = JenaResultSetFormatter.convertToCSV(resultSetx);
		
		sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("phase_system", "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#")
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
				.addVar("?propVal").addWhere("?entity" ,"a", "j6:Building")
				.addWhere("?entity" ,"j2:hasProperty", "?prop").addWhere("?prop" ,"j2:hasValue", "?vProp")
				.addWhere("?prop" ,"rdf:type", "phase_system:ThermalConductivity").addWhere("?vProp" ,"j2:numericalValue", "?propVal")
				.addOrderBy("?prop");
		q = sb.build();
		resultSetx = JenaHelper.query(model, q.toString());
		String resultListK = JenaResultSetFormatter.convertToCSV(resultSetx);
		String[] csv = resultListC.split("\r\n");
		String[] csv2 = resultListK.split("\r\n");
		String[] csvNew =  (String[]) ArrayUtils.addAll(Arrays.copyOfRange(csv, 1, csv.length), Arrays.copyOfRange(csv2, 1, csv2.length));
//		System.out.println(csv+csv2);
		return;
		 
	}
}
