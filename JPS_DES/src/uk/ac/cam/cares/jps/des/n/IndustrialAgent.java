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

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/IndustrialAgent"})
public class IndustrialAgent {

	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	JSONObject responseParams = requestParams;	
    	QueryBroker broker= new QueryBroker();  
        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        String iriofBuilding=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/district/building/CommercialBuilding-001.owl");
        
        String cityIRI = requestParams.optString("cityIRI", "http://dbpedia.org/page/Singapore");
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        List<String[]>TempIrrad =  new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        
        //constants for Fuel Cell
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
		List<String[]> consumer = provideLoadFClist(model); // instance iri
        
        queryForChemicalConstants(model);
        queryForFuelCellConstants(model);
		return responseParams;
    }
	public void queryForChemicalConstants(OntModel model) {
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
		
				
				
	}
	public void queryForFuelCellConstants(OntModel model) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
				.addVar("?nocellval").addVar("?effval").addVar("?tvalmin").addVar("?tvalmax").addVar("?voltVal")
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
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
	}/** returns relevant parameters for Fuel Cell (Used by Fuel Agent)
     * 
     * @param model
     * @return
     */
    public static List<String[]> provideLoadFClist(OntModel model) {
        String fuelcellInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
                + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
                + "SELECT  ?nocellval ?effval ?tvalmin ?tvalmax "
                + "WHERE {?entity  a  j1:FuelCell  ."
                
        		+ "?entity j1:hasNumberOfCells ?no ."
        		+ "?no   j2:hasValue ?vno ."
        		+ "?vno   j2:numericalValue ?nocellval ."
        		
        		+ "?entity j9:hasEfficiency ?eff ."
        		+ "?eff   j2:hasValue ?veff ."
        		+ "?veff   j2:numericalValue ?effval ."
        		
        		+ "?entity j8:has_temperature ?t ."
        		+ "?t   j2:hasValue ?vt ."
        		+ "?vt   j5:upperLimit ?tvalmax ."
        		+ "?vt   j5:lowerLimit ?tvalmin ."

                + "}";
        List<String[]> resultListforcsv = new ArrayList<String[]>();
        ResultSet resultSet = JenaHelper.query(model, fuelcellInfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of query consumer= "+resultListfromquery.size());
		for (int d = 0; d < keys.length; d++) {
				String[] line0 = { keys[d], resultListfromquery.get(0)[d] };
				resultListforcsv.add(line0);
		}

        return resultListforcsv;
    }
}
