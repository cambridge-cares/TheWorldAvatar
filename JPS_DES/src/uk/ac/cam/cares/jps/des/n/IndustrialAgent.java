package uk.ac.cam.cares.jps.des.n;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
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
        
        queryForPhysicalConstants();
		return responseParams;
    }
	
	public void queryForPhysicalConstants() {
		
	}/** returns relevant parameters for Fuel Cell (Used by Fuel Agent)
     * 
     * @param model
     * @return
     */
    public static List<String[]> provideLoadFClist(OntModel model) {
        String fuelcellInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
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
