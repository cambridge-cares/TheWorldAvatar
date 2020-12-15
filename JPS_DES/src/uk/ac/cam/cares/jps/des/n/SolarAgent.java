package uk.ac.cam.cares.jps.des.n;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

public class SolarAgent extends JPSHttpServlet{

	@Override
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
 			
		JSONObject result=new JSONObject();
		JSONObject graData = requestParams.getJSONObject("directory");
		//getData();
		runPythonForSolar();
 	       
 			System.gc();
    	return requestParams;
    }
	private void runPythonForSolar() {
		
	}
	public static List<String[]> provideGenlist(OntModel model) { //for file "PV_parameters.csv"
        String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
                + "PREFIX j10:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "SELECT ?iscval ?vocval ?impval ?vmpval ?alphaval ?aval ?ilval ?ioval ?rsval ?rshval ?tcval ?gval ?egval "
                + "WHERE {?entity  a  j1:PhotovoltaicPanel  ."
                
                + "?entity   j1:hasMaterialBandGap ?mbg ."
                + "?mbg   j2:hasValue ?vmbg ."
                + "?vmbg   j2:numericalValue ?egval ."
                
                + "?entity   j1:hasBaseTestingIrradiance ?g ."
                + "?g   j2:hasValue ?vg ."
                + "?vg   j2:numericalValue ?gval ."
                
                + "?entity   j10:has_temperature ?t ."
                + "?t   j2:hasValue ?vt ."
                + "?vt   j2:numericalValue ?tcval ." 
                
                + "?entity   j2:hasProperty ?a ."
                + "?a   j2:hasValue ?va ."
                + "?va   j2:numericalValue ?aval ."
                
                + "?entity   j1:hasTemperatureCoeffOfPower ?tcoeff ."
                + "?tcoeff   j2:hasValue ?vtcoeff ."
                + "?vtcoeff   j2:numericalValue ?alphaval ."
//------------------------------------------------------------------------------                
                + "?entity   j1:hasRatedVoltage ?vmp ."
                + "?vmp a j9:MaximumVoltage ."
                + "?vmp   j2:hasValue ?vvmp ."
                + "?vvmp   j2:numericalValue ?vmpval ."
                
                + "?entity   j1:hasRatedVoltage ?voc ."
                + "?voc a j9:Voltage ."
                + "?voc   j2:hasValue ?vvoc ."
                + "?vvoc   j2:numericalValue ?vocval ."

                + "?entity   j1:hasResistance ?rs ."
                + "?rs a j1:SeriesResistance ."
                + "?rs  j2:hasValue ?vrs ."
                + "?vrs   j2:numericalValue ?rsval ."
                
                + "?entity   j1:hasResistance ?rsh ."
                + "?rsh a j1:ShuntResistance ."
                + "?rsh   j2:hasValue ?vrsh ."
                + "?vrsh   j2:numericalValue ?rshval ."

                + "?entity   j1:hasRatedCurrent ?imp ."
                + "?imp a j9:MaximumCurrent ."
                + "?imp   j2:hasValue ?vimp ."
                + "?vimp   j2:numericalValue ?impval ."
                
                + "?entity   j1:hasRatedCurrent ?isc ."
                + "?isc a j1:RatedCurrent ."
                + "?isc   j2:hasValue ?visc ."
                + "?visc   j2:numericalValue ?iscval ."
                
                + "?entity   j1:hasRatedCurrent ?il ."
                + "?il a j1:OutputRatedCurrent ."
                + "?il   j2:hasValue ?vil ."
                + "?vil   j2:numericalValue ?ilval ."
                
                + "?entity   j1:hasRatedCurrent ?io ."
                + "?io a j9:MinimumCurrent ."
                + "?io   j2:hasValue ?vio ."
                + "?vio   j2:numericalValue ?ioval ."

                + "}";

        List<String[]> resultListforcsv = new ArrayList<String[]>();
	
        
        ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        System.out.println("size of query= "+resultListfromquery.size());
		for (int d = 0; d < keys.length; d++) {
			if (!keys[d].contains("tcval")) {
				String[] line0 = { keys[d], resultListfromquery.get(0)[d] };
				resultListforcsv.add(line0);
			} else {
				Double value = Double.valueOf(resultListfromquery.get(0)[d]) + 273.15; // convert celcius to K
				String[] line0 = { keys[d], "" + value };
				resultListforcsv.add(line0);
			}

		}

        return resultListforcsv;
    }
}
