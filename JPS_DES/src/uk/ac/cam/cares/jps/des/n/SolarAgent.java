package uk.ac.cam.cares.jps.des.n;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/SolarAgent"})
public class SolarAgent extends JPSHttpServlet{

	@Override
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
		JSONObject responseParams = requestParams;	
        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork); 
        provideGenlist( model, baseUrl);
        try {
        	String result = new DESAgentNew().runPythonScript("solarRadiation.py", baseUrl);
			
			responseParams.put("results", result);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
    	return requestParams;
    }
	public void provideGenlist(OntModel model, String baseUrl) { //for file "PV_parameters.csv"
        String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
                + "SELECT ?alphaval ?aval ?ilval ?ioval ?rsval ?rshval ?tcval ?gval ?egval "
                + "WHERE {?entity  a  j1:PhotovoltaicPanel  ."
                
                + "?entity   j1:hasMaterialBandGap ?mbg ."
                + "?mbg   j2:hasValue ?vmbg ."
                + "?vmbg   j2:numericalValue ?egval ."
                
                + "?entity   j1:hasBaseTestingIrradiance ?g ."
                + "?g   j2:hasValue ?vg ."
                + "?vg   j2:numericalValue ?gval ."
                
                + "?entity   j8:has_temperature ?t ."
                + "?t   j2:hasValue ?vt ."
                + "?vt   j2:numericalValue ?tcval ." 
                
                + "?entity   j2:hasProperty ?a ."
                + "?a   j2:hasValue ?va ."
                + "?va   j2:numericalValue ?aval ."
                
                + "?entity   j1:hasTemperatureCoeffOfPower ?tcoeff ."
                + "?tcoeff   j2:hasValue ?vtcoeff ."
                + "?vtcoeff   j2:numericalValue ?alphaval ."

                + "?entity   j1:hasResistance ?rs ."
                + "?rs a j1:SeriesResistance ."
                + "?rs  j2:hasValue ?vrs ."
                + "?vrs   j2:numericalValue ?rsval ."
                
                + "?entity   j1:hasResistance ?rsh ."
                + "?rsh a j1:ShuntResistance ."
                + "?rsh   j2:hasValue ?vrsh ."
                + "?vrsh   j2:numericalValue ?rshval ."
                               
                + "?entity   j1:hasRatedCurrent ?il ."
                + "?il a j1:OutputRatedCurrent ."
                + "?il   j2:hasValue ?vil ."
                + "?vil   j2:numericalValue ?ilval ."
                
                + "?entity   j1:hasRatedCurrent ?io ."
                + "?io a j9:MinimumCurrent ."
                + "?io   j2:hasValue ?vio ."
                + "?vio   j2:numericalValue ?ioval ."

                + "}";
        
        ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        String fuelCellcsv = MatrixConverter.fromArraytoCsv(resultList);
        new QueryBroker().putLocal(baseUrl + "/PVGenerator.csv", fuelCellcsv);
    }
}
