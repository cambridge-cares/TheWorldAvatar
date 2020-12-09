package uk.ac.cam.cares.jps.des.n;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = { "/DESAgentNew"})

public class DESAgentNew extends JPSHttpServlet {
	public String cityIRI;
	public static String producerdata="PV_parameters.csv";
	public static String consumerdata1="FuelCell.csv";
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String bcap="bcap.csv";
	public static String unwill="unwill.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response, JSONObject reqBody) throws IOException, ServletException {
        logger = LoggerFactory.getLogger( DESAgentNew.class);
        super.doHttpJPS(request, response, reqBody);
    }
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	JSONObject responseParams = requestParams;	
    	QueryBroker broker= new QueryBroker();  
        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        cityIRI = requestParams.optString("cityIRI", "http://dbpedia.org/page/Singapore");
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = readModelGreedy(iriofnetwork);
		List<String[]> producer = providePVParameters(model); // create Parameters for Solar Cell
		List<String[]> consumer = new IndustrialAgent().provideLoadFClist(model); // instance iri
		String producercsv = MatrixConverter.fromArraytoCsv(producer);
		String consumercsv = MatrixConverter.fromArraytoCsv(consumer);
		broker.putLocal(baseUrl + "/"+producerdata, producercsv); //csv for pv
		broker.putLocal(baseUrl + "/"+consumerdata1, consumercsv); //csv for fuelcell
		new ResidentialAgent().extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		try {
			runPythonScript("system.py", baseUrl);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		return responseParams;
    }
    /** Query OWL for Temperature and Radiation readings and place in csv file, return as List<String[]> of Temp followed by Irrad
     * 
     * @param irioftempF
     * @param iriofirrF
     * @param baseUrl location of folder where csvs are dumped
     */
    public List<String[]> queryForIrradTemp(String irioftempF, String iriofirrF, String baseUrl){
   	    	QueryBroker broker= new QueryBroker();  
	       String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
					+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" ;

			
			String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
					+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" ;

			
 			//grab forecast results
			String result = new QueryBroker().queryFile(irioftempF, sensorinfo+ "ORDER BY ASC(?proptimeval)");
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]>  resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
 			System.out.println("Temperature " + resultListfromquerytemp.toString());
 			String result2 = new QueryBroker().queryFile(iriofirrF, sensorinfo2+ "ORDER BY ASC(?proptimeval)");
 			String[] keys2 = JenaResultSetFormatter.getKeys(result2);
 			List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
 			System.out.println("Irradiation " + resultListfromqueryirr.toString());
 			List<String[]> readingFromCSV = new ArrayList<String[]>();
 			for (int d=0;d<resultListfromqueryirr.size();d++) {
 				String[] ed= {resultListfromquerytemp.get(d)[1],resultListfromqueryirr.get(d)[1]};
 				readingFromCSV.add(ed);
 			}
 			broker.putLocal(baseUrl + "/WeatherForecast.csv", MatrixConverter.fromArraytoCsv(readingFromCSV));
 			return readingFromCSV;
    }
    /** find all subsystems that are linked to top node
     * 
     * @param iriofnetwork
     * @return
     */
    public static OntModel readModelGreedy(String iriofnetwork) {
		String nodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {"
				+ "?entity   j2:hasSubsystem ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, nodeInfo);
	}
    
	/** find individual components (for residential) of appliances
	 * 
	 * @param useriri
	 * @return
	 */
    public static OntModel readModelGreedyForUser(String useriri) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "SELECT ?component " + "WHERE { " + "?entity   j2:isConnectedTo ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(useriri, electricalnodeInfo);
	}
    /** returns relevant parameters for PV Cell (Used by SolarRadiation Agent)
     * 
     * @param model OntModel of IRI of Electrical Network
     * @return
     */
    public static List<String[]> providePVParameters(OntModel model) { //for file "PV_parameters.csv"
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
                + "SELECT ?ilval ?alphaval ?aval  ?ioval ?rsval ?rshval ?tcval ?gval ?egval "
                + "WHERE {?entity  a  j1:PhotovoltaicPanel  ."

                + "?entity   j1:hasRatedCurrent ?isc ."
                + "?isc a j1:RatedCurrent ."
                + "?isc   j2:hasValue ?visc ."
                + "?visc   j2:numericalValue ?iscval ."
                
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
    /** run Python Script in folder
	 * 
	 * @param script
	 * @param folder
	 * @return
	 * @throws Exception
	 */
	public String runPythonScript(String script, String folder) throws Exception {
		String result = "";
			String path = AgentLocator.getCurrentJpsAppDirectory(this);
			String command = "python " + path+ "/python/" +script + " " + folder;
			System.out.println(command);
			result = CommandHelper.executeSingleCommand( path, command);
		
		
			return result;
	}
    
}
