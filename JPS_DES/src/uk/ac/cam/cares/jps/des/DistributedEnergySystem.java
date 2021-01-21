package uk.ac.cam.cares.jps.des;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.StringJoiner;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;




@WebServlet(urlPatterns = { "/DESAgent"})
/** Old method for DES; no longer used. 
 * Wrapper for the python agent and displaying the result
 */
public class DistributedEnergySystem extends JPSHttpServlet {
	public static final String SIM_START_PATH = "/DESAgent";
	public static String schedule="ApplianceScheduleLoad1.csv";
	
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String bcap="bcap.csv";
	public static String unwill="unwill.csv";
	
	public static String producerdata="PV_parameters.csv";
	public static String consumerdata1="FuelCell.csv";
	public String cityIRI;
	
	
    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response);
    }

    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response, JSONObject reqBody) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response, reqBody);
    }

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	 JSONObject responseParams = requestParams;
    	    QueryBroker broker= new QueryBroker();  	
 	        String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
 	        String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
 	        String irioftemp=requestParams.optString("temperaturesensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
 	        String iriofirr=requestParams.optString("irradiationsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
 	        String iriofwind=requestParams.optString("windspeedsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
 	        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
 	        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
 	        String iriofwindF=requestParams.optString("windspeedforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedForecast-001.owl#SGWindSpeedForecast-001");

	        cityIRI = requestParams.optString("cityIRI", "http://dbpedia.org/page/Singapore");
	        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
 	        	
 	        //Query latest real time results
 	        
 	       String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
 					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
 					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
 					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
 					+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
 					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
 					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" ;

 			String result = new QueryBroker().queryFile(irioftemp, sensorinfo+ "ORDER BY DESC(?proptimeval) LIMIT 1");
 			String[] keys = JenaResultSetFormatter.getKeys(result);
 			List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

 			String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
 					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
 					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
 					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
 					+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
 					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
 					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}" ;

 			String result2 = new QueryBroker().queryFile(iriofirr, sensorinfo2+ "ORDER BY DESC(?proptimeval) LIMIT 1");
 			String[] keys2 = JenaResultSetFormatter.getKeys(result2);
 			List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
 			
 			String sensorinfo3 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
 					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
 					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
 					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
 					+ "WHERE { ?entity a j5:F-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
 					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
 					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." + "}";

 			String result3 = new QueryBroker().queryFile(iriofwind, sensorinfo3 + "ORDER BY DESC(?proptimeval) LIMIT 1");
 			String[] keys3 = JenaResultSetFormatter.getKeys(result3);
 			List<String[]> resultListfromqueryspeed = JenaResultSetFormatter.convertToListofStringArrays(result3, keys3);
 			
 			List<String[]> readingFromCSV = new ArrayList<String[]>();
 			String timewholecsv=resultListfromquerytemp.get(0)[2];
 			String datemonthcsv=timewholecsv.split("-")[2].split("T")[0]+"-"+timewholecsv.split("-")[1];			
 			String timecsv=timewholecsv.split("-")[2].split("T")[1].split("\\+")[0];
 			String[]e= {timewholecsv.split("-")[0],datemonthcsv,timecsv,"100",resultListfromquerytemp.get(0)[1],"74.9",resultListfromqueryspeed.get(0)[1],"115.7",resultListfromqueryirr.get(0)[1],"0"};
 			readingFromCSV.add(e); 			
 			broker.putLocal(baseUrl + "/WeatherActual.csv", MatrixConverter.fromArraytoCsv(readingFromCSV));
 			
 			//grab forecast results
 			result = new QueryBroker().queryFile(irioftempF, sensorinfo+ "ORDER BY ASC(?proptimeval)");
 			keys = JenaResultSetFormatter.getKeys(result);
 			resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
 			System.out.println("Temperature " + resultListfromquerytemp.toString());
 			result2 = new QueryBroker().queryFile(iriofirrF, sensorinfo2+ "ORDER BY ASC(?proptimeval)");
 			keys2 = JenaResultSetFormatter.getKeys(result2);
 			resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
 			System.out.println("Irradiation " + resultListfromqueryirr.toString());
 			result3 = new QueryBroker().queryFile(iriofwindF, sensorinfo3 + "ORDER BY ASC(?proptimeval)");
 			keys3 = JenaResultSetFormatter.getKeys(result3);
 			resultListfromqueryspeed = JenaResultSetFormatter.convertToListofStringArrays(result3, keys3);
 			System.out.println("Speed " +resultListfromqueryspeed);
 			readingFromCSV = new ArrayList<String[]>();
 			for (int d=0;d<resultListfromqueryirr.size();d++) {
 				timewholecsv=resultListfromquerytemp.get(d)[2];
 				datemonthcsv=timewholecsv.split("-")[2].split("T")[0]+"-"+timewholecsv.split("-")[1];			
 				timecsv=timewholecsv.split("-")[2].split("T")[1].split("\\+")[0];
 				String[] ed= {timewholecsv.split("-")[0],datemonthcsv,timecsv,"100",resultListfromquerytemp.get(d)[1],"74.9",resultListfromqueryspeed.get(d)[1],"115.7",resultListfromqueryirr.get(d)[1],"0"};
 				readingFromCSV.add(ed);
 			}
 			broker.putLocal(baseUrl + "/WeatherForecast.csv", MatrixConverter.fromArraytoCsv(readingFromCSV));
 			
 			//grab initialization for calibrating
	        File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/resources/" + "WeatherInitialize.csv");
    		String destinationUrl = baseUrl + "/WeatherInitialize.csv";
    		broker.putLocal(destinationUrl, file);
    		
    		OntModel model = readModelGreedy(iriofnetwork);
    		List<String[]> producer = provideGenlist(model); // instance iri
    		List<String[]> consumer = provideLoadFClist(model); // instance iri
    		extractResidentialData(iriofdistrict, baseUrl); //csv for residential
    		String producercsv = MatrixConverter.fromArraytoCsv(producer);
    		broker.putLocal(baseUrl + "/"+producerdata, producercsv); //csv for pv

    		String consumercsv = MatrixConverter.fromArraytoCsv(consumer);
    		broker.putLocal(baseUrl + "/"+consumerdata1, consumercsv); //csv for fuelcell
    		
    		
    			try {
    				runOptimization(baseUrl);
    			} catch (IOException ex) {
    				// TODO Auto-generated catch block
    				logger.error(ex.getMessage());
    				ex.printStackTrace();
    			} catch (InterruptedException ex) {
    				// TODO Auto-generated catch block
    				logger.error(ex.getMessage());
    				ex.printStackTrace();
    			} catch (Exception ex) {
    				// TODO Auto-generated catch block
    				logger.error(ex.getMessage());
    				ex.printStackTrace();
    			}
    	
    		
		return responseParams;
    	
    }
    
    public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {"
				//+ "?entity  a  j2:CompositeSystem  ." 
				+ "?entity   j2:hasSubsystem ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
    public static OntModel readModelGreedyForUser(String useriri) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "SELECT ?component " + "WHERE { " + "?entity   j2:isConnectedTo ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(useriri, electricalnodeInfo);
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
                + "SELECT ?enumber ?nocellval ?effval ?tvalmin ?tvalmax "
                + "WHERE {?entity  a  j1:FuelCell  ."
                + "?entity j4:realizes ?proc ."
                + "?proc j2:hasProperty ?propproc ."
                + "?propproc j2:hasValue ?vpropproc ."
                + "?vpropproc j2:numericalValue ?enumber ."
                
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
    
	public void extractResidentialData(String iriofnetworkdistrict, String baseUrl) {
		OntModel model = readModelGreedy(iriofnetworkdistrict);
		String groupInfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "SELECT DISTINCT ?entity (COUNT(?entity) AS ?group) ?propval ?user " + "WHERE {"
				+ "{ ?entity a j6:Building ." + "  ?entity j2:hasProperty ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + "?entity j4:isComprisedOf ?user ." + "}"
				+ "FILTER regex(STR(?user),\"001\") ." + "}" + "GROUP BY ?entity ?propval ?user ";

		String groupInfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "SELECT DISTINCT ?entity (COUNT(?entity) AS ?group) " + "WHERE " + "{ ?entity a j6:Building ."
				+ "?entity j4:isComprisedOf ?user ."

				+ "}"

				+ "GROUP BY ?entity ";

		String equipmentinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j7:<http://www.w3.org/2006/time#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "SELECT ?entity ?Pmaxval ?Pminval ?unwillval ?Pactval ?hourval " + "WHERE "
				+ "{ ?entity a j6:Electronics ." + "?entity j9:hasActivePowerAbsorbed ?Pmax ."
				+ "?Pmax a j9:MaximumActivePower ." + " ?Pmax   j2:hasValue ?vPmax ."
				+ " ?vPmax   j2:numericalValue ?Pmaxval ."

				+ "  ?entity j2:hasProperty ?prop ." + "?prop a j6:IdealityFactor ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?unwillval ."

				+ "?entity j9:hasActivePowerAbsorbed ?Pmin ." + "?Pmin a j9:MinimumActivePower ."
				+ " ?Pmin   j2:hasValue ?vPmin ." + " ?vPmin   j2:numericalValue ?Pminval ."

				+ "?entity j9:hasActivePowerAbsorbed ?Pact ." + "?Pact a j9:AbsorbedActivePower ."
				+ " ?Pact   j2:hasValue ?vPact ." + " ?vPact   j2:numericalValue ?Pactval ."
				+ " ?vPact   j7:hasTime ?proptime ." + "?proptime j7:hour ?hourval ."

				+ "}" + "ORDER BY ASC(?hourval)";

		// ?user ?user ?equipment

		ResultSet resultSet = JenaHelper.query(model, groupInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		System.out.println("sizeofresult=" + resultList.size());
		int size = resultList.size();
		List<String> iriofgroupuser = new ArrayList<String>();
		List<String[]> csvofbcap = new ArrayList<String[]>();
		QueryBroker broker = new QueryBroker();
		for (int d = 0; d < size; d++) {
			for (int t = 0; t < keys.length; t++) {
				// System.out.println("elementonquery1 "+t+"= "+resultList.get(d)[t]);
				if (t == 3) {
					iriofgroupuser.add(resultList.get(d)[t]);
				}

			}
			String[] e = { resultList.get(d)[3], resultList.get(d)[2] };
			csvofbcap.add(e);

		}
		Collections.sort(csvofbcap, new Comparator<String[]>() {
			public int compare(String[] strings, String[] otherStrings) {
				return strings[0].compareTo(otherStrings[0]);
			}
		});
		String bcapcsv = MatrixConverter.fromArraytoCsv(csvofbcap);
		System.out.println(bcapcsv);
		broker.putLocal(baseUrl + "/" + bcap, bcapcsv);

		// part 2 to see how many multiplication factor
		ResultSet resultSet2 = JenaHelper.query(model, groupInfo2);
		String result2 = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultList2 = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		System.out.println("sizeofresult=" + resultList2.size());
		int size2 = resultList2.size();
		for (int d = 0; d < size2; d++) {
			for (int t = 0; t < keys2.length; t++) {
				System.out.println("elementonquery2 " + t + "= " + resultList2.get(d)[t]);
			}
			System.out.println("---------------------------------------");

		}

		int sizeofiriuser = iriofgroupuser.size();
		Collections.sort(iriofgroupuser);
		System.out.println("sizeofiriuser=" + sizeofiriuser);
		List<String[]> csvofpmax = new ArrayList<String[]>();
		List<String[]> csvofpmin = new ArrayList<String[]>();
		List<String[]> csvofw = new ArrayList<String[]>();
		List<String[]> csvofschedule = new ArrayList<String[]>();
		List<String> header = new ArrayList<String>();
		header.add("");
		String[] timeschedu = {"t1","t2", "t3", "t4", "t5","t6","t7", "t8", "t9", "t10","t11","t12", "t13", "t14", "t15","t16","t17", "t18", "t19", "t20","t21","t22", "t23", "t24"};
		//grab the current time
		List<String> lst = Arrays.asList(timeschedu);
		Date date = new Date();   // given date
		Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
		calendar.setTime(date);   // assigns calendar to given date 
		int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
		
		//rotate it according to the current hour to get the appropriate profile
		Collections.rotate(lst, h);
		for(int x=1;x<=sizeofiriuser;x++) {
			List<String[]> subList =  new ArrayList<String[]>();
			OntModel model2 = readModelGreedyForUser(iriofgroupuser.get(x-1));
			ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
			String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
			String[] keysx = JenaResultSetFormatter.getKeys(resultx);
			List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);
			System.out.println("sizeofresult="+resultListx.size());

			List<String>groupPmax=new ArrayList<String>();
			groupPmax.add(iriofgroupuser.get(x-1));
			List<String>groupPmin=new ArrayList<String>();
			groupPmin.add(iriofgroupuser.get(x-1));
			List<String>groupw=new ArrayList<String>();
			groupw.add(iriofgroupuser.get(x-1));
			List<String>groupschedule=new ArrayList<String>();
			groupschedule.add(iriofgroupuser.get(x-1));
			//Set to ensure no repeats
			int countr = 1; 
			groupschedule.add(lst.get(0));
			for(int d=0;d<resultListx.size();d++) {
					if(resultListx.get(d)[5].contentEquals("1")) {
						//System.out.println("equipment= "+resultListx.get(d)[0]);
						if(x==1) {
						header.add(resultListx.get(d)[0].split("#")[1].split("-")[0]);
						}
						groupPmax.add(resultListx.get(d)[1]);
						groupPmin.add(resultListx.get(d)[2]);
						groupw.add(resultListx.get(d)[3]);
					}
					//HashMap
					countr ++; 
					if (countr < 12) { //11 appliances
						groupschedule.add(resultListx.get(d)[4]);
					} else {
						groupschedule.add(resultListx.get(d)[4]);
						String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
						subList.add(arr4);
						//clear groupschedule
						groupschedule=new ArrayList<String>();
						countr = 1;
						if (Integer.parseInt(resultListx.get(d)[5]) < 24) {
							groupschedule.add(iriofgroupuser.get(x-1));
//							groupschedule.add("t"+Integer.toString(Integer.parseInt(resultListx.get(d)[5])+1));
							groupschedule.add(lst.get(Integer.parseInt(resultListx.get(d)[5])));
						}
					}				
			}

			
			String[] arr1 = groupPmax.toArray(new String[groupPmax.size()]);
			csvofpmax.add(arr1);
			String[] arr2 = groupPmin.toArray(new String[groupPmin.size()]);
			csvofpmin.add(arr2);
			String[] arr3 = groupw.toArray(new String[groupw.size()]);
			csvofw.add(arr3);
			System.out.println(groupschedule.toArray().toString());
			String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
			subList.add(arr4);
			Collections.rotate(subList, -h);
			csvofschedule.addAll(subList);

		}

		// csvofpmax.add(0, arr0);
		String pmaxcsv = MatrixConverter.fromArraytoCsv(csvofpmax);
		System.out.println(pmaxcsv);
		broker.putLocal(baseUrl + "/" + Pmax, pmaxcsv);

		// csvofpmin.add(0, arr0);
		String pmincsv = MatrixConverter.fromArraytoCsv(csvofpmin);
		System.out.println(pmincsv);
		broker.putLocal(baseUrl + "/" + Pmin, pmincsv);

		// csvofw.add(0, arr0);
		String wcsv = MatrixConverter.fromArraytoCsv(csvofw);
		System.out.println(wcsv);
		broker.putLocal(baseUrl + "/" + unwill, wcsv);

		String schedulecsv = MatrixConverter.fromArraytoCsv(csvofschedule);
		System.out.println(schedulecsv);
		broker.putLocal(baseUrl + "/" + schedule, schedulecsv);

	}

	

	
	public void runOptimization(String baseUrl) throws Exception {

		copyFromPython(baseUrl, "runpy.bat");
		copyFromPython(baseUrl, "Receding_Horizon_Optimization_V.py");

		String startbatCommand = baseUrl + "/runpy.bat";
		String result = executeSingleCommand(baseUrl, startbatCommand);
		logger.info("final after calling: " + result);
		createTimer(baseUrl);
		String agent = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
		MetaDataAnnotator.annotate(baseUrl, null, agent, true, null);

	}
	public void createTimer(String baseUrl) throws Exception {
		Date date = new Date();
		Calendar calendar = GregorianCalendar.getInstance();
		calendar.setTime(date);
		StringJoiner sj1 = new StringJoiner(",");
		int n = calendar.get(Calendar.HOUR_OF_DAY);
		for (int i = 0; i < 24; i++){
		      sj1.add(n + ":00");
		      n++; 
		      if (n == 24){
		        n = 0;
		      }

		    }
		new QueryBroker().putLocal(baseUrl +"/timer.csv", sj1.toString());
	}

	public void copyFromPython(String baseUrl, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/python/" + filename);

		String destinationUrl = baseUrl + "/" + filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}

	public String executeSingleCommand(String targetFolder, String command) throws InterruptedException {

		// logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds
			pr.waitFor(); // will be executed within such folder.

		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {

			while ((line = bfr.readLine()) != null) {
				resultString += line;

			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}

		return resultString;
	}

	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/" + filename);

		String destinationUrl = newdir + "/" + filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	
	/** provides result in the response of a JSON form
	 * 
	 * @param baseUrl
	 * @return
	 */
	public JSONObject provideJSONResult(String baseUrl) {
		JSONObject responseParams;
		String weatherdir = baseUrl + "/WeatherForecast.csv";
		String content = new QueryBroker().readFileLocal(weatherdir);
		List<String[]> weatherResult = MatrixConverter.fromCsvToArray(content);

		String powerdir = baseUrl + "/totgen.csv";
		String content2 = new QueryBroker().readFileLocal(powerdir);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(content2);
		JSONObject dataresult = new JSONObject();

		String rhdir = baseUrl + "/rh1.csv";
		String content3 = new QueryBroker().readFileLocal(rhdir);
		List<String[]> rhResult = MatrixConverter.fromCsvToArray(content3);
		
		String timer = baseUrl + "/timer.csv";
		String content4 = new QueryBroker().readFileLocal(timer);
		List<String[]> timerResult = MatrixConverter.fromCsvToArray(content4);
		
		JSONArray temperature = new JSONArray();
		JSONArray irradiation = new JSONArray();

		// 25-48 (last 24)
		int sizeofweather = weatherResult.size();
		for (int x = sizeofweather - 24; x < sizeofweather; x++) {
			temperature.put(weatherResult.get(x)[4]);
			irradiation.put(weatherResult.get(x)[8]);
		}

		// log to check if it's reading the right one. x

		dataresult.put("temperature", temperature);
		dataresult.put("irradiation", irradiation);
		dataresult.put("gridsupply", simulationResult.get(4));
		dataresult.put("solar", simulationResult.get(3));
		dataresult.put("residential", simulationResult.get(0));
		dataresult.put("industrial", simulationResult.get(2));
		dataresult.put("commercial", simulationResult.get(1));
		dataresult.put("timer",timerResult.get(0));
		dataresult.put("rh1", rhResult.subList(0, 3).toArray());
		dataresult.put("rh2", rhResult.subList(3, 6).toArray());
		dataresult.put("rh3", rhResult.subList(6, rhResult.size()).toArray());

		return dataresult;
	}
    
	
    
    
    
    

}
