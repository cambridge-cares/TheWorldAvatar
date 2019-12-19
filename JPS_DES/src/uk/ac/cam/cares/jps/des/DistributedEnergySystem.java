package uk.ac.cam.cares.jps.des;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
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
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;




@WebServlet(urlPatterns = { "/DESAgent" })
public class DistributedEnergySystem extends JPSHttpServlet {
	
	public static String weather="Weather.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
	
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String bcap="bcap.csv";
	public static String unwill="unwill.csv";
	
	
	
	public static String producerdata="PV_parameters.csv";
	public static String consumerdata1="FuelCell.csv";
	
	
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
    protected JSONObject processRequestParameters(JSONObject requestParams) {
		QueryBroker broker = new QueryBroker();
		String baseUrl = QueryBroker.getLocalDataPath();
		
		//currently not needed because owl file not ready
		String iriofnetwork = requestParams.getString("electricalnetwork");
		OntModel model = readModelGreedy(iriofnetwork);
		List<String[]> producer = provideGenlist(model); // instance iri
		List<String[]> consumer = provideLoadFClist(model); // instance iri

		String producercsv = MatrixConverter.fromArraytoCsv(producer);
		
		
		broker.putLocal(baseUrl + "/"+producerdata, producercsv);

		String consumercsv = MatrixConverter.fromArraytoCsv(consumer);
		broker.putLocal(baseUrl + "/"+consumerdata1, consumercsv);
		
		

		
		try {
			runOptimization(baseUrl);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		String directory = baseUrl + "";
		JSONObject newresult = new JSONObject();
		newresult.put("folder", directory);

		return newresult;
    	
    }
    
    public void runOptimization(String baseUrl) throws IOException, InterruptedException {
		
		//copyTemplate(baseUrl, weather);//temporary
		JSONObject jo = new JSONObject();

		jo.put("folder", baseUrl);
		jo.put("tempsensor",
				"http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
		jo.put("speedsensor",
				"http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		jo.put("irradiationsensor",
				"http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData",
				jo.toString());
		
		//constraints related to residential
		copyTemplate(baseUrl, Pmin);
		copyTemplate(baseUrl, Pmax);
		copyTemplate(baseUrl, bcap);
		copyTemplate(baseUrl, unwill);
		//property for residential
		copyTemplate(baseUrl, schedule);
		
		
		copyFromPython(baseUrl, "runpy.bat");
		copyFromPython(baseUrl,"Receding_Horizon_Optimization_V0.py");
		
		String startbatCommand =baseUrl+"/runpy.bat";
		String result= executeSingleCommand(baseUrl,startbatCommand);
		logger.info("final after calling: "+result);
    	//String DES = PythonHelper.callPythonwithNoParameter("Receding Horizon Optimization_V0.py", this);
    	
    	
    }

	public void copyFromPython(String baseUrl,String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/python/"+filename);
		
		String destinationUrl = baseUrl + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	
	public String executeSingleCommand(String targetFolder, String command) throws InterruptedException {

		//logger.info("In folder: " + targetFolder + " Excuted: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds
			pr.waitFor();														// will be executed within such folder.

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
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
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
                + "?isc a j9:RatedCurrent ."
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
            
                            
                //+ "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
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
        
		for (int d = 0; d < keys.length; d++) {
				String[] line0 = { keys[d], resultListfromquery.get(0)[d] };
				resultListforcsv.add(line0);
		}

        return resultListforcsv;
    }

}
