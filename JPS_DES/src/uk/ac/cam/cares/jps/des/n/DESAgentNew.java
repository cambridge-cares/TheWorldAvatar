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
import java.util.Map;
import java.util.StringJoiner;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.apache.jena.sparql.core.Var;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
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
    	String iriofnetwork = requestParams.optString("electricalnetwork", "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork");
        String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
        String irioftempF=requestParams.optString("temperatureforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001");
        String iriofirrF=requestParams.optString("irradiationforecast", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001");
        
        cityIRI = requestParams.optString("cityIRI", "http://dbpedia.org/page/Singapore");
        String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
        queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        OntModel model = readModelGreedy(iriofnetwork);
        new SolarAgent().provideGenlist(model, baseUrl); // create Parameters for Solar Cell
		new ResidentialAgent().extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		new CommercialAgent().queryForBuildingConstants(model, baseUrl);;//csv for commercial
		new IndustrialAgent().queryForConstantsIndustrial(model, baseUrl);;//csv for commercial
		
		try {
			String result = runPythonScript("system.py", baseUrl);
			String agent = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
			createTimer(baseUrl);
			MetaDataAnnotator.annotate(baseUrl, null, agent, true, null);

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
   	    	WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
   	    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
   	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
   	    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
   	    			.addWhere("?prop", "j2:hasValue", "?vprop").addWhere("?vprop", "j2:numericalValue", "?propval")
   	    			.addWhere("?vprop", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
	       
   	    	
   	    	SelectBuilder sensorTemp = new SelectBuilder()
   	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
   	    			.addVar("?entity").addVar("?propval")
   	    			.addVar("?proptimeval").addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
   	    	Query q= sensorTemp.build(); 
   	    	String sensorInfo = q.toString();
   	    	SelectBuilder sensorIrrad = new SelectBuilder()
   	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
   	    			.addVar("?entity").addVar("?propval")
   	    			.addVar("?proptimeval").addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB);
   	    	
   	    	q= sensorIrrad.build(); 
   	    	String sensorInfo2 = q.toString();
   	    	
			
 			//grab forecast results
			String result = new QueryBroker().queryFile(irioftempF, sensorInfo);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]>  resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
 			System.out.println("Temperature " + resultListfromquerytemp.toString());
 			String result2 = new QueryBroker().queryFile(iriofirrF, sensorInfo2);
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
	/** states time at which next reading is done. 
	 * 
	 * @param baseUrl
	 * @throws Exception
	 */
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
    
}
