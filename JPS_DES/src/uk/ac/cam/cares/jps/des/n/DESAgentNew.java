package uk.ac.cam.cares.jps.des.n;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.StringJoiner;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

import uk.ac.cam.cares.jps.base.util.InputValidator;


@WebServlet(urlPatterns = { "/DESAgentNew"})

public class DESAgentNew extends JPSAgent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String cityIRI;
	private static String producerdata="PV_parameters.csv";
	private static String consumerdata1="FuelCell.csv";
	private static String Pmin="Pmin.csv";
	private static String Pmax="Pmax.csv";
	private static String bcap="bcap.csv";
	private static String unwill="unwill.csv";
	private static String schedule="ApplianceScheduleLoad1.csv";
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	/** main Execution method for DESAgent
	 * 
	 */
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	JSONObject responseParams = new JSONObject();	
    	if (!validateInput(requestParams)) {
    		throw new BadRequestException("DESAgent:  Input parameters not found.\n");
    	}
    	String iriofnetwork = requestParams.getString("electricalnetwork");
        String iriofdistrict = requestParams.getString("district");
        String irioftempF=requestParams.getString("temperatureforecast");
        String iriofirrF=requestParams.getString("irradiationforecast");
        String baseUrl = requestParams.getString("baseUrl"); //create unique uuid
        
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
			baseUrl = new File(baseUrl).toURI().toString();
			MetaDataAnnotator.annotate(baseUrl, null, agent, true, null);

			responseParams.put("result", result);
		} catch (Exception e) {
			e.printStackTrace();
		} 
		return responseParams;
    }
	
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
        String iriofnetwork = requestParams.getString("electricalnetwork");
        boolean q = InputValidator.checkIfValidIRI(iriofnetwork);

        String iriofdistrict = requestParams.getString("district");
        boolean w = InputValidator.checkIfValidIRI(iriofdistrict);
        
        String irioftempF=requestParams.getString("temperatureforecast");

        boolean e = InputValidator.checkIfValidIRI(irioftempF);
        String iriofirrF=requestParams.getString("irradiationforecast");
        boolean r = InputValidator.checkIfValidIRI(iriofirrF);
        // Till now, there is no system independent to check if a file path is valid or not. 
        
        return q&w&e&r;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        	throw new JSONException("Forecast not present in getString");
        }

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
   	    			.addVar("?propval").addWhere("?entity","a", "j5:T-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
   	    	Query q= sensorTemp.build(); 
   	    	String sensorInfo = q.toString();
   	    	SelectBuilder sensorIrrad = new SelectBuilder()
   	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
   	    			.addVar("?propval").addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
   	    	
   	    	q= sensorIrrad.build(); 
   	    	String sensorInfo2 = q.toString();
   	    	
			
 			//grab forecast results
   	    	JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo)
   					.put(JPSConstants.TARGETIRI , tempIRItoFile(irioftempF));
   			String resultf = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
			String[] keysf = {"propval"};
			List<String[]>  resultListfromquerytemp = JenaResultSetFormatter. convertToListofStringArraysWithKeys(resultf, keysf);
   			requestParams.put(JPSConstants.QUERY_SPARQL_QUERY, sensorInfo2)
   			.put(JPSConstants.TARGETIRI , tempIRItoFile(iriofirrF));
   			String result2 = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString());
 			List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArraysWithKeys(result2, keysf);
 			List<String[]> readingFromCSV = new ArrayList<String[]>();
 			for (int d=0;d<resultListfromqueryirr.size();d++) {
 				String[] ed= {resultListfromquerytemp.get(d)[0],resultListfromqueryirr.get(d)[0]};
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
	    SelectBuilder modelQ = new SelectBuilder()
	    .addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
	    .addVar("?component")
	    .addWhere("?entity","j2:hasSubsystem", "?component");
	    Query q= modelQ.build();
	    String nodeInfo = q.toString();
	    JSONObject requestParams = new JSONObject().put(JPSConstants.QUERY_SPARQL_QUERY, nodeInfo)
				.put(JPSConstants.TARGETIRI , tempIRItoFile(iriofnetwork));
		String result = AgentCaller.executeGetWithJsonParameter("jps/kb", requestParams.toString()); 
		
	    return readModelGreedyCon(result);
	}
	/** Submethod of readModelGreedy
	 * 
	 * @param greedyResult
	 * @return
	 */
	public static OntModel readModelGreedyCon(String greedyResult) {
		
		JSONArray ja = new JSONArray(new JSONObject(greedyResult).getString("results"));
		
		List<String> nodesToVisit = new ArrayList<String>();
		for (int i=0; i<ja.length(); i++) {
			JSONObject row = ja.getJSONObject(i);
			for (String current : row.keySet()) {
				String potentialIri =  row.getString(current);
				if (potentialIri.startsWith("http")) {
					int index = potentialIri.lastIndexOf("#");
					if (index > 0) {
						potentialIri = potentialIri.substring(0, index);
					}
					if (!nodesToVisit.contains(potentialIri)) {
						nodesToVisit.add(potentialIri);
					} 
				}
			}
		}
		
		
		//OntModel model = ModelFactory.createOntologyModel();
		OntModel model = JenaHelper.createModel();
		for (String current : nodesToVisit) {
			current = ResourcePathConverter.convert(current);
			model.read(current, null); 
		}
		
		return model;
	}

    /** find individual components (for residential) of appliances
    *
    * @param useriri
    * @return
    */
    public static OntModel readModelGreedyForUser(String useriri) {
	
	    SelectBuilder modelQ = new SelectBuilder()
	    .addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
	    .addVar("?component")
	    .addWhere("?entity","j2:isConnectedTo", "?component");
	    Query q= modelQ.build();
	    String electricalnodeInfo = q.toString();
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
	/** copy the file from workingdir and place it in folder
	 * used because when I tried to run it without copying
	 * 
	 * 
	 * @param baseUrl
	 * @param filename
	 */
	public void copyFromPython(String baseUrl, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/python/" + filename);

		String destinationUrl = baseUrl + "/" + filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	/** Temporary translation fix for switch from KBClient to KGRouter. In the end, it should be querying appropriately from the 
	 * correct Source
	 * 
	 * @param iriofnetwork The file name to be changed. 
	 * @return iriofnetwork (changed)
	 */
	public static String tempIRItoFile(String iriofnetwork) {
		String translatedIRI = iriofnetwork.replace("http://www.theworldavatar.com", AgentLocator.getProperty("absdir.root"));
		translatedIRI = ScenarioHelper.cutHash(translatedIRI);
		iriofnetwork = translatedIRI.replace("C:/", "C:\\");
		return iriofnetwork;
	}
    
}
