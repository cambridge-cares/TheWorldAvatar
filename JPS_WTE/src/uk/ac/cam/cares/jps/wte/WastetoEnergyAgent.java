package uk.ac.cam.cares.jps.wte;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.arq.querybuilder.Order;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns= {"/startSimulationAgent"})

public class WastetoEnergyAgent extends JPSAgent {
	
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
    }
    /**
     *  create logger to log changes attached to WasteToEnergyAgent class. 
     */
    protected Logger logger = LoggerFactory.getLogger(WastetoEnergyAgent.class);
	
	
	private static final long serialVersionUID = 1L;
	public static final String KEY_WATCH = "watch";
	public static final String KEY_CALLBACK_URL = "callback";
	// first called to begin simulation. 
	public static final String SIM_START_PATH = "/startSimulationAgent";
	public static final String COORDINATION_PATH = "/startsimulationCoordinationWTE";
	//called to produce this result directly after start simulation is called. Waits for the first simulation to finish. 
	public static final String SIM_PROCESS_PATH = "/processresult";
	/**gets the food court name, xy coordinates, amount of waste, the year
	 */
	public static String getFCQuery() {
		SelectBuilder sb = FCQuerySource.getFCQuery();
		sb.addPrefix("j6", "http://www.w3.org/2006/time#").addVar("?entity").addVar("?name")
		.addVar("?wasteproductionvalue").addVar("?year")
		.addWhere("?entity", "j1:produceWaste","?WP").addWhere("?WP", "j2:hasValue","?vWP")
		.addWhere("?vWP", "j2:numericalValue","?wasteproductionvalue").addWhere("?vWP", "j6:hasTime","?time")
		.addWhere("?time", "j6:inDateTime","?vdatetime")
		.addWhere("?vdatetime", "j6:year","?year").addOrderBy("?year", Order.ASCENDING);
		Query q = sb.build();
		return q.toString();
	}
	/** gets the transportation route, the tax on the route, the capacity of travel, the cost of travel, and
	 * emission rate of travelling on that route. 
	 */
	public static String getTransportQuery() {
		SelectBuilder sb = new SelectBuilder()
				.addPrefix("j1","http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addPrefix("j8", "http://www.theworldavatar.com/ontology/ontotransport/OntoTransport.owl#")
				.addVar("?Unit_transport_capacity").addVar("?Unit_transport_cost")
				.addVar("?pollutionTransportTax").addVar("?dieselConsTruck")
		.addWhere("?entity", "a","j8:TransportationRoute").addWhere("?entity", "j8:suitFor","?truck")
		.addWhere("?truck" ,"j1:hasTax", "?PTT").addWhere("?PTT" ,"j2:hasValue", "?vPTT")
		.addWhere("?vPTT" ,"j2:numericalValue", "?pollutionTransportTax")
		.addWhere("?truck", "j8:hasTransportationCapacity","?TC")
		.addWhere("?TC", "j2:hasValue","?vTC")
		.addWhere("?vTC", "j2:numericalValue","?Unit_transport_capacity")
		.addWhere("?truck", "j8:hasTransportationCost","?TCost")
		.addWhere("?TCost", "j2:hasValue","?vTCost")
		.addWhere("?vTCost", "j2:numericalValue","?Unit_transport_cost")

		.addWhere("?truck", "j8:hasEmission","?Temission")
		.addWhere("?Temission", "j2:hasValue","?vTemission")
		.addWhere("?vTemission", "j2:numericalValue","?dieselConsTruck");
		Query q = sb.build();
		return q.toString();
	}
	
	
	
	/** gets the OffsiteWasteTreatmentFacility's 
	 * Incineration upper bound, CoDigestion upper bound, and Anerobic Digestion upper bound. 
	 */
	public static String returnUpperBoundQuery() {
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#" )
				.addVar("?entity").addVar("?tech1upp").addVar("?tech2upp").addVar("?tech3upp")
				.addWhere("?entity" ,"j1:hasOffsiteIncinerationUpperBound", "?tech1upp")
				.addWhere("?entity" ,"j1:hasOffsiteCoDigestionUpperBound", "?tech2upp")
				.addWhere("?entity" ,"j1:hasOffsiteAnerobicDigestionUpperBound", "?tech3upp");
		return sb.buildString();
	}
	
	/** main function. Reads the values in and copies the templates back. 
	 * 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if (!validateInput(requestParams)) {
			throw new BadRequestException("WTE:startSimulationAgent: Input parameters not found.\n");
		}
		String baseUrl= requestParams.getString("baseUrl");
		String wasteIRI=requestParams.getString("wastenetwork");
		//render ontological model of waste network
		OntModel model= readModelGreedy(wasteIRI);
		//creates the csv of FCs, with Site_xy reading for location, waste containing the level of waste in years 1-15
		//in baseUrl folder
		 prepareCSVFC("Site_xy.csv","Waste.csv", baseUrl,model,15); 
		// searches for number of clusters to agglomerate (i.e. number of onsite WTF max)
		String n_cluster= requestParams.getString("n_cluster");
		//TODO: Can put this as input to matlab, but I don't know Matlab well enough for this, so having a txt to read from 
		// is feasible for now. 
        new QueryBroker().putLocal(baseUrl + "/n_cluster.txt",n_cluster ); 
        //create csv for location of offsite WTF in baseUrl folder
		prepareCSVWT("Location.csv", baseUrl,model); 
		// get transport costs for transport usage. 
		prepareCSVTransport(getTransportQuery(),"transport.csv", baseUrl,model); 
		//prepare csv of upper bounds of all kinds of tech
		//TODO: the more types of technology is added, the more likely this method (returnUpperBoundQuery()) changes. 
		prepareCSVCompTECHBased(returnUpperBoundQuery(),baseUrl,model);
		//form query of waste Tech offsite 
		String WTFTechOffsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OffsiteWasteTreatmentFacility").buildString();
		prepareCSVTECHBased(WTFTechOffsiteQuery,baseUrl,model,"offsite");
		String WTFTechOnsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility")
				.addWhere("?Tech1" ,"a", "j1:OnSiteDigester").buildString();
		prepareCSVTECHBased(WTFTechOnsiteQuery,baseUrl,model,"onsite");
		//TODO: should be able to run Main.m without needing to copy over, but I don't know Matlab well enough 
		// Should need something like fullfile to use "readmatrix" to read an absolute path. 
		copyTemplate(baseUrl, "SphereDist.m");
		copyTemplate(baseUrl, "Main.m");
		copyTemplate(baseUrl, "D2R.m");

		String path = requestParams.getString("scenarioagentoperation");
		try {
			createBat(baseUrl, n_cluster);
            notifyWatcher(requestParams, baseUrl+"/year by year_NPV.txt",
                    path.replace(COORDINATION_PATH, SIM_PROCESS_PATH));
		} catch (Exception e) {
			throw new JPSRuntimeException("WasteToEnergy StartSimulationAgent: Notify watcher had error. ");
		}
		return requestParams;
	}
	/** checks if n_cluster is an integer
	 * and wastenetwork is an IRI
	 * 
	 * @param requestParams
	 * @return
	 * @throws BadRequestException
	 */
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
        String iriofnetwork = requestParams.getString("wastenetwork");
        String nCluster = requestParams.getString("n_cluster");
        return InputValidator.checkIfValidIRI(iriofnetwork) & InputValidator.checkIfInteger(nCluster);
        } catch (JSONException ex) {
        	return false;
        }
    }
	
	/** reads the topnode into an OntModel of all its subsystems. 
	 * @param iriofnetwork
	 * @return
	 */
	public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the offsite wtf, transportation and food court
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:hasSubsystem", "?component");
		String wasteInfo = sb.build().toString();

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, wasteInfo);
	}
	/** Creates the CSV of the foodcourt for the Matlab code to read. 
	 * a. reading the model and getting the number of FC *number of years of waste levels 
	 * b. creating the csv file of site locations, and waste levels of those FoodCourts per year
	 * 
	 * @param mainquery
	 * @param filename
	 * @param filename2
	 * @param baseUrl
	 * @param model
	 * @return
	 */
	public void prepareCSVFC(String filename,String filename2, String baseUrl,OntModel model, int noOfYears) { //create csv for food court and giving the list of complete food court iri
		//csv input file		
		ResultSet resultSet = JenaHelper.query(model,getFCQuery());
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysfc = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysfc);
        List<String[]> resultxy = new ArrayList<String[]>();
        List<String[]> resultfcmapper = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[2], resultList.get(d)[3] };// only extract and y
			String[] mapper = {resultList.get(d)[0],resultList.get(d)[2], resultList.get(d)[3] };// only extract and y
			if (resultList.get(d)[5].contentEquals("1")) { //get the initial year first
				resultxy.add(comp);
				resultfcmapper.add(mapper);
			}
		}
        if (filename2 != null) {//waste.csv
			List<String[]> resultwaste = new ArrayList<String[]>();
			int size = resultList.size();
			int amountinst = size / noOfYears; // assume it's from year 1

			for (int n = 0; n < amountinst; n++) {
				String[] consumption = new String[noOfYears];
				for (int r = 0; r < noOfYears; r++) { 
					consumption[r] = resultList.get(r * amountinst + n)[4];
				}
				resultwaste.add(consumption);
			}
			new QueryBroker().putLocal(baseUrl + "/" + filename2, MatrixConverter.fromArraytoCsv(resultwaste));
		}
        String[]header= {keysfc[2],keysfc[3]};
       // String[]headerwaste= {"waste year1"};
        resultxy.add(0,header);
        //resultwaste.add(0,headerwaste);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy));
	}
	/** prepares the CSV file for the offsite Waste treatment facility and it's xy coordinates. 
	 * 
	 * @param mainquery String
	 * @param filename String
	 * @param baseUrl String
	 * @param model
	 */
	public void prepareCSVWT(String filename,String baseUrl,OntModel model) {
		String mainquery = FCQuerySource.getOffsiteWasteTreatmentQuery();
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultxy = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = { resultList.get(d)[1], resultList.get(d)[2] };// only extract and y
			resultxy.add(comp);
		}
        String[]header= {keyswt[1],keyswt[2]};
        resultxy.add(0,header);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy)); 	
	}
	/** grabs the offsite waste treatment facility's costs, sorted by technology type. 
	 * 
	 * @param mainquery String
	 * @param baseUrl String
	 * @param model OntModel 
	 */
	public void prepareCSVTransport(String mainquery,String filename,String baseUrl,OntModel model) {		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> resultxy = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			String[] comp = resultList.get(d);// only extract and y
			resultxy.add(comp);
		}
        resultxy.add(0,keyswt);
        new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultxy)); 	
	}
	/** Creates the CSV file for the upper bound 
	 * 
	 * @param mainquery compquery. 
	 * @param baseUrl String
	 * @param model Ontological Model
	 * @return List
	 */
	public void prepareCSVCompTECHBased(String mainquery,String baseUrl,OntModel model) {		
		List<String[]> resultList = FCQuerySource.queryResult(model, mainquery);
		 List<String[]> resultTechOffsiteWTF = new ArrayList<String[]>();
        int technumber=3;
        String[] header = new String[resultList.size()];
		for (int d = 0; d < technumber; d++) {
			String[] comp = new String[resultList.size()];
			
			for (int dd = 0; dd < resultList.size(); dd++) {
				comp[dd] = resultList.get(dd)[d+1];
				header[dd]=resultList.get(dd)[0];

			}
			resultTechOffsiteWTF.add(comp);
		}
		resultTechOffsiteWTF.add(0,header);
		
        new QueryBroker().putLocal(baseUrl + "/n_unit_max_offsite.csv",MatrixConverter.fromArraytoCsv(resultTechOffsiteWTF));
        
	}
	/** 
	 * 
	 * @param mainquery: costs of waste treatment facility, either offsite or onsite
	 * @param baseUrl location of folder
	 * @param model of WasteNetwork topnode
	 * @param keyword
	 * @return
	 */
	public List<String[]> prepareCSVTECHBased(String mainquery,String baseUrl,OntModel model,String keyword) {//keyword offsite or onsite		
		ResultSet resultSet = JenaHelper.query(model, mainquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keyswt = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyswt);
        List<String[]> tax = new ArrayList<String[]>();
        List<String[]> capacity = new ArrayList<String[]>();
        List<String[]> inscost = new ArrayList<String[]>();
        List<String[]> opcost = new ArrayList<String[]>();
        List<String[]> transferrate = new ArrayList<String[]>();
        List<String[]> consumption = new ArrayList<String[]>();
		for (int d = 0; d < resultList.size(); d++) {
			//?pollutiontreatmenttaxvalue ?Tech1Capvalue ?installationcostvalue ?operationcostvalue ?transferrateelectricvalue ?energyconsumptionvalue ?tech "
			String[] comp0 = { resultList.get(d)[0]};
			String[] comp1 = { resultList.get(d)[1]};
			String[] comp2 = { resultList.get(d)[2]};
			String[] comp3 = { resultList.get(d)[3]};
			String[] comp4 = { resultList.get(d)[4]};
			String[] comp5 = { resultList.get(d)[5]};
			tax.add(comp0);
			capacity.add(comp1);
			inscost.add(comp2);
			opcost.add(comp3);
			transferrate.add(comp4);
			consumption.add(comp5);	
		}
		String[] header0= {keyswt[0]};
		String[] header1= {keyswt[1]};
		String[] header2= {keyswt[2]};
		String[] header3= {keyswt[3]};
		String[] header4= {keyswt[4]};
		String[] header5= {keyswt[5]};
		tax.add(0,header0);
		capacity.add(0,header1);
		inscost.add(0,header2);
		opcost.add(0,header3);
		transferrate.add(0,header4);
		consumption.add(0,header5);
		
		new QueryBroker().putLocal(baseUrl + "/Conversion rate ("+keyword+").csv", MatrixConverter.fromArraytoCsv(transferrate));
        new QueryBroker().putLocal(baseUrl + "/Pollution treatment tax ("+keyword+").csv", MatrixConverter.fromArraytoCsv(tax)); 
        new QueryBroker().putLocal(baseUrl + "/Resource conversion ("+keyword+").csv", MatrixConverter.fromArraytoCsv(consumption)); 
        new QueryBroker().putLocal(baseUrl + "/Unit installation cost ("+keyword+").csv", MatrixConverter.fromArraytoCsv(inscost)); 
        new QueryBroker().putLocal(baseUrl + "/Unit operation cost ("+keyword+").csv", MatrixConverter.fromArraytoCsv(opcost));
        new QueryBroker().putLocal(baseUrl + "/Unit_capacity_"+keyword+".csv", MatrixConverter.fromArraytoCsv(capacity)); 
	
        return resultList;
	}
	/** copies over the files in the working directory over to scenario folder. 
	 * 
	 * @param newdir
	 * @param filename
	 */
	public void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}/** create the batch file in the mentioned folder. 
	 * 
	 * @param baseUrl
	 * @throws Exception
	 */
	public void createBat(String baseUrl, String n_cluster) throws Exception {
		String loc = baseUrl + "\\Main.m";
		
		String bat =  "matlab -nosplash -noFigureWindows -r \"try; run('"
				+ loc + "'); catch; end; quit\"";
		System.out.println(bat);
        CommandHelper.executeSingleCommand(baseUrl, bat);
	}
	
	/** notifies the watcher to return with the callback. 
	 * 
	 * @param agentArgs JSONObject
	 * @param filePath String
	 * @param callbackIRI String
	 */
    private void notifyWatcher(JSONObject agentArgs, String filePath, String callbackIRI) {
        agentArgs.put(KEY_WATCH, filePath);
        agentArgs.put(KEY_CALLBACK_URL, callbackIRI);
        execute(KeyValueMap.getInstance().get("url.jps_aws"), agentArgs.toString(), HttpPost.METHOD_NAME);
    }
}
