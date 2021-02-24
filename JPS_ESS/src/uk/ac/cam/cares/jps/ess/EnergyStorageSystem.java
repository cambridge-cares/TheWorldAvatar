package uk.ac.cam.cares.jps.ess;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.io.FileUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


@WebServlet(urlPatterns = { "/ESSAgent"})

public class EnergyStorageSystem extends JPSAgent {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	//public static final String AGENT_TAG = "GAMS_NuclearAgent";

	Logger logger = LoggerFactory.getLogger(EnergyStorageSystem.class);
	@Override
	protected void setLogger() {
		logger = LoggerFactory.getLogger(EnergyStorageSystem.class);
	}
	private String modelname="NESS.gms";
    private List<ElectricalComponentObject>batterylist=new ArrayList<ElectricalComponentObject>();
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	    
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			String baseUrl = QueryBroker.getLocalDataPath() + "/GAMS_ESS";
			System.out.println("baseURL: " + baseUrl);
			boolean v = validateInput(requestParams);
			if (v == false) {
				throw new JSONException("INPUT no longer valid");
			}
			String batIRI=requestParams.getString("BatteryCatalog");
			String ENIRI=requestParams.getString("electricalnetwork");
			
			List<String> pvGenIRI=filterPV(ENIRI);						
			JSONObject resultofbattery = null;
			try {
				resultofbattery = optimizedBatteryMatching(baseUrl, pvGenIRI, batIRI);
				return resultofbattery;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				logger.error(e.getMessage());
			}
			return resultofbattery;
		}
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);
			String batIRI=requestParams.getString("BatteryCatalog");	  
	        boolean q = InputValidator.checkIfValidIRI(batIRI); 
	        return w&q;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
    /** code that should run GAMS
     * 
     * @param baseUrl
     * @throws IOException
     * @throws InterruptedException
     */
 	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 		
		
		modifyTemplate(baseUrl,modelname);		
		logger.info("Start");
		//If user does not have GAMSDIR on, replace the following with the location of his GAMS
//		String executablelocation ="C:/GAMS/win64/26.1/gams.exe"; //depends where is in claudius
		String gamsLocation = System.getenv("GAMSDIR").split(";")[0];


		gamsLocation =gamsLocation.replace("\\", "/");
		gamsLocation =gamsLocation.replace("//", "/");
		String executablelocation = gamsLocation+"/gams.exe";
        String folderlocation =baseUrl.replace("//", "/");
        String[] cmdArray = new String[7];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation+"/" + modelname;
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "PROCDIR="+folderlocation;
        cmdArray[5] = "CURDIR="+folderlocation;
        cmdArray[6] = "LO=2";
        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4]+" "+cmdArray[5]+" "+cmdArray[6];
        
        CommandHelper.executeSingleCommand(baseUrl, cmdArrayinstring);
   
 		}
	/** modifies the GAMS code in workingdir folder to accept the file written in newdir
	 * 
	 * @param newdir location of simulation files
	 * @param filename NESS.gms
	 * @throws IOException
	 */
	public void modifyTemplate(String newdir, String filename) throws IOException {
		//header that include the battery name hardcoded in the gams MUST BE THE SAME as the one in the input file
		newdir = newdir.replace("//", "/");
		String destinationUrl = newdir + "/"+filename;
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
        String fileContext = FileUtils.readFileToString(file);
		System.out.println("FILE: FILE: "+ file);
        
        fileContext = fileContext.replaceAll("Ptlow.gdx",newdir+"/Ptlow.gdx");
        fileContext = fileContext.replaceAll("Pthigh.gdx",newdir+"/Pthigh.gdx");
        fileContext = fileContext.replaceAll("Dtlow.gdx",newdir+"/Dtlow.gdx");
        fileContext = fileContext.replaceAll("Dthigh.gdx",newdir+"/Dthigh.gdx");
        fileContext = fileContext.replaceAll("EnvironmentalScore.gdx",newdir+"/EnvironmentalScore.gdx");
        fileContext = fileContext.replaceAll("EconomicalScore.gdx",newdir+"/EconomicalScore.gdx");
        fileContext = fileContext.replaceAll("Maturity.gdx",newdir+"/Maturity.gdx");
        fileContext = fileContext.replaceAll("Pa_high.gdx",newdir+"/Pa_high.gdx");
        
        fileContext = fileContext.replaceAll("Ptlow.csv",newdir+"/Ptlow.csv output="+newdir+"/Ptlow.gdx");
        fileContext = fileContext.replaceAll("Pthigh.csv",newdir+"/Pthigh.csv output="+newdir+"/Pthigh.gdx");
        fileContext = fileContext.replaceAll("Dtlow.csv",newdir+"/Dtlow.csv output="+newdir+"/Dtlow.gdx");
        fileContext = fileContext.replaceAll("Dthigh.csv",newdir+"/Dthigh.csv output="+newdir+"/Dthigh.gdx");
        fileContext = fileContext.replaceAll("EnvironmentalScore.csv",newdir+"/EnvironmentalScore.csv output="+newdir+"/EnvironmentalScore.gdx");
        fileContext = fileContext.replaceAll("EconomicalScore.csv",newdir+"/EconomicalScore.csv output="+newdir+"/EconomicalScore.gdx");
        fileContext = fileContext.replaceAll("Maturity.csv",newdir+"/Maturity.csv output="+newdir+"/Maturity.gdx");
        fileContext = fileContext.replaceAll("Pa_high.csv",newdir+"/Pa_high.csv output="+newdir+"/Pa_high.gdx");
        System.out.println("NEWDIR: "+ newdir);
        //FileUtils.write(file, fileContext);
 
		
		new QueryBroker().putLocal(destinationUrl, fileContext);
	}
	/** read the battery to a OntModel of its components
	 * 
	 * @param batterycatiri
	 * @return
	 */
	public static OntModel readBatteryGreedy(String batterycatiri) {
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:contains", "?component");
		String batteryInfo= sb.build().toString();
		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(batterycatiri, batteryInfo);
		
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
	/** prepare the max and min power generated as well as state of charge. 
	 * 
	 * @param pvGenIRI IRI of Solar generator
	 * @param baseUrl folder where csv is dumped
	 * @return
	 */
	public void prepareCSVPahigh(List<String> pvGenIRI, String baseUrl) {

		// System.out.println("model= "+model);
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addVar("?Pa_high").addVar("?Da_low").addVar("?Pa_low").addVar("?Da_high")
				.addWhere("?entity" ,"a", "j1:PowerGenerator")
				.addWhere("?entity" ,"j6:hasMaximumActivePowerGenerated", "?Pmax")
				.addWhere("?Pmax" ,"j2:hasValue", "?vPmax").addWhere("?vPmax" ,"j5:upperLimit", "?Pa_high")

				.addWhere("?entity" ,"j6:hasMinimumActivePowerGenerated", "?Pmin")
				.addWhere("?Pmin" ,"j2:hasValue", "?vPmin").addWhere("?vPmin" ,"j5:lowerLimit", "?Pa_low")
				
				.addWhere("?entity" ,"j6:hasStateOfCharge", "?dt").addWhere("?dt" ,"j2:hasValue", "?vdt")
				.addWhere("?vdt" ,"j5:upperLimit", "?Da_high")
				.addWhere("?vdt" ,"j5:lowerLimit", "?Da_low");
		String pvquery = sb.buildString();
		

		List<String[]> resultListforcsv = new ArrayList<String[]>();
		String[] header = { "Parameters", "Value" };
		resultListforcsv.add(header);

		int variablequeried = 4;
		String[] keyspv = new String[variablequeried];
		Double[] pvprop = { 0.0, 0.0, 0.0, 0.0 };

		for (int w = 0; w < pvGenIRI.size(); w++) {

			String result = new QueryBroker().queryFile(pvGenIRI.get(w), pvquery);
			keyspv = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyspv);

			for (int x = 0; x < variablequeried; x++) {
				pvprop[x] = pvprop[x] + Double.valueOf(resultList.get(0)[x]);
			}
		}

		for (int d = 0; d < variablequeried; d++) {
			String[] line0 = { keyspv[d], "" + pvprop[d] };
			resultListforcsv.add(line0);
		}

		String s = MatrixConverter.fromArraytoCsv(resultListforcsv);
		new QueryBroker().putLocal(baseUrl + "/Pa_high.csv", s);
	}
	/** run through the characteristics of fifteen batteries and print out in csv format in folder baseUrl
	 * 
	 * @param batcal battery catalog IRI
	 * @param baseUrl folder where this csv is dumped
	 */
	public void prepareCSVRemaining(String batcal, String baseUrl) {
		OntModel model=readBatteryGreedy(batcal);		
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addVar("?entity").addVar("?environment").addVar("?cost").addVar("?maturity")
				.addVar("?vPtHigh").addVar("?vPtLow").addVar("?DTHigh").addVar("?DTLow")
				.addWhere("?entity" ,"j2:hasProperty", "?MC")
				.addWhere("?MC" ,"j2:hasValue", "?vMC")
				.addWhere("?vMC" ,"j2:numericalValue", "?environment")
				
				.addWhere("?entity" ,"j3:hasCost", "?EC")
				.addWhere("?EC" ,"j2:hasValue", "?vEC")
				.addWhere("?vEC" ,"j2:numericalValue", "?cost")
				
				.addWhere("?entity" ,"j1:hasNumberOfCells", "?no")
				.addWhere("?no" ,"j2:hasValue", "?vno")
				.addWhere("?vno" ,"j2:numericalValue", "?maturity")
				

				.addWhere("?entity" ,"j6:hasMaximumActivePowerGenerated", "?Pmax")
				.addWhere("?Pmax" ,"j2:hasValue", "?vPmax")
				.addWhere("?vPmax" ,"j2:numericalValue", "?vPtHigh")

				.addWhere("?entity" ,"j6:hasMinimumActivePowerGenerated", "?Pmin")
				.addWhere("?Pmin" ,"j2:hasValue", "?vPmin")
				.addWhere("?vPmin" ,"j2:numericalValue", "?vPtLow")
				
				.addWhere("?entity" ,"j6:hasStateOfCharge", "?dt")
				.addWhere("?dt" ,"j2:hasValue", "?vdt")
				.addWhere("?vdt" ,"j5:upperLimit", "?DTHigh")
				.addWhere("?vdt" ,"j5:lowerLimit", "?DTLow");
		
		String batteryquery = sb.buildString();
        List<String[]> resultList = queryResult(model, batteryquery);
        System.out.println("battery list size= "+resultList.size());

        for(int bat=0; bat<resultList.size();bat++) {
        	ElectricalComponentObject b= new ElectricalComponentObject(resultList.get(bat)[0]);
        	System.out.println("battery list= "+resultList.get(bat)[0]);
        	b.setenv(Double.valueOf(resultList.get(bat)[1]));
        	b.setcost(Double.valueOf(resultList.get(bat)[2]));
        	b.setmatur(Double.valueOf(resultList.get(bat)[3]));
        	b.setpthigh(Double.valueOf(resultList.get(bat)[4]));
        	b.setptlow(Double.valueOf(resultList.get(bat)[5]));
        	b.setdthigh(Double.valueOf(resultList.get(bat)[6]));
        	b.setdtlow(Double.valueOf(resultList.get(bat)[7]));
        	batterylist.add(b);        	
        }
        
        
		String[] header = { "Parameter", "Value" };
		
		makeBatteryInputParamCSV(baseUrl, resultList, header,"EnvironmentalScore.csv",1);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"EconomicalScore.csv",2);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"Maturity.csv",3);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"Pthigh.csv",4);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"Ptlow.csv",5);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"Dthigh.csv",6);
		makeBatteryInputParamCSV(baseUrl, resultList, header,"Dtlow.csv",7);
		
	}
	private void makeBatteryInputParamCSV(String baseUrl, List<String[]> resultList, String[] header,String filename,int index) {
		List<String[]> resultListforcsv = new ArrayList<String[]>();
		resultListforcsv.add(header);
		for (int x = 0; x < resultList.size(); x++) {
			String[] line = {resultList.get(x)[0].split("#")[1], resultList.get(x)[index] };
			resultListforcsv.add(line);
		}
		new QueryBroker().putLocal(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultListforcsv));
	}
	
	/** battery select: Currently hardcoded
	 * batterylist was created in prepareCSVRemaining() earlier
	 * TODO: get a better method of selecting the result than a number comparison. 
	 * @param outputfiledir
	 * @param batterycat
	 * @return
	 */
	public JSONObject giveResult(String outputfiledir,String batterycat) {
		JSONObject result=new JSONObject();
		List<Double[]> simulationResult=readOutput(outputfiledir);
		String choseniri=null;
		for(int d=0;d<simulationResult.size();d++) {
			for (ElectricalComponentObject var : batterylist) 
			{
			    if(var.getenv()==simulationResult.get(d)[0]&&var.getcost()==simulationResult.get(d)[1]&&var.getmatur()==simulationResult.get(d)[2]) {
			    	choseniri=var.getObjectIRI();
			    }
			    else if(var.getenv()==simulationResult.get(d)[0]&&var.getcost()==simulationResult.get(d)[1]) {
			    	choseniri=var.getObjectIRI();
			    }
			    else if(var.getcost()==simulationResult.get(d)[1]&&var.getmatur()==simulationResult.get(d)[2]) {
			    	choseniri=var.getObjectIRI();
			    }
			    else if(var.getenv()==simulationResult.get(d)[0]&&var.getmatur()==simulationResult.get(d)[2]) {
			    	choseniri=var.getObjectIRI();
			    }
			    
			}
		}
	
		result.put("storage",choseniri);
		batterylist.clear();
		return result;	
	}
	
	/** reads the result of the GAMS code and returns a list of arrays of doubles, which
	 * 
	 * 
	 * @param outputfiledir
	 * @return
	 */
	public List<Double[]> readOutput (String outputfiledir){
		int contentsize = new QueryBroker().readFileLocal(outputfiledir).split("      ").length;
		List<Double[]>simulationResult=new ArrayList<Double[]>();
		for(int x=1;x<contentsize;x+=3) {
			double venvironment=Double.valueOf(new QueryBroker().readFileLocal(outputfiledir).split("      ")[x]);
			double vcost=Double.valueOf(new QueryBroker().readFileLocal(outputfiledir).split("      ")[x+1]);
			double vmaturity=Double.valueOf(new QueryBroker().readFileLocal(outputfiledir).split("      ")[x+2].split("\\*")[0]);
			if(venvironment!=0.0&&vcost!=0.0&vmaturity!=0.0) {
				Double[]propbat=new Double[3];
				propbat[0]=venvironment;
				propbat[1]=vcost;
				propbat[2]=vmaturity;
				simulationResult.add(propbat);
			}
		}
		return simulationResult;
		
	}
	
	
	
	
	/** Constructs an OntModel of Electrical network, and determine the generators, bus numbers and respective locations
	 * 
	 * @param ENIRI
	 * @return
	 */
	public List<String> filterPV (String ENIRI){
		SelectBuilder sb = new SelectBuilder().addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#" )
				.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")				
				.addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
				.addVar("?entity").addVar("?valueofx").addVar("?valueofy").addVar("?BusNumbervalue")
				.addWhere("?entity" ,"a", "j1:PowerGenerator").addWhere("?entity" ,"j9:realizes", "<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SolarGeneration>")
				.addWhere("?entity" ,"j2:isModeledBy", "?model").addWhere("?model" ,"j5:hasModelVariable", "?num")
				.addWhere("?num" ,"a", "j3:BusNumber").addWhere("?num" ,"j2:hasValue", "?vnum")
				.addWhere("?vnum" ,"j2:numericalValue", "?BusNumbervalue")
				
				.addWhere("?entity" ,"j7:hasGISCoordinateSystem", "?coorsys")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_x", "?x")
				.addWhere("?x" ,"j2:hasValue", "?xval").addWhere("?xval" ,"j2:numericalValue", "?valueofx")
				.addWhere("?coorsys" ,"j7:hasProjectedCoordinate_y", "?y")
				.addWhere("?y" ,"j2:hasValue", "?yval").addWhere("?yval" ,"j2:numericalValue", "?valueofy");

		OntModel model=readModelGreedy(ENIRI);
		String busQuery = sb.buildString();
		List<String[]> resultList = queryResult(model, busQuery);
		List<String> pvGenIRI= new ArrayList<String>();
		for(int x=0;x<resultList.size();x++) {
			pvGenIRI.add(resultList.get(x)[0]);
		}
		return pvGenIRI;
	}
	/** feeds a query and gets a result
	 * 
	 * @param model
	 * @param query
	 * @return
	 */
	public static List<String[]> queryResult(OntModel model, String query) {
		
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return resultListfromquery;
	}
	
	public JSONObject optimizedBatteryMatching(String baseUrl, List<String> pvGenIRI, String batIRI) throws IOException {
		prepareCSVPahigh(pvGenIRI,baseUrl);
		prepareCSVRemaining(batIRI,baseUrl);		
		try {
			runGAMS(baseUrl);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			System.out.println("gams cannot run completely");
			logger.error(e.getMessage());
		}
		JSONObject result=giveResult(baseUrl+"/solutions.csv",batIRI);
		logger.info("selected battery = " + result.getString("storage"));
		return result;
	}
}
