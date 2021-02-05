package uk.ac.cam.cares.jps.ess;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


@WebServlet(urlPatterns = { "/ESSAgent"})

public class EnergyStorageSystem extends JPSHttpServlet {

	private static final long serialVersionUID = -4199209974912271432L;
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(EnergyStorageSystem.class);
    }
	//public static final String AGENT_TAG = "GAMS_NuclearAgent";
	private String modelname="NESS.gms";
	
	   Logger logger = LoggerFactory.getLogger(EnergyStorageSystem.class);
    List<ElectricalComponentObject>batterylist=new ArrayList<ElectricalComponentObject>();
    
    /** code that should run GAMS
     * 
     * @param baseUrl
     * @throws IOException
     * @throws InterruptedException
     */
 	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 		
		
		modifyTemplate(baseUrl,modelname);

		
		logger.info("Start");
		//logger.info("separator= "+File.separator);
		//If user does not have GAMSDIR on 
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
        
        System.out.println(cmdArrayinstring);
        try {
        	
            Process p = Runtime.getRuntime().exec(cmdArray);
            BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String s = null;
            while((s=stdInput.readLine()) !=null){
               System.out.println(s);
            }
            p.waitFor();
     }
     catch (java.io.IOException e )
     {
            System.err.println(">>>>" + e.getMessage() );
            e.printStackTrace();
     }
     catch (InterruptedException e )
     {
            System.err.println(">>>>" + e.getMessage() );
            e.printStackTrace();
     }
		   System.out.println("Done Processing");
	}
	
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
	
	public static OntModel readBatteryGreedy(String batterycatiri) {
		String batteryInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." 
				+ "?entity   j2:contains ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(batterycatiri, batteryInfo);
	}
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	
	public void prepareCSVPahigh(List<String> pvGenIRI, String baseUrl) {

		// System.out.println("model= "+model);

		String pvquery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?Pa_high ?Da_low ?Pa_low ?Da_high " 
				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				
				+ "?entity   j6:hasMaximumActivePowerGenerated ?Pmax ." 
				+ "?Pmax     j2:hasValue ?vPmax ."
				+ "?vPmax  j5:upperLimit ?Pa_high ."

				+ "?entity   j6:hasMinimumActivePowerGenerated ?Pmin ." 
				+ "?Pmin     j2:hasValue ?vPmin ."
				+ "?vPmin  j5:lowerLimit ?Pa_low ."

				+ "?entity   j6:hasStateOfCharge ?dt ." 
				+ "?dt     j2:hasValue ?vdt ."
				+ "?vdt  j5:upperLimit ?Da_high ." 
				+ "?vdt  j5:lowerLimit ?Da_low ." + "}";

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
	
	public void prepareCSVRemaining(String batcal, String baseUrl,OntModel model) {
		
		//System.out.println("model= "+model);

		String batteryquery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?environment ?cost ?maturity ?vPtHigh ?vPtLow ?DTHigh ?DTLow " 
				+ "WHERE {"
				+ "?entity   j2:hasProperty ?MC ." 
				+ "?MC     j2:hasValue ?vMC ."
				+ "?vMC  j2:numericalValue ?environment ."

				+ "?entity   j3:hasCost ?EC ." 
				+ "?EC     j2:hasValue ?vEC ."
				+ "?vEC  j2:numericalValue ?cost ."

				+ "?entity   j1:hasNumberOfCells ?no ." 
				+ "?no     j2:hasValue ?vno ."
				+ "?vno  j2:numericalValue ?maturity ." 
				
				+ "?entity   j6:hasMaximumActivePowerGenerated ?PMax ." 
				+ "?PMax     j2:hasValue ?PtHigh ."
				+ "?PtHigh  j2:numericalValue ?vPtHigh ."
				
				+ "?entity   j6:hasMinimumActivePowerGenerated ?PMin ." 
				+ "?PMin     j2:hasValue ?PtLow ."
				+ "?PtLow  j2:numericalValue ?vPtLow ."
				
				+ "?entity   j6:hasStateOfCharge ?DT ." 
				+ "?DT     j2:hasValue ?valDT ."
				+ "?valDT j5:upperLimit ?DTHigh ."
				+ "?valDT j5:lowerLimit ?DTLow ."
				+ "}"; 
		
		
		ResultSet resultSet = JenaHelper.query(model, batteryquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keysbat = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keysbat);
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
	
	
	public JSONObject giveResult(String outputfiledir,String batterycat,OntModel model) { //unfinished yet
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
	
	
	 @Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

		 	System.gc();//garbage collection (reduce memory consumption)
			JSONObject joforess = requestParams;
			String baseUrl = QueryBroker.getLocalDataPath() + "/GAMS_ESS";
			System.out.println("baseURL: " + baseUrl);
			String batIRI=joforess.getString("BatteryCatalog");
			String ENIRI=joforess.getString("electricalnetwork");
			
			List<String> pvGenIRI=filterPV(ENIRI);
			
			//System.out.println("GENERATOR: " + pvGenIRI);
			System.out.println("parameter got= "+joforess.toString());
						
			JSONObject resultofbattery = null;
			try {
				resultofbattery = optimizedBatteryMatching(baseUrl, pvGenIRI, batIRI);
				return resultofbattery;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				logger.error(e.getMessage());
			}
			
		    System.gc();

	
			return resultofbattery;
		}
	
/*	public List<String[]> getBatteryCoord(OntModel model) { must be the complete model (not needed at the moment
		String gencoordinate = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
				+ "SELECT ?entity ?valueofx ?valueofy "
				+ "WHERE {?entity  a  ?class ."
				+ "?class rdfs:subClassOf j1:Battery ." 
				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

				+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
				+ "?y  j2:hasValue ?vy ." 
				+ "?vy  j2:numericalValue ?valueofy ."

				+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
				+ "?x  j2:hasValue ?vx ." 
				+ "?vx  j2:numericalValue ?valueofx ."
				+ " {?class rdfs:subClassOf j1:Battery ."
				+ "} "
				+ "UNION { ?class rdfs:subClassOf j1:EnergyStorageSystem . } ."
				+ "}";
			ResultSet resultSet = JenaHelper.query(model, gencoordinate);
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			
		return resultList;
	}*/
	
	public List<String> filterPV (String ENIRI){
	OntModel model=readModelGreedy(ENIRI);
		
		String queryGenerator = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "SELECT ?entity ?valueofx ?valueofy ?BusNumbervalue "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j9:realizes <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SolarGeneration> ."
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?num ." 
				+ "?num  a  j3:BusNumber  ." 
				+ "?num  j2:hasValue ?vnum ."
				+ "?vnum   j2:numericalValue ?BusNumbervalue ." // number

				+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
				+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
				+ "?y  j2:hasValue ?vy ." 
				+ "?vy  j2:numericalValue ?valueofy ."

				+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
				+ "?x  j2:hasValue ?vx ." 
				+ "?vx  j2:numericalValue ?valueofx ."

				+ "}";
		
		ResultSet resultSet = JenaHelper.query(model, queryGenerator);
		String resultquerypv = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(resultquerypv);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(resultquerypv, keys);

		List<String> pvGenIRI= new ArrayList<String>();
		for(int x=0;x<resultList.size();x++) {
			pvGenIRI.add(resultList.get(x)[0]);
		}
		logger.info("PV iri selected= "+pvGenIRI.size());
		logger.info("===================================================================================");
		return pvGenIRI;
		
	}

	public JSONObject optimizedBatteryMatching(String baseUrl, List<String> pvGenIRI, String batIRI) throws IOException {
		OntModel modelbattery=readBatteryGreedy(batIRI);
		
		prepareCSVPahigh(pvGenIRI,baseUrl);
		prepareCSVRemaining(batIRI,baseUrl,modelbattery);		
		try {
			runGAMS(baseUrl);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			System.out.println("gams cannot run completely");
			logger.error(e.getMessage());
		}
		JSONObject result=giveResult(baseUrl+"/solutions.csv",batIRI,modelbattery);
		logger.info("selected battery = " + result.getString("storage"));
		return result;
	}
}
