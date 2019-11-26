package uk.ac.cam.cares.jps.ess;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


@WebServlet(urlPatterns = { "/ESSAgent" })

public class EnergyStorageSystem extends JPSHttpServlet {

	private static final long serialVersionUID = -4199209974912271432L;
	private Logger logger = LoggerFactory.getLogger(EnergyStorageSystem.class);
	//public static final String AGENT_TAG = "GAMS_NuclearAgent";
	private String modelname="NESS.gms";
	
    List<ElectricalComponentObject>batterylist=new ArrayList<ElectricalComponentObject>();
    
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;

	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 		
		
		modifyTemplate(baseUrl,modelname);

		
		logger.info("Start");
		//logger.info("separator= "+File.separator);
//        String executablelocation ="C:/GAMS/win64/28.2/gams.exe"; //depends where is in claudius
		 String executablelocation ="C:/GAMS/win64/26.1/gams.exe"; //depends where is in claudius
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
 
		
		new QueryBroker().put(destinationUrl, fileContext);
	}
	
	/*
	 * public void copyTemplate(String newdir, String filename) { File file = new
	 * File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
	 * 
	 * String destinationUrl = newdir + "/"+filename; new
	 * QueryBroker().put(destinationUrl, file); }
	 */
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
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
	
	public ArrayList<String[]> readResultfromtxt(String outputfiledir, int colnum) throws IOException {
		ArrayList<String[]> entryinstance = new ArrayList<String[]>();
		
		logger.info("reading result from " + outputfiledir);
		String content = new QueryBroker().readFile(outputfiledir);
		StringReader stringreader = new StringReader(content);
		CSVReader reader = null;
		try {
			reader = new CSVReader(stringreader, '\t');
			//CSVReader reader = new CSVReader(new FileReader(outputfiledir), '\t');
			String[] record;
			while ((record = reader.readNext()) != null) {
				int element = 0;
				String[] entityline = new String[colnum];
				for (String value : record) {
	
					entityline[element] = value;
					element++;
				}
				entryinstance.add(entityline);
	
			}
		} finally {
			reader.close();
		}
		return entryinstance;
	}
	
	public void prepareCSVPahigh(String PVNetworkiri, String baseUrl) {
		
		//System.out.println("model= "+model);

		String batteryquery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?Pa_high ?Da_low ?Pa_low ?Da_high " 
				+ "WHERE {?entity  a  j1:PhotovoltaicGenerator  ."

				+ "?entity   j6:hasMaximumActivePowerGenerated ?Pmax ." 
				+ "?Pmax     j2:hasValue ?vPmax ."
				+ "?vPmax  j5:upperLimit ?Pa_high ."

				+ "?entity   j6:hasMinimumActivePowerGenerated ?Pmin ." 
				+ "?Pmin     j2:hasValue ?vPmin ."
				+ "?vPmin  j5:lowerLimit ?Pa_low ."

				+ "?entity   j6:hasStateOfCharge ?dt ." 
				+ "?dt     j2:hasValue ?vdt ."
				+ "?vdt  j5:upperLimit ?Da_high ." 
				+ "?vdt  j5:lowerLimit ?Da_low ."
				+ "}";
		//?Pa_low ?Pa_high ?Da_high ?Da_low 
		
        String result = new QueryBroker().queryFile(PVNetworkiri, batteryquery);
        String[] keyspv = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keyspv);


		List<String[]> resultListforcsv = new ArrayList<String[]>();
		String[] header = { "Parameters", "Value" };
		resultListforcsv.add(header);
		for (int x = 0; x < resultList.get(0).length; x++) {
			String[] line = { keyspv[x], resultList.get(0)[x] };
			resultListforcsv.add(line);
		}
		String s = MatrixConverter.fromArraytoCsv(resultListforcsv);
		new QueryBroker().put(baseUrl + "/Pa_high.csv", s);
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
		new QueryBroker().put(baseUrl + "/"+filename, MatrixConverter.fromArraytoCsv(resultListforcsv));
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
	
		result.put("battery",choseniri);
		batterylist.clear();
		return result;	
	}
	
	public List<Double[]> readOutput (String outputfiledir){
		int contentsize = new QueryBroker().readFile(outputfiledir).split("      ").length;
		List<Double[]>simulationResult=new ArrayList<Double[]>();
		for(int x=1;x<contentsize;x+=3) {
			double venvironment=Double.valueOf(new QueryBroker().readFile(outputfiledir).split("      ")[x]);
			double vcost=Double.valueOf(new QueryBroker().readFile(outputfiledir).split("      ")[x+1]);
			double vmaturity=Double.valueOf(new QueryBroker().readFile(outputfiledir).split("      ")[x+2].split("\\*")[0]);
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
	
	public double[] prepareBatteryLocationData(String indexline, String baseUrl, OntModel model) throws IOException {
		String content = new QueryBroker().readFile(baseUrl + "/mappingforbranch" + ".csv");
		// System.out.println("dir= "+content);
		List<String[]> readinglist = MatrixConverter.fromCsvToArray(content);
		int r = readinglist.size();
		String branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "SELECT ?entity ?num1 ?num2 " + "WHERE {?entity  a  j1:UndergroundCable  ."
				+ "?entity   j2:hasInput ?num1 ." + "?entity   j2:hasOutput ?num2 ." + "}";

		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?entity ?valx ?valy " + "WHERE {?entity  a  j1:BusNode  ."
				+ "?entity   j3:hasGISCoordinateSystem ?coor ." + "?coor  j3:hasProjectedCoordinate_x ?x ."
				+ "?x  j2:hasValue ?valuex ." + "?valuex  j2:numericalValue ?valx ."
				+ "?coor  j3:hasProjectedCoordinate_y ?y ." + "?y  j2:hasValue ?valuey ."
				+ "?valuey  j2:numericalValue ?valy ." + "}";

		ResultSet resultSet = JenaHelper.query(model, busInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListbus = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		List<ElectricalComponentObject> buslist = new ArrayList<ElectricalComponentObject>();

		for (int x = 0; x < resultListbus.size(); x++) {
			ElectricalComponentObject bus = new ElectricalComponentObject(resultListbus.get(x)[0]);
			bus.setx(Double.valueOf(resultListbus.get(x)[1]));
			bus.sety(Double.valueOf(resultListbus.get(x)[2]));
			buslist.add(bus);
		}

		int t = 0;
		double[] ans = new double[2];
		ans[0] = 0.0;
		ans[1] = 0.0;
		while (t < r) {
			if (readinglist.get(t)[1].equals(indexline)) {
				String result2 = new QueryBroker().queryFile(readinglist.get(t)[0], branchInfo);
				String[] keys2 = JenaResultSetFormatter.getKeys(result2);

//				System.out.println("keys="+keys2.length);
				List<String[]> resultListbranch = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
//				System.out.println("sizeofresultbranch="+resultListbranch.size());
				String bus1 = resultListbranch.get(0)[1];
				String bus2 = resultListbranch.get(0)[2];
				double bus1x = 0.0;
				double bus1y = 0.0;
				double bus2x = 0.0;
				double bus2y = 0.0;
				for (int h = 0; h < buslist.size(); h++) {
					if (buslist.get(h).getObjectIRI().contains(bus1)) {
						bus1x = buslist.get(h).getx();
						bus1y = buslist.get(h).gety();
					}
					if (buslist.get(h).getObjectIRI().contains(bus2)) {
						bus2x = buslist.get(h).getx();
						bus2y = buslist.get(h).gety();
					}
				}
				double xbat = (bus1x + bus2x) / 2;
				double ybat = (bus1y + bus2y) / 2;
				System.out.println("x= " + xbat);
				System.out.println("y= " + ybat);

				ans[0] = xbat;
				ans[1] = ybat;
			}
			t++;
		}
		return ans;
	}
		
	public void initOWLClasses(OntModel jenaOwlModel) {
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		
	}
	
	
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String baseUrl = QueryBroker.getLocalDataPath() + "/GAMS_ESS";
		System.out.println("baseURL: " + baseUrl);
		JSONObject jofornuc = AgentCaller.readJsonParameter(request);
		String pvGenIRI=jofornuc.getString("RenewableEnergyGenerator");
		String batIRI=jofornuc.getString("BatteryCatalog");
		String ENIRI=jofornuc.getString("electricalnetwork");
		System.out.println("GENERATOR: " + pvGenIRI);
		System.out.println("parameter got= "+jofornuc.toString());
		
		JSONObject resultofbattery = optimizedBatteryMatching(baseUrl, pvGenIRI, batIRI);
		//1st step done
		
		
		//modified the EN with the additional renewable gen added
		JSONObject jo = new JSONObject();
		JSONArray value1 = new JSONArray();
		JSONArray value2 = new JSONArray();
		jo.put("electricalnetwork", ENIRI);
		value1.put(pvGenIRI);
		jo.put("renewableGens", value1);
		jo.put("substitutionalgenerators", value2);
		AgentCaller.executeGet("JPS_POWSYS/retrofit", jo.toString());
		
		
		//run the scenario for EN after it is retrofitted
		logger.info("starting the OPF");
		String scenarioUrl = BucketHelper.getScenarioUrl("POWSYS-ESS-OPFCallAgent");
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		
		String usecaseUrl = BucketHelper.getUsecaseUrl();
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
		String dir = new JSONObject(resultStart).getString("folder");
		
		//make battery owl file
		ArrayList<String[]> resultfrommodelbranch = readResultfromtxt(dir + "/outputBranch" + "OPF" + ".txt", 6);
		int size=resultfrommodelbranch.size();
		double standard=0.003;
		int d=0;
		OntModel model = readModelGreedy(ENIRI);
		QueryBroker broker=new QueryBroker();
			while(Double.valueOf(resultfrommodelbranch.get(d)[1])<standard&&d<size) {
				double[]coordinate=prepareBatteryLocationData(resultfrommodelbranch.get(d)[0],dir,model);
				double x=coordinate[0];
				double y=coordinate[1];
				double capacity=Double.valueOf(resultfrommodelbranch.get(d)[1]);
				String typebat=resultofbattery.getString("battery").split("#")[1];
				String content=broker.readFile(resultofbattery.getString("battery"));
				
				content.replaceAll(typebat+".owl",typebat+"-"+String.format("%03d", d+1)+".owl"); //individual file name changed
				
				OntModel bat= new JenaHelper().createModel();
				new JenaHelper().readFromString(content, bat);
				
				Individual gencoordinate = coordinatesystemclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#CoordinateSystem_of_"+typebat);
				Individual xgencoordinate = coordinateclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#x_coordinate_of_"+typebat);
				Individual ygencoordinate = coordinateclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1)+ ".owl#y_coordinate_of_"+typebat);
				Individual xgencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#v_x_coordinate_of_"+typebat);
				Individual ygencoordinatevalue = valueclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#v_y_coordinate_of_"+typebat);
				Individual activepowerbalance=powerbalanceclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#ActivePowerInjection_of_"+typebat);
				Individual activepowerbalancevalue=scalarvalueclass.createIndividual(iriprefix + typebat+"-"+String.format("%03d", d+1) + ".owl#V_ActivePowerInjection_of_"+typebat);
				xgencoordinatevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (x)));
				xgencoordinatevalue.addProperty(hasunit, degree);
				ygencoordinatevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (y)));
				ygencoordinatevalue.addProperty(hasunit, degree);
				activepowerbalancevalue.setPropertyValue(numval, bat.createTypedLiteral(new Double (capacity)));
				activepowerbalancevalue.addProperty(hasunit, MW);
	
				broker.put(resultofbattery.getString("battery"), new JenaHelper().writeToString(bat));
			}
			
		
		
		AgentCaller.printToResponse(resultofbattery, response);
	}

	public JSONObject optimizedBatteryMatching(String baseUrl, String pvGenIRI, String batIRI) throws IOException {
		OntModel modelbattery=readBatteryGreedy(batIRI);
		prepareCSVPahigh(pvGenIRI,baseUrl);
		prepareCSVRemaining(batIRI,baseUrl,modelbattery);
		
//		copyTemplate(baseUrl, "Ptlow.csv");
//		copyTemplate(baseUrl, "Pthigh.csv");
//		copyTemplate(baseUrl, "Dtlow.csv");
//		copyTemplate(baseUrl, "Dthigh.csv");
//		copyTemplate(baseUrl, "EnvironmentalScore.csv");
//		copyTemplate(baseUrl, "EconomicalScore.csv");
//		copyTemplate(baseUrl, "Maturity.csv");
		
		
		try {
			runGAMS(baseUrl);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			System.out.println("gams cannot run completely");
			logger.error(e.getMessage());
		}
		JSONObject result=giveResult(baseUrl+"/solutions.csv",batIRI,modelbattery);
		logger.info("selected battery = " + result.getString("battery"));
		return result;
	}
}
