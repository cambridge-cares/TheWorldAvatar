package uk.ac.cam.cares.jps.powsys.carbontax;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearGenType;

@WebServlet(urlPatterns = { "/optimizeforcarbontax" })
public class CarbonTaxAgent extends JPSHttpServlet {

	private static final long serialVersionUID = -2354646810093235777L;
	List<NuclearGenType>plant =new ArrayList<NuclearGenType>();	
	private Logger logger = LoggerFactory.getLogger(CarbonTaxAgent.class);
	private String modelname="prllelwrld_dynamicvar.gms";

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		//put the template file
		String newdir=QueryBroker.getLocalDataPath() ;
		copyTemplate(newdir, "time_profile.csv");
		//prepareCSVGeneratorParameter(jo.getString("electricalnetwork"),newdir);
		prepareCSVGeneratorParameterUpdatedGenScale(jo.getString("electricalnetwork"),newdir);
		
		BigDecimal carbontax = jo.getBigDecimal("carbontax");
		prepareConstantCSV(carbontax,newdir);
		
		try {
			logger.info("start optimization for carbon tax = " + carbontax);
			if (!AgentLocator.isJPSRunningForTest()) {
				runGAMS(newdir);
			}else { //18 oct 19
				//runGAMS(newdir); need to change if we want to run the gams code itself 
				String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir" + "/results.csv";
				File file = new File(source);
				String destinationUrl = newdir+"/results.csv";
				new QueryBroker().put(destinationUrl, file);
				
			}
			
			
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			logger.error("the gams not running successfully");
		}
				
		JSONObject result=giveResult(newdir+"/results.csv",plant);
		logger.info("optimization result = " + result);
		AgentCaller.printToResponse(result, response);
		plant.clear();
	}

	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().put(destinationUrl, file);
	}
	
	public void modifyTemplate(String newdir, String filename) throws IOException { 
		String destinationUrl = newdir + "/"+filename;
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
        String fileContext = FileUtils.readFileToString(file);
        fileContext = fileContext.replaceAll("constants.gdx",newdir+"/constants.gdx");
        fileContext = fileContext.replaceAll("Generator_Parameters.gdx",newdir+"/Generator_Parameters.gdx");
        fileContext = fileContext.replaceAll("time_profile.gdx",newdir+"/time_profile.gdx");
        
        fileContext = fileContext.replaceAll("constants.csv",newdir+"/constants.csv output="+newdir+"/constants.gdx");
        fileContext = fileContext.replaceAll("Generator_Parameters.csv",newdir+"/Generator_Parameters.csv output="+newdir+"/Generator_Parameters.gdx");
        fileContext = fileContext.replaceAll("time_profile.csv",newdir+"/time_profile.csv output="+newdir+"/time_profile.gdx");
       

        //FileUtils.write(file, fileContext);
 
		
		new QueryBroker().put(destinationUrl, fileContext);
	}
	
	
	
	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 
		
		//copyTemplate(baseUrl,"constants.gdx");
		//copyTemplate(baseUrl,"Generator_Parameters.gdx");
		//copyTemplate(baseUrl,"time_profile.gdx");
		//copyTemplate(baseUrl,"Final_parallel_wrld.gms");
		//copyTemplate(baseUrl,"gmsproj.gpr");
		
		
		modifyTemplate(baseUrl,modelname);

		
		logger.info("Start");
		//logger.info("separator= "+File.separator);
        String executablelocation ="C:/GAMS/win64/26.1/gams.exe"; //depends where is in claudius
        String folderlocation =baseUrl+"/";
        //String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/parallelworld/";
        String[] cmdArray = new String[5];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + modelname;
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "LO=2";

        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4];
        
        logger.info(cmdArrayinstring);
        Process p = Runtime.getRuntime().exec(cmdArray);
		   p.waitFor();
         
		   logger.info("Done");
	}
	
	public void prepareCSVGeneratorParameter(String ENiri, String baseUrl) {	
		
		//updated version so that plant owl file won't be used
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
				+ "SELECT ?entity ?plant ?Pmaxvalue ?emissionvalue " //add the emission value as optional
				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isSubsystemOf ?plant ." //plant
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax
				
				+ "OPTIONAL { ?entity j4:realizes ?genprocess ."
				+ "?genprocess j9:hasEmission ?emm ."
				+ "?emm a j9:Actual_CO2_Emission ."
				+ "?emm j2:hasValue ?valueemm ."
				+ "?valueemm j2:numericalValue ?emissionvalue ."
				+ " } "
				
				+ "}";
		
		String plantinfo = "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
				+ "SELECT ?entity ?vemission ?vcapa "
				+ "WHERE {?entity  a  cp:PowerPlant  ."
				+ "?entity   j4:designCapacity ?capa ."
				+ "?capa   j2:hasValue ?valuecapa . "
				+ "?valuecapa   j2:numericalValue ?vcapa ."
				+ "?entity   j3:realizes ?generation ."
				+ "?generation j5:hasEmission ?emission ." 
				+ "?emission   j2:hasValue ?valueemission . "
				+ "?valueemission   j2:numericalValue ?vemission ."
				+ "}";
		
		
		OntModel model = ENAgent.readModelGreedy(ENiri);
		QueryBroker broker = new QueryBroker();
	    	
    	ResultSet resultSet = JenaHelper.query(model, genInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
    	
		//logger.info("number of queried lot entities = " + resultList.size());

		
    	List<String>plantname =new ArrayList<String>();	
    	List<String[]> resultListforcsv =new ArrayList<String[]>();
    	String[] header = {"Type","Yr","Cap","Fix","OM","Fuel","Carb","Ri","Ci","a","b","c"};
    	String[] nuclear = {"n","7","1000000","8000","2.14","7.7","0","2","0","0","0","0"};
		
    	for (int i = 0; i < resultListfromquery.size(); i++) {
    		plantname.add(resultListfromquery.get(i)[1]);
    	}
    	
    	
    	/*IF IN THE FUTURE NEED TO READ FROM THE TEMPLATE*/
		String csv = new QueryBroker().readFile(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Generator_Parameters.csv");
		List<String[]> inputcontent = MatrixConverter.fromCsvToArray(csv);
    	
		List<String>uniqueplant=new ArrayList<>(new HashSet<>(plantname));
		System.out.println("uniqueplant size= "+uniqueplant.size());
		
		
		for(int c=0;c<uniqueplant.size();c++) {
			NuclearGenType a = new NuclearGenType(uniqueplant.get(c));
			Double sumofinstance=0.0;
			//summing the total capacity of each powerplant
			for (int i=0; i<resultListfromquery.size(); i++) {
				if(resultListfromquery.get(i)[1].contentEquals(uniqueplant.get(c))) {
					sumofinstance=sumofinstance+Double.valueOf(resultListfromquery.get(i)[2]);
				}
			}
			
			String resultplant = broker.queryFile(uniqueplant.get(c),plantinfo);
			String[] keysplant = JenaResultSetFormatter.getKeys(resultplant);
	    	List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(resultplant, keysplant);
//	    	System.out.println("1 result name= "+resultList.get(0)[0]);
//	    	System.out.println("1 result carbon= "+resultList.get(0)[1]);
	    	if(c+2<inputcontent.size()) {
		    	inputcontent.get(c+2)[0]="c"+Integer.valueOf(c);
		    	inputcontent.get(c+2)[6]=""+Double.valueOf(resultList.get(0)[1])/sumofinstance;
				inputcontent.get(c+2)[8]=""+sumofinstance;	
	    	}

			a.setcapacity(sumofinstance);
			a.setid("c"+c);
			plant.add(a);
			String[]current= new String[12];
			logger.info("plant added= "+a.getnucleargen());
			current[0]="c"+c; //what to write there???or uniqueplant.get(c)
			current[1]="2.5"; //for pacific light; 
			current[2]="1200000";//pacific light; 525000000 for keppel merlimau
			current[3]="8500"; //rand
			current[4]="7.33";//rand
			current[5]="0";//rand
			current[6]=""+Double.valueOf(resultList.get(0)[1])/sumofinstance; //in ton/MWh
			current[7]="10";//rand
			current[8]=""+sumofinstance;
			current[9]="1";//rand
			current[10]="2";//rand
			current[11]="1";//rand
			resultListforcsv.add(current);
		}
		
	    resultListforcsv.add(0, header);
	    resultListforcsv.add(1, nuclear);
	    String s = MatrixConverter.fromArraytoCsv(resultListforcsv);
	    if(inputcontent.size()-2==uniqueplant.size()) {
	    	System.out.println("it has the same number of gen!!!!");
	    	s=MatrixConverter.fromArraytoCsv(inputcontent);
	    }
	    broker.put(baseUrl + "/Generator_Parameters.csv", s);
	    
	    logger.info("generator input ok"); 
	}
	
	public static List<String[]> provideGenlist(String iriofnetwork) {
		String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity " 
				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
				+ "}";
		

		OntModel model = ENAgent.readModelGreedy(iriofnetwork);
		ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

		return resultListfromquery;
	}
	
	public void prepareCSVGeneratorParameterUpdatedGenScale(String ENiri, String baseUrl) {

		// updated version so that plant owl file won't be used
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity ?Pmaxvalue ?emissionfactor " // add the emission value as optional
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				//+ "?entity   j2:isSubsystemOf ?plant ." // plant
				+ "?entity   j2:isModeledBy ?model ." 
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ."
				+ "?pmax  j2:hasValue ?vpmax ." 
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "?entity j4:realizes ?genprocess ." 
				+ "?genprocess j9:usesGenerationTechnology ?tech ."
				+ "?tech j9:hasEmissionFactor ?emm ."
				+ "?emm j2:hasValue ?valueemm ."
				+ "?valueemm j2:numericalValue ?emissionfactor ." 
				//+ "}"
				+ "}";


		/*
		 * String plantinfo =
		 * "PREFIX cp:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
		 * +
		 * "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
		 * +
		 * "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
		 * +
		 * "PREFIX j4:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> "
		 * +
		 * "PREFIX j5:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
		 * + "SELECT ?entity ?vemission ?vcapa " + "WHERE {?entity  a  cp:PowerPlant  ."
		 * + "?entity   j4:designCapacity ?capa ." + "?capa   j2:hasValue ?valuecapa . "
		 * + "?valuecapa   j2:numericalValue ?vcapa ." +
		 * "?entity   j3:realizes ?generation ." +
		 * "?generation j5:hasEmission ?emission ." +
		 * "?emission   j2:hasValue ?valueemission . " +
		 * "?valueemission   j2:numericalValue ?vemission ." + "}";
		 */

		QueryBroker broker = new QueryBroker();
		List<String[]> resultListforcsv = new ArrayList<String[]>();
		List<String[]> genList = provideGenlist(ENiri);	
			System.out.println("size="+genList.size());	
		for(int d=0;d<genList.size();d++) {
			NuclearGenType a = new NuclearGenType(genList.get(d)[0]);

			System.out.println("MYGEN= "+genList.get(d)[0]);
			String result = broker.queryFile(genList.get(d)[0], genInfo);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			System.out.println("resultsize= "+resultListfromquery.size());
			a.setcapacity(Double.valueOf(resultListfromquery.get(0)[1]));
			a.setid("c"+d);
			plant.add(a);
			
			String[] current = new String[12];
			current[0] = "c" + d; // what to write there???or uniqueplant.get(c)
			//current[0] = resultListfromquery.get(0)[0];
			current[1] = "2.5"; // for pacific light;
			current[2] = "1200000";// pacific light; 525000000 for keppel merlimau
			current[3] = "8500"; // rand
			current[4] = "7.33";// rand
			current[5] = "0";// rand
			current[6] = resultListfromquery.get(0)[2]; // in ton/MWh
			current[7] = "10";// rand
			current[8] = resultListfromquery.get(0)[1]; //capacity in mw
			current[9] = "1";// rand
			current[10] = "2";// rand
			current[11] = "1";// rand
			resultListforcsv.add(current);
		}
		
		String[] header = { "Type", "Yr", "Cap", "Fix", "OM", "Fuel", "Carb", "Ri", "Ci", "a", "b", "c" };
		String[] nuclear = { "n", "7", "1000000", "8000", "2.14", "7.7", "0", "2", "0", "0", "0", "0" };



		/* IF IN THE FUTURE NEED TO READ FROM THE TEMPLATE */
		String csv = new QueryBroker()
				.readFile(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Generator_Parameters.csv");

		resultListforcsv.add(0, header);
		resultListforcsv.add(1, nuclear);
		String s = MatrixConverter.fromArraytoCsv(resultListforcsv);
		broker.put(baseUrl + "/Generator_Parameters.csv", s);

		logger.info("generator input ok");
	}
	
	public void prepareConstantCSV(BigDecimal tax,String baseUrl) {
		ArrayList<String[]> constant= new ArrayList<String[]> ();
		String[]header= {"Parameter","Value"};
		String[]delta= {"delta","0.05"};
		String[]tau= {"tau",""+tax};
		String[]D= {"D","0.1"};
		String[]L= {"L","30"};
		constant.add(header);
		constant.add(delta);
		constant.add(tau);
		constant.add(D);
		constant.add(L);
	    String s = MatrixConverter.fromArraytoCsv(constant);
	    new QueryBroker().put(baseUrl + "/Constants.csv", s);
	}
	
	public JSONObject giveResult(String outputfiledir,List<NuclearGenType>plant) {
		String content = new QueryBroker().readFile(outputfiledir);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(content);
		ArrayList<String>removedplant=new ArrayList<String>();
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		for(int d=1;d<simulationResult.size();d++) {
			if(Double.valueOf(simulationResult.get(d)[1])<0) {
				removedplant.add(simulationResult.get(d)[0].replace("\"", ""));
				//System.out.println("contain= "+simulationResult.get(d)[0].toString());
			}
		}
		for(NuclearGenType ind:plant) {
			//System.out.println("id= "+ind.getid());
			if(removedplant.contains(ind.getid())){
				//System.out.println(ind.getnucleargen());
				ja.put(ind.getnucleargen());
			}
		}
		result.put("substitutionalgenerators",ja);
		return result;	
	}
}
