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
		prepareCSVGeneratorParameter(jo.getString("electricalnetwork"),newdir);
		
		BigDecimal carbontax = jo.getBigDecimal("carbontax");
		prepareConstantCSV(carbontax,newdir);
		
		try {
			logger.info("start optimization for carbon tax = " + carbontax);
			runGAMS(newdir);
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
		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?plant ?Pmaxvalue "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isSubsystemOf ?plant ." //plant
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

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
//		int inputsize=inputcontent.size();
//		int b=1;
//		while(b<=inputsize) {
//			inputcontent.get(b)[0]="c"+Integer.valueOf(b-1);
//			inputcontent.get(b)[6]="c"+Integer.valueOf(b-1);
//			inputcontent.get(b)[8]="c"+Integer.valueOf(b-1);	
//		}
    	
    	
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
	    	System.out.println("it has the same number of plant!!!!");
	    	s=MatrixConverter.fromArraytoCsv(inputcontent);
	    }
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
		result.put("substitutionalpowerplants",ja);
		return result;	
	}
}
