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

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		//put the template file
		String newdir=QueryBroker.getLocalDataPath() ;
		String filename="time_profile.csv";
		copyTemplate(newdir, filename);
//		copyTemplate(newdir, "Generator_Parameters.csv");
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
		
//		JSONObject result = new JSONObject();
//		JSONArray ja = new JSONArray();
//		ja.put("http://www.theworldavatar.com/kb/powerplants/Keppel_Merlimau_Cogen_Power_Plant_Singapore.owl#Keppel_Merlimau_Cogen_Power_Plant_Singapore");
//		ja.put("http://www.theworldavatar.com/kb/powerplants/SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore.owl#SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore");
//		ja.put("http://www.theworldavatar.com/kb/powerplants/Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore.owl#Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore");
//		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_OCGT_Power_Plant_Singapore.owl#PowerSeraya_OCGT_Power_Plant_Singapore");
//		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore.owl#PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore");
//		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore.owl#PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore");
//		result.put("substitutionalpowerplants", ja);
		
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
        fileContext = fileContext.replaceAll("constants.csv",newdir+"/constants.csv output="+newdir+"/constants.gdx");
        fileContext = fileContext.replaceAll("Generator_Parameters.csv",newdir+"/Generator_Parameters.csv output="+newdir+"/Generator_Parameters.gdx");
        fileContext = fileContext.replaceAll("time_profile.csv",newdir+"/time_profile output="+newdir+"/time_profile.gdx");
       
        fileContext = fileContext.replaceAll("$GDXIN constants.gdx","$GDXIN "+newdir+"/constants.gdx");
        fileContext = fileContext.replaceAll("$GDXIN Generator_Parameters.gdx","$GDXIN "+newdir+"/Generator_Parameters.gdx");
        fileContext = fileContext.replaceAll("$GDXIN time_profile.gdx","$GDXIN "+newdir+"/time_profile.gdx");
        //FileUtils.write(file, fileContext);
 
		
		new QueryBroker().put(destinationUrl, fileContext);
	}
	
	
	
	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 
		
		//copyTemplate(baseUrl,"constants.gdx");
		//copyTemplate(baseUrl,"Generator_Parameters.gdx");
		//copyTemplate(baseUrl,"time_profile.gdx");
		//copyTemplate(baseUrl,"Final_parallel_wrld.gms");
		//copyTemplate(baseUrl,"gmsproj.gpr");
		
		
		modifyTemplate(baseUrl,"Final_parallel_wrld.gms");

		
		logger.info("Start");
		logger.info("separator= "+File.separator);
        String executablelocation ="C:/GAMS/win64/26.1/gams.exe"; //depends where is in claudius
        String folderlocation =baseUrl+"/";
        //String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/parallelworld/";
        String[] cmdArray = new String[5];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "Final_parallel_wrld.gms";
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
				+ "SELECT ?entity ?vemission "
				+ "WHERE {?entity  a  cp:PowerPlant  ."
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
    	String[] nuclear = {"n","7","8000000","85000","2.14","7.7","0","2","0","0","0","0"};
		
    	for (int i = 0; i < resultListfromquery.size(); i++) {
    		plantname.add(resultListfromquery.get(i)[1]);
    	}
    	
		List<String>uniqueplant=new ArrayList<>(new HashSet<>(plantname));
		System.out.println("uniqueplant size= "+uniqueplant.size());
		for(int c=0;c<uniqueplant.size();c++) {
			NuclearGenType a = new NuclearGenType(uniqueplant.get(c));
			Double sumofinstance=0.0;
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

			
			a.setcapacity(sumofinstance);
			a.setid("c"+c);
			plant.add(a);
			String[]current= new String[12];
			current[0]="c"+c; //what to write there???or uniqueplant.get(c)
			current[1]="0";
			current[2]=""+sumofinstance;
			current[3]="0";
			current[4]="0";
			current[5]="0";
			current[6]=resultList.get(0)[1];
			current[7]="0";
			current[8]="0";
			current[9]="0";
			current[10]="0";
			current[11]="0";
			resultListforcsv.add(current);
		}
		
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
		int index=1;
		ArrayList<String>removedplant=new ArrayList<String>();
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		while(Double.valueOf(simulationResult.get(index)[1])<0) {
			
			removedplant.add(simulationResult.get(index)[0]);
			index++;
		}
		for(NuclearGenType ind:plant) {
			if(removedplant.contains(ind.getid())){
				ja.put(ind.getnucleargen());
			}
			
		}
		result.put("substitutionalpowerplants",ja);
		return result;	
	}
}
