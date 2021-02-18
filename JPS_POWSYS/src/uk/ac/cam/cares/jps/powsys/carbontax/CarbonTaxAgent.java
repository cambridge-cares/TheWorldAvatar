package uk.ac.cam.cares.jps.powsys.carbontax;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.io.FileUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearGenType;
import uk.ac.cam.cares.jps.powsys.util.Util;

@WebServlet(urlPatterns = { "/optimizeforcarbontax" })
public class CarbonTaxAgent extends JPSAgent {

	private static final long serialVersionUID = -2354646810093235777L;
	List<NuclearGenType>plant =new ArrayList<NuclearGenType>();	

	//private String modelname="prllelwrld_dynamicvar.gms";
	private String modelname="Final_parallel_wrld.gms";
    
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(CarbonTaxAgent.class);
    }
	Logger logger = LoggerFactory.getLogger(CarbonTaxAgent.class);
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);
			BigDecimal batIRI=requestParams.getBigDecimal("carbontax");	
	        return w;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

		if (!validateInput(requestParams)) {
			throw new JSONException ("CarbonTaxAgent input parameters invalid");
		}
		// put the template file
		String newdir = QueryBroker.getLocalDataPath() + "/GAMS_CarbonTaxAgent";
		copyTemplate(newdir, "time_profile.csv");
		// prepareCSVGeneratorParameter(jo.getString("electricalnetwork"),newdir);
		prepareCSVGeneratorParameterUpdatedGenScale(requestParams.getString("electricalnetwork"), newdir);

		BigDecimal carbontax = requestParams.getBigDecimal("carbontax");
		prepareConstantCSV(carbontax, newdir);

		logger.info("start optimization for carbon tax = " + carbontax);
		try {
			runGAMS(newdir);
		} catch (InterruptedException e1) {
			logger.error(e1.getMessage());
		} 
		catch (IOException e2) {
			
			logger.error(e2.getMessage());
		}
		
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir" + "/results.csv";
		File file = new File(source);
		String destinationUrl = newdir + "/results.csv";
		new QueryBroker().putLocal(destinationUrl, file);

		JSONObject result = giveResult(newdir + "/results.csv", plant);
		logger.info("optimization result = " + result);
		plant.clear();

		return result;
	}

	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
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
       
		new QueryBroker().putLocal(destinationUrl, fileContext);
	}
	
	
	
	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 
		
		
		modifyTemplate(baseUrl,modelname);

		
		logger.info("Start");
		//logger.info("separator= "+File.separator);
		String gamsLocation = System.getenv("GAMSDIR").split(";")[0];


		gamsLocation =gamsLocation.replace("\\", "/");
		gamsLocation =gamsLocation.replace("//", "/");
		String executablelocation = gamsLocation+"/gams.exe";
		String folderlocation =baseUrl; //+"/";
        //String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/parallelworld/";
        String[] cmdArray = new String[7];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "/" + modelname;
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "PROCDIR="+folderlocation;
        cmdArray[5] = "CURDIR="+folderlocation;
        cmdArray[6] = "LO=2";

        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4];
        
        CommandHelper.executeSingleCommand(baseUrl, cmdArrayinstring);
     
        logger.info("Done");
	}
	
	
	public static List<String[]> provideGenlist(String iriofnetwork) {
		//Filter Exists doesn't occur in SelectBuilder. 
//		String gennodeInfo = new SelectBuilder()
//				.addPrefix("j1", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
//				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
//				.addVar("?entity").addWhere("?entity", "a", "j1:PowerGenerator")
//				.addWhere("?entity", "j2:isSubsystemOf", "?plant")
//				.buildString();
		String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?entity " 
				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
				+ "}";
		

		OntModel model = ENAgent.readModelGreedy(iriofnetwork);
		List<String[]> resultListfromquery = Util.queryResult(model, gennodeInfo);

		return resultListfromquery;
	}
	
	public void prepareCSVGeneratorParameterUpdatedGenScale(String ENiri, String baseUrl) {

		// updated version so that plant owl file won't be used
		String genInfo = new SelectBuilder()
		.addPrefix("j1", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
		.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
		.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
		.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
		.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
		.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#")
		.addVar("?entity").addVar("?Pmaxvalue").addVar("?emissionfactor")
		.addWhere("?entity", "a", "j1:PowerGenerator")
		.addWhere("?entity", "j2:isModeledBy", "?model")
		.addWhere("?model", "j5:hasModelVariable", "?pmax")
        .addWhere("?pmax","a","j3:PMax")
        .addWhere("?pmax","j2:hasValue", "?vpmax")
        .addWhere("?vpmax","j2:numericalValue", "?Pmaxvalue")
		.addWhere("?entity", "j4:realizes", "?genprocess")
		.addWhere("?genprocess", "j9:usesGenerationTechnology", "?tech")
        .addWhere("?tech","j9:hasEmissionFactor","?emm")
        .addWhere("?emm","j2:hasValue", "?valueemm")
        .addWhere("?valueemm","j2:numericalValue", "?emissionfactor")
		.buildString();



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
			double indicator=Double.valueOf(resultListfromquery.get(0)[2]);
			current[0] = "c" + d; // what to write there???or uniqueplant.get(c)
			if(indicator>0.25) {//oil
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
				current[11] = "2";// rand
				resultListforcsv.add(current);
	    	}
	    	else {//gas
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
//				current[11] = "0.33";// rand
				current[11] = "1";// try to make the value higher 81119
				resultListforcsv.add(current);
	    	}
		}
		
		String[] header = { "Type", "Yr", "Cap", "Fix", "OM", "Fuel", "Carb", "Ri", "Ci", "a", "b", "c" };
		String[] nuclear = { "n", "7", "2000000", "8000", "2.14", "7.7", "0", "2", "0", "0", "0", "0" };
// cap change temporarily from 3000000


		/* IF IN THE FUTURE NEED TO READ FROM THE TEMPLATE */
		String csv = new QueryBroker()
				.readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Generator_Parameters.csv");

		resultListforcsv.add(0, header);
		resultListforcsv.add(1, nuclear);
		String s = MatrixConverter.fromArraytoCsv(resultListforcsv);
		broker.putLocal(baseUrl + "/Generator_Parameters.csv", s);

		logger.info("generator input ok");
	}
	
	public void prepareConstantCSV(BigDecimal tax,String baseUrl) {
		ArrayList<String[]> constant= new ArrayList<String[]> ();
		String[]header= {"Parameter","Value"};
		String[]delta= {"delta","0.05"};
		String[]tau= {"tau",""+tax};
		String[]D= {"D","0.02"};
		String[]L= {"L","30"};
		constant.add(header);
		constant.add(delta);
		constant.add(tau);
		constant.add(D);
		constant.add(L);
	    String s = MatrixConverter.fromArraytoCsv(constant);
	    new QueryBroker().putLocal(baseUrl + "/Constants.csv", s);
	}
	
	public JSONObject giveResult(String outputfiledir,List<NuclearGenType>plant) {
		String content = new QueryBroker().readFileLocal(outputfiledir);
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
