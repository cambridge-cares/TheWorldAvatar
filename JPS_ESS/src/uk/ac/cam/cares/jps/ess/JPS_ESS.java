package uk.ac.cam.cares.jps.ess;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;


@WebServlet(urlPatterns = { "/ESSAgent" })

public class JPS_ESS extends JPSHttpServlet {

	private static final long serialVersionUID = -4199209974912271432L;
	private Logger logger = LoggerFactory.getLogger(JPS_ESS.class);
	//public static final String AGENT_TAG = "GAMS_NuclearAgent";
	private String modelname="NESS.gms";

	public static void runGAMSOld() throws IOException, InterruptedException {
		System.out.println("Start");
		System.out.println("separator= " + File.separator);
		String executablelocation = "C:/GAMS/win64/27.3/gams.exe";
		// String folderlocation ="D:/Users/KADIT01/Documents/gamsdir/projdir/";
		String folderlocation = "C:/Users/GKAR01/Documents/gamsdir/projdir/";
		String[] cmdArray = new String[5];

		cmdArray[0] = executablelocation;
		cmdArray[1] = folderlocation + "NESS.gms";
		cmdArray[2] = "WDIR=" + folderlocation;
		cmdArray[3] = "SCRDIR=" + folderlocation;
		cmdArray[4] = "LO=2";

		String cmdArrayinstring = cmdArray[0] + " " + cmdArray[1] + "," + cmdArray[2] + "," + cmdArray[3] + " "
				+ cmdArray[4];

//		System.out.println(cmdArrayinstring);
//        Process p = Runtime.getRuntime().exec(cmdArray);
//		   p.waitFor();
		// String startbatCommand ="C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat";

		ArrayList<String> groupcommand = new ArrayList<String>();

		groupcommand.add("C:/Users/GKAR01/Documents/gamsdir/projdir/gamsexecute.bat");

		// CommandHelper.executeSingleCommand(folderlocation,startbatCommand);
		CommandHelper.executeCommands(folderlocation, groupcommand);
		System.out.println("Done");
	}


	public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location 		
		
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
	
	public void modifyTemplate(String newdir, String filename) throws IOException { 
		String destinationUrl = newdir + "/"+filename;
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
        String fileContext = FileUtils.readFileToString(file);
        fileContext = fileContext.replaceAll("Ptlow.gdx",newdir+"/Ptlow.gdx");
        fileContext = fileContext.replaceAll("Pthigh.gdx",newdir+"/Pthigh.gdx");
        fileContext = fileContext.replaceAll("Dtlow.gdx",newdir+"/Dtlow.gdx");
        fileContext = fileContext.replaceAll("Dthigh.gdx",newdir+"/Dthigh.gdx");
        fileContext = fileContext.replaceAll("EnvironmentalScore.gdx",newdir+"/EnvironmentalScore.gdx");
        fileContext = fileContext.replaceAll("EconomicalScore.gdx",newdir+"/EconomicalScore.gdx");
        fileContext = fileContext.replaceAll("Maturity.gdx",newdir+"/Maturity.gdx");
        
        fileContext = fileContext.replaceAll("Ptlow.csv",newdir+"/Ptlow.csv output="+newdir+"/Ptlow.gdx");
        fileContext = fileContext.replaceAll("Pthigh.csv",newdir+"/Pthigh.csv output="+newdir+"/Pthigh.gdx");
        fileContext = fileContext.replaceAll("Dtlow.csv",newdir+"/Dtlow.csv output="+newdir+"/Dtlow.gdx");
        fileContext = fileContext.replaceAll("Dthigh.csv",newdir+"/Dthigh.csv output="+newdir+"/Dthigh.gdx");
        fileContext = fileContext.replaceAll("EnvironmentalScore.csv",newdir+"/EnvironmentalScore.csv output="+newdir+"/EnvironmentalScore.gdx");
        fileContext = fileContext.replaceAll("EconomicalScore.csv",newdir+"/EconomicalScore.csv output="+newdir+"/EconomicalScore.gdx");
        fileContext = fileContext.replaceAll("Maturity.csv",newdir+"/Maturity.csv output="+newdir+"/Maturity.gdx");

        //FileUtils.write(file, fileContext);
 
		
		new QueryBroker().put(destinationUrl, fileContext);
	}
	
	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().put(destinationUrl, file);
	}
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String baseUrl = QueryBroker.getLocalDataPath() + "/JPS_ESS";
		JSONObject jofornuc = AgentCaller.readJsonParameter(request);
		String PVNetworkiri=jofornuc.getString("PVNetwork");
		
		System.out.println("parameter got= "+jofornuc.toString());

		String sparqlQuery = "PREFIX rdf:<http://www.w3.org/2001/XMLSchema#>\r\n"
				+ "PREFIX App:<http://www.theworldavatar.com/kb/sgp/pvsingaporenetwork.owl#>\r\n"
				+ "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>\r\n"
				+ "PREFIX PV:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PhotovoltaicGenerator>\r\n"
				+ "\r\n" + "SELECT ?Pa_low ?Pa_high  ?Da_high ?Da_low \r\n" + "WHERE \r\n" + "{ \r\n"
				+ "  ?pv a <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PhotovoltaicGenerator> .\r\n"
				+ "  ?pv  <http://www.theworldavatar.com/kb/sgp/pvsingaporenetwork.owl#hasMaximumActivePowerGenerated> ?apg .\r\n"
				+ "  ?apg  <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?v_apg.\r\n"
				+ "  ?v_apg  <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#upperLimit> ?Pa_high.\r\n"
				+ "  ?pv  <http://www.theworldavatar.com/kb/sgp/pvsingaporenetwork.owl#hasMinimumActivePowerGenerated> ?mapg .\r\n"
				+ "  ?mapg  <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?v_mapg.\r\n"
				+ "  ?v_mapg  <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#lowerLimit> ?Pa_low.\r\n"
				+ "  \r\n"
				+ "  ?pv  <http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasStateOfCharge> ?dt . \r\n"
				+ "  ?dt  <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue> ?v_dt.\r\n"
				+ "  ?v_dt <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#lowerLimit> ?Da_low.\r\n"
				+ "  ?v_dt <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#upperLimit> ?Da_high.\r\n"
				+ "}\r\n" + "";

		QueryExecution qe = QueryExecutionFactory
				.sparqlService("http://www.theworldavatar.com/damecoolquestion/pvsingaporenetwork/query", sparqlQuery);

		ResultSet results = qe.execSelect();

		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		List<QuerySolution> result_list = ResultSetFormatter.toList(results);

		String text = "Parameters, Value\n";
		for (QuerySolution solution : result_list) {
			Iterator<String> vars = solution.varNames();
			while (vars.hasNext()) {
				String var_name = vars.next();
				String value = solution.getLiteral(var_name).getString();
				String line = var_name + "," + value + "\n";
				System.out.println(line);
				text = text + line;
			}
		}

//		File file = new File(baseUrl+"/Pa_high.csv");
//		FileWriter writer = new FileWriter(file);
		new QueryBroker().put(baseUrl+"/Pa_high.csv", text);
//        csvWriter.flush();
//        csvWriter.close();
		
		copyTemplate(baseUrl, "Ptlow.csv");
		copyTemplate(baseUrl, "Pthigh.csv");
		copyTemplate(baseUrl, "Dtlow.csv");
		copyTemplate(baseUrl, "Dthigh.csv");
		copyTemplate(baseUrl, "EnvironmentalScore.csv");
		copyTemplate(baseUrl, "EconomicalScore.csv");
		copyTemplate(baseUrl, "Maturity.csv");
		
		try {
			runGAMS(baseUrl);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			logger.error(e.getMessage());
			
			
		}
	}
}
