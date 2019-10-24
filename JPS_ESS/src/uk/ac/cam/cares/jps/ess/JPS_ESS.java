package uk.ac.cam.cares.jps.ess;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

import uk.ac.cam.cares.jps.base.util.CommandHelper;


@WebServlet(urlPatterns = { "/ESSAgent" })

public class JPS_ESS extends JPSHttpServlet {

	private static final long serialVersionUID = -4199209974912271432L;
	private Logger logger = LoggerFactory.getLogger(JPS_ESS.class);
	//public static final String AGENT_TAG = "GAMS_NuclearAgent";

	public static void runGAMS() throws IOException, InterruptedException {
		System.out.println("Start");
		System.out.println("separator= " + File.separator);
		String executablelocation = "C:/GAMS/win64/27.3/gams.exe";
		 String folderlocation ="D:/Users/LONG01/Documents/gamsdir/projdir/";
//		String folderlocation = "C:/Users/GKAR01/Documents/gamsdir/projdir/";
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

		groupcommand.add("D:/Users/LONG01/Documents/gamsdir/projdir/gamsexecute.bat");

		// CommandHelper.executeSingleCommand(folderlocation,startbatCommand);
		CommandHelper.executeCommands(folderlocation, groupcommand);
		System.out.println("Done");
	}


	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jofornuc = AgentCaller.readJsonParameter(request);
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

		File file = new File("D:/Users/LONG01/Documents/gamsdir/projdir/Pa_high.csv");
		FileWriter writer = new FileWriter(file);
		writer.write(text);
		writer.close();
//        csvWriter.flush();
//        csvWriter.close();
		try {
			runGAMS();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
