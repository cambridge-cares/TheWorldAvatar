package uk.ac.cam.cares.jps.des;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/ResidentialAgent"})
public class ResidentialAgent extends JPSHttpServlet {
	public static String bcap="bcap.csv";
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String unwill="unwill.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
	
	public void extractResidentialData(String iriofnetworkdistrict, String baseUrl) {
		OntModel model = DESAgentNew.readModelGreedy(iriofnetworkdistrict);
		//extracts 
		String groupInfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "SELECT ?entity ?propval ?user " 
				+ "WHERE {"
				+ "{ ?entity a j6:Building ." + "  ?entity j2:hasProperty ?prop ." 
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + "?entity j4:isComprisedOf ?user ." 
				+ "}"
				+ "FILTER regex(STR(?user),\"001\") ." + "}" + "GROUP BY ?entity ?propval ?user "
				+ "ORDER BY ASC(?user)";
		
		
		ResultSet resultSet = JenaHelper.query(model, groupInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		int size = resultList.size();
		List<String> iriofgroupuser = new ArrayList<String>();
		List<String[]> csvofbcap = new ArrayList<String[]>();
		QueryBroker broker = new QueryBroker();
		for (int d = 0; d < size; d++) {
			iriofgroupuser.add(resultList.get(d)[2]);
			String[] e = {resultList.get(d)[1] };
			csvofbcap.add(e);
		}
		String bcapcsv = MatrixConverter.fromArraytoCsv(csvofbcap);

		int sizeofiriuser = iriofgroupuser.size();
		List<String[]> csvofpmax = new ArrayList<String[]>();
		List<String[]> csvofpmin = new ArrayList<String[]>();
		List<String[]> csvofw = new ArrayList<String[]>();
		List<String[]> csvofschedule = new ArrayList<String[]>();
		
		//grab the current time
		Date date = new Date();   // given date
		Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
		calendar.setTime(date);   // assigns calendar to given date 
		int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
		
		//rotate it according to the current hour to get the appropriate profile

//		Collections.rotate(lst, h);
		//subfunction to read per iri of user (in case there are more than one user. 
		for(int x=0;x<sizeofiriuser;x++) {
			List<String[]> pmVal = readUserforPminPmaxUnwill( iriofgroupuser.get(x));
			csvofpmax.add(pmVal.get(0));
			csvofpmin.add(pmVal.get(1));
			csvofw.add(pmVal.get(2));
			csvofschedule.add(readUserforAppSch(iriofgroupuser.get(x)));
		}

		broker.putLocal(baseUrl + "/" + bcap, bcapcsv);
		String pmaxcsv = MatrixConverter.fromArraytoCsv(csvofpmax);
//		System.out.println(pmaxcsv);
		broker.putLocal(baseUrl + "/" + Pmax, pmaxcsv);

		String pmincsv = MatrixConverter.fromArraytoCsv(csvofpmin);
//		System.out.println(pmincsv);
		broker.putLocal(baseUrl + "/" + Pmin, pmincsv);

		String wcsv = MatrixConverter.fromArraytoCsv(csvofw);
//		System.out.println(wcsv);
		broker.putLocal(baseUrl + "/" + unwill, wcsv);

		String schedulecsv = MatrixConverter.fromArraytoCsv(csvofschedule);
//		System.out.println(schedulecsv);
		broker.putLocal(baseUrl + "/" + schedule, schedulecsv);
		
	}
	protected List<String[]> readUserforPminPmaxUnwill( String iriOfTypeUser) {
		//per equipment, per user, extract high, low and actual value 
		String equipmentinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "SELECT ?entity ?Pmaxval ?Pminval ?unwillval " + "WHERE "
				+ "{ ?entity a j6:Electronics ." + "?entity j9:hasActivePowerAbsorbed ?Pmax ."
				+ "?Pmax a j9:MaximumActivePower ." + " ?Pmax   j2:hasValue ?vPmax ."
				+ " ?vPmax   j2:numericalValue ?Pmaxval ."

				+ "  ?entity j2:hasProperty ?prop ." + "?prop a j6:IdealityFactor ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?unwillval ."

				+ "?entity j9:hasActivePowerAbsorbed ?Pmin ." + "?Pmin a j9:MinimumActivePower ."
				+ " ?Pmin   j2:hasValue ?vPmin ." + " ?vPmin   j2:numericalValue ?Pminval ."


				+ "}"+ "ORDER BY ASC(?entity)";
		OntModel model2 = DESAgentNew.readModelGreedyForUser(iriOfTypeUser);
		ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
		String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
		String[] keysx = JenaResultSetFormatter.getKeys(resultx);
		List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);
		
		
		String[] low1 = new String[resultListx.size()];
		String[] high1 = new String[resultListx.size()];
		String[] unwill1 = new String[resultListx.size()];

		for(int d=0;d<resultListx.size();d++) {
			high1[d] = resultListx.get(d)[1];
			low1[d] = resultListx.get(d)[2];
			unwill1[d] = resultListx.get(d)[3];
		}
//		System.out.println("LOW 1");
		return Arrays.asList(high1, low1, unwill1);
	}
	protected String[] readUserforAppSch( String iriOfTypeUser) {
		String equipmentinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j7:<http://www.w3.org/2006/time#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "SELECT ?entity ?Pactval ?hourval " + "WHERE "
				+ "{ ?entity a j6:Electronics ." 
				+ "?entity j9:hasActivePowerAbsorbed ?Pact ." 
				+ "?Pact a j9:AbsorbedActivePower ."
				+ " ?Pact   j2:hasValue ?vPact ." + " ?vPact   j2:numericalValue ?Pactval ."
				+ " ?vPact   j7:hasTime ?proptime ." + "?proptime j7:hour ?hourval ."
				 
				+ "}" + "ORDER BY ASC(?hourval) ASC(?entity)";
		OntModel model2 = DESAgentNew.readModelGreedyForUser(iriOfTypeUser);
		ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
		String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
		String[] keysx = JenaResultSetFormatter.getKeys(resultx);
		List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);
		String[] groupschedule = new String[resultListx.size()];
		for(int d=0;d<resultListx.size();d++) {
			groupschedule[d] = resultListx.get(d)[1];
		}
		return groupschedule;
	}
	public String runResidentialPy(String script, String folder) throws Exception {
		String result = "";
			String path = AgentLocator.getCurrentJpsAppDirectory(this);
			String command = "python " + path+ "/python/" +script + " " + folder;
//			System.out.println(command);
			result = CommandHelper.executeSingleCommand( path, command);
		
		
			return result;
	}
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request)  {
		String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
		
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		JSONObject responseParams = new JSONObject();
		try {
			String res = runResidentialPy("residential.py", baseUrl);
			//TODO: When other agents employ residential, results would be returned not just as a csv, but as another object
			
//			System.out.println(res);
			responseParams.put("results", res);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
    	
		return responseParams;
	}
}
