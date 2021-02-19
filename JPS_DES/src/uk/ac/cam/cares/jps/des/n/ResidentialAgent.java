package uk.ac.cam.cares.jps.des.n;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.ResultSet;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@SuppressWarnings("serial")
@WebServlet(urlPatterns = {"/ResidentialAgent"})
public class ResidentialAgent extends JPSAgent {
	public static String bcap="bcap.csv";
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String unwill="unwill.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
	
	/** returns noOfHouseHoulds x  (hourly power consumption profile of all appliances for a given household 
	 * +hourly charging(+)/discharging(-) profile of all batteries for a given household
	 * +hourly total power consumption profile for a given household)
	 * 9 rows for now
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request)  {
		String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
		
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		JSONObject responseParams = new JSONObject();
		try {
			String res =  new DESAgentNew().runPythonScript("residential.py", baseUrl);

			responseParams.put("results", res);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
    	
		return responseParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        } 
        try {
        String iriofdistrict = requestParams.getString("district");
        return InputValidator.checkIfValidIRI(iriofdistrict); 
        }catch (JSONException ex) {
        	ex.printStackTrace();
        	throw new JSONException("Sensor not present in getString");
        }
    }
	/** general function for extracting Residential Data
	 * 
	 * @param iriofnetworkdistrict
	 * @param baseUrl
	 */
	public void extractResidentialData(String iriofnetworkdistrict, String baseUrl) {
		OntModel model = DESAgentNew.readModelGreedy(iriofnetworkdistrict);
		String groupInfo ="";
		try {
			SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
					.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#")
					.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
					.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
					.addVar("?entity").addVar("?propval").addVar("?user").addWhere("?entity" ,"a", "j6:Building")
					.addWhere("?entity" ,"j2:hasProperty", "?prop").addWhere("?prop" ,"j2:hasValue", "?vProp")
					.addWhere("?vProp" ,"j2:numericalValue", "?propval").addWhere("?entity" ,"j4:isComprisedOf", "?user")
					.addOrderBy("?user").addFilter("regex(STR(?user),\"001\")");
				Query q = sb.build();
				groupInfo = q.toString();
		} catch (ParseException e1) {
			// parseExpression due to REGEX used in Filter
			e1.printStackTrace();
		}
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
	/** read each User for PminPmax and Unwill schedule for each appliance
	 * 
	 * @param iriOfTypeUser
	 * @return
	 */
	protected List<String[]> readUserforPminPmaxUnwill( String iriOfTypeUser) {
		//per equipment, per user, extract high, low and actual value
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addVar("?entity").addVar("?Pmaxval").addVar("?Pminval").addVar("?unwillval")
				.addWhere("?entity" ,"a", "j6:Electronics").addWhere("?entity" ,"j9:hasActivePowerAbsorbed", "?Pmax")
				.addWhere("?Pmax" ,"a", "j9:MaximumActivePower").addWhere("?Pmax" ,"j2:hasValue", "?vPmax")
				.addWhere("?vPmax" ,"j2:numericalValue", "?Pmaxval")
				
				.addWhere("?entity" ,"j2:hasProperty", "?prop").addWhere("?prop" ,"a", "j6:IdealityFactor")
				.addWhere("?prop" ,"j2:hasValue", "?vProp").addWhere("?vProp" ,"j2:numericalValue", "?unwillval")
				
				.addWhere("?entity" ,"j9:hasActivePowerAbsorbed", "?Pmin").addWhere("?Pmin" ,"a", "j9:MinimumActivePower")
				.addWhere("?Pmin" ,"j2:hasValue", "?vPmin").addWhere("?vPmin" ,"j2:numericalValue", "?Pminval")
				.addOrderBy("?entity");
			Query q = sb.build();
			String equipmentinfo = q.toString();
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
	/** read each User's appliance schedule and return as String[]
	 * 
	 * @param iriOfTypeUser
	 * @return
	 */
	protected String[] readUserforAppSch( String iriOfTypeUser) {
		
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j7", "http://www.w3.org/2006/time#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addVar("?entity").addVar("?Pactval").addVar("?hourval")
				.addWhere("?entity" ,"a", "j6:Electronics").addWhere("?entity" ,"j9:hasActivePowerAbsorbed", "?Pact")
				.addWhere("?Pact" ,"a", "j9:AbsorbedActivePower").addWhere("?Pact" ,"j2:hasValue", "?vPact")
				.addWhere("?vPact" ,"j2:numericalValue", "?Pactval")
				.addWhere("?vPact" ,"j7:hasTime", "?proptime").addWhere("?proptime" ,"j7:hour", "?hourval")
				.addOrderBy("?hourval").addOrderBy("?entity");
		Query q = sb.build();
		String equipmentinfo = q.toString();
		OntModel model2 = DESAgentNew.readModelGreedyForUser(iriOfTypeUser);
		ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
		String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
		String[] keysx = JenaResultSetFormatter.getKeys(resultx);
		List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);

		//grab the current time
		Date date = new Date();   // given date
		Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
		calendar.setTime(date);   // assigns calendar to given date 
		int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
		Collections.rotate(resultListx,(24-h)*11); //rotate by number of hours
		String[] groupschedule = new String[resultListx.size()];
		for(int d=0;d<resultListx.size();d++) {
			groupschedule[d] = resultListx.get(d)[1];
		}
		return groupschedule;
	}
	
	
}
