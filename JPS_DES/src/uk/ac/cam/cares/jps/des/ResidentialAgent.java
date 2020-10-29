package uk.ac.cam.cares.jps.des;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.des.DESAgentNew;

@SuppressWarnings("serial")
public class ResidentialAgent extends JPSHttpServlet {
	public static String bcap="bcap.csv";
	public static String Pmin="Pmin.csv";
	public static String Pmax="Pmax.csv";
	public static String unwill="unwill.csv";
	public static String schedule="ApplianceScheduleLoad1.csv";
	public void extractResidentialData(String iriofnetworkdistrict, String baseUrl) {
		OntModel model = DESAgentNew.readModelGreedy(iriofnetworkdistrict);
//		String j2Prefix = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
//		String j4Prefix = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#";
//		String j5Prefix = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#";
//		String j6Prefix = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#";
//		String entitySub = "?entity";
//		String a = "a";
//		try {
//			WhereBuilder aBuild = new WhereBuilder()
//					.addPrefix("j6", j6Prefix)
//					.addWhere(entitySub, a, "j6:Building")
//					.addWhere("?entity", "j4:isComprisedOf", "?user");
//			
//			SelectBuilder sb1 = new SelectBuilder()
//					.addPrefix("j2", j2Prefix)
//					.addPrefix("j4", j4Prefix)
//					.addVar("?entity (COUNT(?entity) AS ?group)")
//					.addVar("?propval")
//					.addVar("?user")
//					.addWhere(aBuild)
//					.addWhere(entitySub, "j2:hasProperty", "?prop")
//					.addWhere("?prop", "j2:hasValue", "?vprop")
//					.addFilter("regex(STR(?user),\"001\")")
//					.addGroupBy("?entity")
//					.addGroupBy("?propval")
//					.addGroupBy("?user");
//			
//			SelectBuilder sb2 = new SelectBuilder()
//					.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#")
//					.addVar("?entity (COUNT(?entity) AS ?group)")
//					.addWhere(aBuild)
//					.addGroupBy("?entity");
				
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
		System.out.println("sizeofresult=" + resultList.size());
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
		String[] timeschedu = {"t1","t2", "t3", "t4", "t5","t6","t7", "t8", "t9", "t10","t11","t12", "t13", "t14", "t15","t16","t17", "t18", "t19", "t20","t21","t22", "t23", "t24"};
		
		//grab the current time
		List<String> lst = Arrays.asList(timeschedu);
		Date date = new Date();   // given date
		Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
		calendar.setTime(date);   // assigns calendar to given date 
		int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
		
		//rotate it according to the current hour to get the appropriate profile
		
		//subfunction to read per iri of user (in case there are more than one user. 
		for(int x=0;x<sizeofiriuser;x++) {
			readUserforAppSche(lst, h, iriofgroupuser.get(x));
		}

		broker.putLocal(baseUrl + "/" + bcap, bcapcsv);
		// csvofpmax.add(0, arr0);
		String pmaxcsv = MatrixConverter.fromArraytoCsv(csvofpmax);
		System.out.println(pmaxcsv);
		broker.putLocal(baseUrl + "/" + Pmax, pmaxcsv);

		// csvofpmin.add(0, arr0);
		String pmincsv = MatrixConverter.fromArraytoCsv(csvofpmin);
		System.out.println(pmincsv);
		broker.putLocal(baseUrl + "/" + Pmin, pmincsv);

		// csvofw.add(0, arr0);
		String wcsv = MatrixConverter.fromArraytoCsv(csvofw);
		System.out.println(wcsv);
		broker.putLocal(baseUrl + "/" + unwill, wcsv);

		String schedulecsv = MatrixConverter.fromArraytoCsv(csvofschedule);
		System.out.println(schedulecsv);
		broker.putLocal(baseUrl + "/" + schedule, schedulecsv);
		
//		}	catch (org.apache.jena.sparql.lang.sparql_11.ParseException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		}
	}
	protected void readUserforAppSche(List<String> lst, int h, String iriOfTypeUser) {
		//per equipment, per user, extract high, low and actual value 
		Collections.rotate(lst, -h);
		String equipmentinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j7:<http://www.w3.org/2006/time#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "SELECT ?entity ?Pmaxval ?Pminval ?unwillval ?Pactval ?hourval " + "WHERE "
				+ "{ ?entity a j6:Electronics ." + "?entity j9:hasActivePowerAbsorbed ?Pmax ."
				+ "?Pmax a j9:MaximumActivePower ." + " ?Pmax   j2:hasValue ?vPmax ."
				+ " ?vPmax   j2:numericalValue ?Pmaxval ."

				+ "  ?entity j2:hasProperty ?prop ." + "?prop a j6:IdealityFactor ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?unwillval ."

				+ "?entity j9:hasActivePowerAbsorbed ?Pmin ." + "?Pmin a j9:MinimumActivePower ."
				+ " ?Pmin   j2:hasValue ?vPmin ." + " ?vPmin   j2:numericalValue ?Pminval ."

				+ "?entity j9:hasActivePowerAbsorbed ?Pact ." + "?Pact a j9:AbsorbedActivePower ."
				+ " ?Pact   j2:hasValue ?vPact ." + " ?vPact   j2:numericalValue ?Pactval ."
				+ " ?vPact   j7:hasTime ?proptime ." + "?proptime j7:hour ?hourval ."

				+ "}" + "ORDER BY ASC(?hourval)";
		List<String[]> subList =  new ArrayList<String[]>();
		OntModel model2 = DESAgentNew.readModelGreedyForUser(iriOfTypeUser);
		ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
		String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
		String[] keysx = JenaResultSetFormatter.getKeys(resultx);
		List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);
		System.out.println("sizeofresult="+resultListx.size()); //264 rows, appliance then by hour
		

		List<String>groupPmax=new ArrayList<String>();
		groupPmax.add(iriOfTypeUser);
		List<String>groupPmin=new ArrayList<String>();
		groupPmin.add(iriOfTypeUser);
		List<String>groupw=new ArrayList<String>();
		groupw.add(iriOfTypeUser);
		List<String>groupschedule=new ArrayList<String>();
		groupschedule.add(iriOfTypeUser);
		//Set to ensure no repeats
		int countr = 1; 
		groupschedule.add(lst.get(0));
		for(int d=0;d<resultListx.size();d++) {
				if(resultListx.get(d)[5].contentEquals("1")) {
					//System.out.println("equipment= "+resultListx.get(d)[0]);
					groupPmax.add(resultListx.get(d)[1]);
					groupPmin.add(resultListx.get(d)[2]);
					groupw.add(resultListx.get(d)[3]);
				}
				//HashMap
				countr ++; 
				if (countr < 12) { //11 appliances
					groupschedule.add(resultListx.get(d)[4]);
				} else {
					groupschedule.add(resultListx.get(d)[4]);
					String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
					subList.add(arr4);
					//clear groupschedule
					groupschedule=new ArrayList<String>();
					countr = 1;
					if (Integer.parseInt(resultListx.get(d)[5]) < 24) {
						groupschedule.add(iriOfTypeUser);
//						groupschedule.add("t"+Integer.toString(Integer.parseInt(resultListx.get(d)[5])+1));
						groupschedule.add(lst.get(Integer.parseInt(resultListx.get(d)[5])));
					}
				}				
		}

		List<String[]> csvofpmax = new ArrayList<String[]>();
		List<String[]> csvofpmin = new ArrayList<String[]>();
		List<String[]> csvofw = new ArrayList<String[]>();
		List<String[]> csvofschedule = new ArrayList<String[]>();
		
		String[] arr1 = groupPmax.toArray(new String[groupPmax.size()]);
		csvofpmax.add(arr1);
		String[] arr2 = groupPmin.toArray(new String[groupPmin.size()]);
		csvofpmin.add(arr2);
		String[] arr3 = groupw.toArray(new String[groupw.size()]);
		csvofw.add(arr3);
		System.out.println(groupschedule.toArray().toString());
		String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
		subList.add(arr4);
		Collections.rotate(subList, h);
		csvofschedule.addAll(subList);
	}
	protected JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
		String iriofdistrict = requestParams.optString("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
		String baseUrl = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES"); //create unique uuid
        
		extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		JSONObject responseParams = requestParams;	
    	
		return responseParams;
	}
}
