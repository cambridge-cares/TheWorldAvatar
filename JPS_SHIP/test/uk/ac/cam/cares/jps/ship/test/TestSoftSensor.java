package uk.ac.cam.cares.jps.ship.test;

import java.util.ArrayList;
import java.util.List;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.ship.SoftSensor;

public class TestSoftSensor extends TestCase {
	
	public void testCallingSoftsensor() {
		JSONArray ja = new JSONArray();
		JSONObject location1 = new JSONObject();
		location1.put("x",833776.38);
		location1.put("y",816731.54);
		location1.put("z",4.5);
		ja.put(location1);
		JSONObject location2 = new JSONObject();
		location2.put("x",833776.38);
		location2.put("y",816731.54);
		location2.put("z",23.2);
		ja.put(location2);
		JSONObject location3 = new JSONObject();
		location3.put("x",124);
		location3.put("y",33.4);
		location3.put("z",23.2);
		ja.put(location3);
		
		JSONObject time = new JSONObject();
		time.put("from", "2020-01-14T01:00:00");
		time.put("to", "2020-01-14T04:00:00");
		JSONObject jo = new JSONObject();
		jo.put("timeinterval", time);
		jo.put("coordinates", ja);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SoftSensor",jo.toString());
		System.out.println("result= "+result);
		System.out.println("simplified result= "+JenaResultSetFormatter.convertToSimplifiedList(result));
		int number=JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results").length();
		assertEquals(81, number); //2time x 3point x 9pollutant  
		
	}
	
	public void testCallingSoftsensorwithPMmodif() {
		SoftSensor a= new SoftSensor();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/test.levels.csv");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
//		double x=822082.44;
//		double y=809548.75;
//		double z=0.0;
		double x=823682.44;
		double y=809548.75;
		double z=0.0;
		String timeinst="just general timestamp";
		
		//all concentrations in a specific location and time stamp
		List<String>concentration=a.findtheconcentration(simulationResult,x,y,z);
		
		//start creating the csv header before converted to json
		List<String[]>propercsv=new ArrayList<String[]>();
		String[]header= {"time","x","y","z","crs","pollutant","observes","value","unit"};
		propercsv.add(header);
		
		
		double sumpm10=0;
		double sumpm25=0;

		for (int r = 0; r < concentration.size(); r += 2) {
			String content[] = new String[9];
			content[0] = timeinst;
			content[1] = "" + x;
			content[2] = "" + y;
			content[3] = "" + z;
			content[4] = "EPSG:2326";
			content[5] = concentration.get(r).split("\\|")[2]; // later need to be mapped to iri
			content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
			content[7] = concentration.get(r + 1);
			content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
			
			
			if(content[5].toLowerCase().contains("pm2.5")) {
				sumpm25=sumpm25+Double.valueOf(content[7]);	
			}
			else if(content[5].toLowerCase().contains("pm10")){
				sumpm10=sumpm10+Double.valueOf(content[7]);	
			} 
			else {
				propercsv.add(content);
			}	
		}
		String content[] = new String[9];
		String content2[] = new String[9];
		content[0] = timeinst;
		content[1] = "" + x;
		content[2] = "" + y;
		content[3] = "" + z;
		content[4] = "EPSG:2326";
		content[5] = "PM10"; // later need to be mapped to iri
		content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
		content[7] = String.valueOf(sumpm10+sumpm25);
		content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
		propercsv.add(content);
		content2[0] = timeinst;
		content2[1] = "" + x;
		content2[2] = "" + y;
		content2[3] = "" + z;
		content2[4] = "EPSG:2326";
		content2[5] = "PM2.5"; // later need to be mapped to iri
		content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
		content2[7] = ""+sumpm25;
		content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
		propercsv.add(content2);
		
		String arrayinstring = null;
		for(int n=0;n<propercsv.size();n++) {		
			arrayinstring=arrayinstring+a.convertArrayToStringMethod(propercsv.get(n))+"\n";
		}

		String[]headertype= {"xsd:dateTime","xsd:number","xsd:number","xsd:number","literal","literal","uri","xsd:number","uri"};
		System.out.println("result csv format= "+arrayinstring);
		
		JSONObject dataSet = new JSONObject(new JenaResultSetFormatter().createJSONfromCSV(propercsv,headertype));
		System.out.println(dataSet.toString());
//		assertEquals(585014.1000000001, Double.valueOf(content2[7])); //pm10 amount
//		assertEquals(585029.5000000001, Double.valueOf(content[7]));  //pm2.5 amount
		assertEquals(585013.5399999999, Double.valueOf(content2[7])); //pm10 amount
		assertEquals(585028.5399999999, Double.valueOf(content[7]));  //pm2.5 amount
		
	}

	public void testcalculationclosest() {
		SoftSensor a= new SoftSensor();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/test.levels.gst");
		
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		System.out.println("the closest number= "+a.findtheclosest(simulationResult,833776.38,816731.54,17.0));
		double realx=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(0));
		double realy=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(1));
		double realz=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(2));
		assertEquals(833782.44, realx, 0.01);
		assertEquals(816748.75, realy, 0.01);
		assertEquals(20.0, realz, 0.01);
	}
	
	public void testcalculationconcentration() {
		SoftSensor a= new SoftSensor();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/test.levels.gst");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		double realx=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(0));
		double realy=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(1));
		double realz=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(2));
		System.out.println("the concentration result= "+a.findtheconcentration(simulationResult,realx,realy,realz));
		assertEquals("CO2", a.findtheconcentration(simulationResult,realx,realy,realz).get(0).split("\\|")[2]);
		assertEquals(585168, Double.valueOf(a.findtheconcentration(simulationResult,realx,realy,realz).get(1)), 0.01);
		assertEquals("CO", a.findtheconcentration(simulationResult,realx,realy,realz).get(2).split("\\|")[2]);
		assertEquals(58.4218, Double.valueOf(a.findtheconcentration(simulationResult,realx,realy,realz).get(3)), 0.01);
		
	}
	public static synchronized ResultSet queryFromFusekiServer(String serviceURI, String query) {
		
		QueryExecution q = QueryExecutionFactory.sparqlService(serviceURI,query);
		ResultSet results = q.execSelect();	

		return results;
	}
	
	public void xxxtestquery() {
		String plantInfo = "PREFIX time:<https://www.w3.org/2006/time#>"
				+ "PREFIX dcterms:<http://purl.org/dc/terms/> "
				
				+ "SELECT ?s ?q "
				+ "WHERE "
				+ "{?s  time:hasTime  ?o  ." 
				+ "?o   times:inXSDDateTime ?q ."
				+ "FILTER (REGEX(str(?s), \"C://JPS_DATA/workingdir/JPS_SCENARIO\"))" 
				
				+ "}";
		
		
		ResultSet rs_plant = queryFromFusekiServer("http://www.theworldavatar.com:80/damecoolquestion/jpsmetadata/query",plantInfo); 
		int x=0;
		for (; rs_plant.hasNext();) {		
			int c=0;
			QuerySolution qs_p = rs_plant.nextSolution();

			Resource cpiri = qs_p.getResource("s");
			String value1 = cpiri.toString();
			Literal cap = qs_p.getLiteral("q"); // extract the name of the source
			String capacity = cap.getString();
	
		}
	}
	
	public void testCallingSoftsensorlocal() {
		JSONArray ja = new JSONArray();
		JSONObject location1 = new JSONObject();
		location1.put("x",833776.38);
		location1.put("y",816731.54);
		location1.put("z",4.5);
		ja.put(location1);
		JSONObject location2 = new JSONObject();
		location2.put("x",833776.38);
		location2.put("y",816731.54);
		location2.put("z",23.2);
//		ja.put(location2);
		JSONObject location3 = new JSONObject();
		location3.put("x",124);
		location3.put("y",33.4);
		location3.put("z",23.2);
//		ja.put(location3);
		
		JSONObject time = new JSONObject();
		time.put("from", "2019-12-10T01:00:00");
		time.put("to", "2019-12-11T01:00:00");
//		time.put("to", "2020-04-09T12:00:00");
		JSONObject jo = new JSONObject();
		jo.put("timeinterval", time);
		jo.put("coordinates", ja);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SoftSensor",jo.toString());
		//System.out.println("result= "+result);
		System.out.println("simplified result= "+JenaResultSetFormatter.convertToSimplifiedList(result));
		int number=JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results").length();
		int co2=0;
		for(int d=0;d<number;d++) {
			if(JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results").getJSONObject(d).get("pollutant").toString().contentEquals("CO2")){
				co2++;
			}
		}
		System.out.println("number of file data= "+co2);
		//assertEquals(81, number); //2time x 1point x 9pollutant  
		
	}
	
}
