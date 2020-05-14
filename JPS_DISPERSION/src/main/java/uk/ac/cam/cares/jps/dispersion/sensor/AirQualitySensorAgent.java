<<<<<<< HEAD:JPS_DISPERSION/src/main/java/uk/ac/cam/cares/jps/dispersion/sensor/AirQualitySensorAgent.java
package uk.ac.cam.cares.jps.dispersion.sensor;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = {"/AirQualitySensorAgent","/resetAirQualityRepository"})
public class AirQualitySensorAgent extends JPSHttpServlet {

	private static final long serialVersionUID = 1L;
	public static String rdf4jServer = "http://localhost:8080/rdf4j-server"; //this is only the local repo, changed if it's inside claudius
	public static String repositoryID = "airqualitystation";
	public static Repository repo = new HTTPRepository(rdf4jServer, repositoryID);
	public static final String dataseturl=KeyValueManager.get(IKeys.DATASET_AIRQUALITY_URL);
	protected void setLogger() {
		logger=LoggerFactory.getLogger(AirQualitySensorAgent.class);
	}
	Logger logger= LoggerFactory.getLogger(AirQualitySensorAgent.class);
	
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		
		JSONObject response= new JSONObject();
		String path = request.getServletPath();
		if(path.contains("/resetAirQualityRepository")) { //used for Both AQMesh and soft sensor use case
			rdf4jServer = "http://localhost/rdf4j-server"; //for claudius
	   		 repo = new HTTPRepository(rdf4jServer, repositoryID);
	   		RepositoryConnection con = repo.getConnection();
				
	   		String[]location= {"singapore"};
	   		String cityiri= "http://dbpedia.org/resource/Singapore";
				
			for (String el:location){
				resetRepoTrial(con,el); //currently the context is not used
			}	
//			String inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/sensor weather reference.json");
//			List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(inputRef);
			int numbersensor=1;
			for(int x=1;x<=numbersensor;x++) {
				String index="0"+x;
				if(x<10) {
					index="00"+x;
				}
				String context="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-"+index+".owl#AirQualityStation-"+index;
				String name="VirtualSensor-001";
//				String locationname=""; (for AQMESH)
//				String serialnumber="";
				List<String>info= new ArrayList<String>();
				info.add(cityiri);
				info.add(name);
				info.add("0"); //overallpsi
//				info.add(locationname); (for AQMESH)
//				info.add(serialNumber);
				
				insertDataRepoContext(info,context);
			
			}	
			response.put("status", "reset endpoint successful");
			
		}else { //used for AQmesh only
			String cityiri=requestParams.get("city").toString();
			//right now the input is not connected yet
			executePeriodicUpdate("singapore");
			List<String[]> contextlist=extractAvailableContext( cityiri);
			String context=contextlist.get(0)[0];
			response.put("airStationIRI", context);	
		}
	
		return response;
	}
	
	public List<String[]> extractAvailableContext(String cityiri) {
		 String querycontext = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT DISTINCT ?graph " 
					+ "{ graph ?graph " 
					+ "{ "
					+ "?graph j2:hasAddress <"+cityiri+"> ."
					//+ "?graph j2:enumerationValue <"+"AQMESH"+"> ."
					
					+ "}"; 
		 
		 List<String[]> listmap = queryEndPointDataset(querycontext); //it will give 30 data
		
		 return listmap;
	}
	
	public void resetRepoTrial(RepositoryConnection con, String location) {// unused for the servlet
		int stnnumber=1;
		String index="";
		String midfix="";
		if(location.contains("hague")) {
			stnnumber=1;
			index="NL";
			midfix="nld/thehague";
		}else if(location.contains("berlin")) {
			stnnumber=1;
			index="DE";
			midfix="deu/berlin";
		}else if(location.contains("kong")) {
			stnnumber=8;
			index="HK";
			midfix="hkg/hongkong";
		}else if(location.contains("singapore")) {
			stnnumber=14;
			index="SG";
			midfix="sgp/singapore";
		}
		for (int d = 1; d <= stnnumber; d++) {
			String number = "00" + d;
			if (d > 9&& d<=99) {
				number = "0" + d;
			}
			String[] filenames = { index+"CO2Sensor-" + number + ".owl",
					index+"COSensor-" + number + ".owl", index+"SO2Sensor-" + number + ".owl",
					index+"O3Sensor-" + number + ".owl", index+"NO2Sensor-" + number + ".owl",
					index+"NOSensor-" + number + ".owl", index+"NOxSensor-" + number + ".owl",
					index+"PM1Sensor-" + number + ".owl", index+"PM2.5Sensor-" + number + ".owl",
					index+"HCSensor-" + number + ".owl",index+"PM10Sensor-" + number + ".owl"};
			String context = "http://www.theworldavatar.com/kb/"+midfix+"/AirQualityStation-" + number
					+ ".owl#AirQualityStation-" + number;
			System.out.println("upload files for graph");
			for (String el : filenames) {
				new AirQualitySensorAgent().addFiletoRepo(con, el, context,midfix);

			}
		}
	}
	
	public void addFiletoRepo(RepositoryConnection con,String filename,String contextiri, String midfix) {
		String root=AgentLocator.getProperty("absdir.root");
		String fileprefix=root+"/kb/"+midfix+"/";
		String iriprefix="http://www.theworldavatar.com/kb/"+midfix+"/";
		File file =new File(fileprefix+filename);
		String baseURI=iriprefix+filename;
		try {
			
			try {
	    		rdf4jServer = "http://localhost/rdf4j-server"; //for claudius
	    		 repo = new HTTPRepository(rdf4jServer, repositoryID);
//				con.add(file, baseURI, RDFFormat.RDFXML);
				//BELOW IS FOR ADDING THE NAMED GRAPH/CONTEXT :
				ValueFactory f=repo.getValueFactory();
				IRI context= f.createIRI(contextiri);
				con.add(file, baseURI, RDFFormat.RDFXML,context);
				System.out.println("success");
			}
			finally {
				con.close();				
			}
			
		}
		catch(RDF4JException e) {
			System.out.println("fail 1");
			logger.error(e.getMessage());
		}
		catch (java.io.IOException e) {
			System.out.println("fail 2");
			logger.error(e.getMessage());
		}
	}
	
	public void insertDataRepoContext(List<String>info,String context) {

		String sparqlupdate2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "WITH <" + context + ">"
					//+ "DELETE { ?valueemission j2:numericalValue ?vemission .} "
					+ "INSERT {"
					+ "<" + context+ "> j2:hasAddress <"+info.get(0)+"> ." 
					+ "<" + context+ "> j2:enumerationValue \""+info.get(1)+"\" ." 
					+ "<" + context+ "> j4:hasOverallPSI \""+info.get(2)+"\"^^xsd:integer ." 
					//+ "<" + context+ "> j4:podSerialNumber \""+info.get(2)+"\" ."  for aqmesh
					//+ "<" + context+ "> j4:locationName \""+info.get(3)+"\" ." for aqmesh
					+ "} "
					+ "WHERE { " 
					+ "}";
			
		KnowledgeBaseClient.update(dataseturl, null, sparqlupdate2);	

	}
	
	private List<String[]> queryEndPointDataset(String querycontext) {
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}
	
	private String  getDataFromAPI() {
		
		String result="";
		
		return result;
	}
	
	public void executePeriodicUpdate(String cityname) {
		String result=getDataFromAPI();
		//processed the input to have suitable format
		
		
		
	}
	
	
	
	public void updateRepoNewMethod(String context,String propnameclass, String scaledvalue,String prescaledvalue, String newtimestamp) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?vprop ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ."
//				+ " ?proptime   j6:hasBeginning ?proptimestart ."
//				+ " ?proptime   j6:hasEnd ?proptimeend ."
//				+ " ?proptimestart   j6:inXSDDateTimeStamp ?proptimestartval ." 
//				+ " ?proptimeend   j6:inXSDDateTimeStamp ?proptimeendval ."
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)LIMIT1";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		
		String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "WITH <" + context + ">"
				+ "DELETE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue ?oldpropertydata ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue ?oldpropertydata2 ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatatime ."
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "} "
				+ "INSERT {"
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue \""+scaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue \""+prescaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp \""+newtimestamp+"\" ." 
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp \""+newtimestampend+"\" ." 
				+ "} "
				+ "WHERE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue ?oldpropertydata ."	
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue ?oldpropertydata2 ."	
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatatime ."
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "}";
		
			
			KnowledgeBaseClient.update(dataseturl, null, sparqlupdate); //update the dataset

		
		
	}
	
	public static void main(String[]args) { //used for upload all content locally

		RepositoryConnection con = repo.getConnection();
		String location="singapore";
//		String location="hong kong";
//		String location="berlin";
//		String location="the hague";
		AirQualitySensorAgent a=new AirQualitySensorAgent();
		a.resetRepoTrial(con,location); //currently the context is not used
		int numbersensor=1; //should change if added by AQMesh
		String cityiri= "http://dbpedia.org/resource/Singapore";
		for(int x=1;x<=numbersensor;x++) {
			String index="0"+x;
			if(x<10) {
				index="00"+x;
			}
			String context="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-"+index+".owl#AirQualityStation-"+index;
			String name="VirtualSensor-001";
//			String locationname=""; (for AQMESH)
//			String serialnumber="";
			List<String>info= new ArrayList<String>();
			info.add(cityiri);
			info.add(name);
			info.add("0"); //overallpsi
//			info.add(locationname); (for AQMESH)
//			info.add(serialNumber);
			
			a.insertDataRepoContext(info,context);
		
		}

			System.out.println("update is done");		

		
	}
	
}



=======
package uk.ac.cam.cares.jps.dispersion.sensor;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
@WebServlet(urlPatterns = {"/AirQualitySensorAgent","/resetAirQualityRepository"})
public class AirQualitySensorAgent extends JPSHttpServlet {

	private static final long serialVersionUID = 1L;
	public static String rdf4jServer = "http://localhost:8080/rdf4j-server"; //this is only the local repo, changed if it's inside claudius
	public static String repositoryID = "airqualitystation";
	public static Repository repo = new HTTPRepository(rdf4jServer, repositoryID);
	public static final String dataseturl=KeyValueManager.get(IKeys.DATASET_AIRQUALITY_URL);
	protected void setLogger() {
		logger=LoggerFactory.getLogger(AirQualitySensorAgent.class);
	}
	Logger logger= LoggerFactory.getLogger(AirQualitySensorAgent.class);
	
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		
		JSONObject response= new JSONObject();
		String path = request.getServletPath();
		if(path.contains("/resetAirQualityRepository")) { //used for Both AQMesh and soft sensor use case
			rdf4jServer = "http://localhost/rdf4j-server"; //for claudius
	   		 repo = new HTTPRepository(rdf4jServer, repositoryID);
	   		RepositoryConnection con = repo.getConnection();
				
	   		String[]location= {"singapore"};
	   		String cityiri= "http://dbpedia.org/resource/Singapore";
				
			for (String el:location){
				resetRepoTrial(con,el); //currently the context is not used
			}	
//			String inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/sensor weather reference.json");
//			List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(inputRef);
			int numbersensor=1;
			for(int x=1;x<=numbersensor;x++) {
				String index="0"+x;
				if(x<10) {
					index="00"+x;
				}
				String context="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-"+index+".owl#AirQualityStation-"+index;
				String name="VirtualSensor-001";
//				String locationname=""; (for AQMESH)
//				String serialnumber="";
				List<String>info= new ArrayList<String>();
				info.add(cityiri);
				info.add(name);
				info.add("0"); //overallpsi
//				info.add(locationname); (for AQMESH)
//				info.add(serialNumber);
				
				insertDataRepoContext(info,context);
			
			}	
			response.put("status", "reset endpoint successful");
			
		}else { //used for AQmesh only
			String cityiri=requestParams.get("city").toString();
			//right now the input is not connected yet
			executePeriodicUpdate("singapore");
			List<String[]> contextlist=extractAvailableContext( cityiri);
			String context=contextlist.get(0)[0];
			response.put("airStationIRI", context);	
		}
	
		return response;
	}
	
	public List<String[]> extractAvailableContext(String cityiri) {
		 String querycontext = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT DISTINCT ?graph " 
					+ "{ graph ?graph " 
					+ "{ "
					+ "?graph j2:hasAddress <"+cityiri+"> ."
					//+ "?graph j2:enumerationValue <"+"AQMESH"+"> ."
					
					+ "}"; 
		 
		 List<String[]> listmap = queryEndPointDataset(querycontext); //it will give 30 data
		
		 return listmap;
	}
	
	public void resetRepoTrial(RepositoryConnection con, String location) {// unused for the servlet
		int stnnumber=1;
		String index="";
		String midfix="";
		if(location.contains("hague")) {
			stnnumber=1;
			index="NL";
			midfix="nld/thehague";
		}else if(location.contains("berlin")) {
			stnnumber=1;
			index="DE";
			midfix="deu/berlin";
		}else if(location.contains("kong")) {
			stnnumber=8;
			index="HK";
			midfix="hkg/hongkong";
		}else if(location.contains("singapore")) {
			stnnumber=14;
			index="SG";
			midfix="sgp/singapore";
		}
		for (int d = 1; d <= stnnumber; d++) {
			String number = "00" + d;
			if (d > 9&& d<=99) {
				number = "0" + d;
			}
			String[] filenames = { index+"CO2Sensor-" + number + ".owl",
					index+"COSensor-" + number + ".owl", index+"SO2Sensor-" + number + ".owl",
					index+"O3Sensor-" + number + ".owl", index+"NO2Sensor-" + number + ".owl",
					index+"NOSensor-" + number + ".owl", index+"NOxSensor-" + number + ".owl",
					index+"PM1Sensor-" + number + ".owl", index+"PM2.5Sensor-" + number + ".owl",
					index+"HCSensor-" + number + ".owl",index+"PM10Sensor-" + number + ".owl"};
			String context = "http://www.theworldavatar.com/kb/"+midfix+"/AirQualityStation-" + number
					+ ".owl#AirQualityStation-" + number;
			System.out.println("upload files for graph");
			for (String el : filenames) {
				new AirQualitySensorAgent().addFiletoRepo(con, el, context,midfix);

			}
		}
	}
	
	public void addFiletoRepo(RepositoryConnection con,String filename,String contextiri, String midfix) {
		String root=AgentLocator.getProperty("absdir.root");
		String fileprefix=root+"/kb/"+midfix+"/";
		String iriprefix="http://www.theworldavatar.com/kb/"+midfix+"/";
		File file =new File(fileprefix+filename);
		String baseURI=iriprefix+filename;
		try {
			
			try {
	    		rdf4jServer = "http://localhost/rdf4j-server"; //for claudius
	    		 repo = new HTTPRepository(rdf4jServer, repositoryID);
//				con.add(file, baseURI, RDFFormat.RDFXML);
				//BELOW IS FOR ADDING THE NAMED GRAPH/CONTEXT :
				ValueFactory f=repo.getValueFactory();
				IRI context= f.createIRI(contextiri);
				con.add(file, baseURI, RDFFormat.RDFXML,context);
				System.out.println("success");
			}
			finally {
				con.close();				
			}
			
		}
		catch(RDF4JException e) {
			System.out.println("fail 1");
			logger.error(e.getMessage());
		}
		catch (java.io.IOException e) {
			System.out.println("fail 2");
			logger.error(e.getMessage());
		}
	}
	
	public void insertDataRepoContext(List<String>info,String context) {

		String sparqlupdate2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "WITH <" + context + ">"
					//+ "DELETE { ?valueemission j2:numericalValue ?vemission .} "
					+ "INSERT {"
					+ "<" + context+ "> j2:hasAddress <"+info.get(0)+"> ." 
					+ "<" + context+ "> j2:enumerationValue \""+info.get(1)+"\" ." 
					+ "<" + context+ "> j4:hasOverallPSI \""+info.get(2)+"\"^^xsd:integer ." 
					//+ "<" + context+ "> j4:podSerialNumber \""+info.get(2)+"\" ."  for aqmesh
					//+ "<" + context+ "> j4:locationName \""+info.get(3)+"\" ." for aqmesh
					+ "} "
					+ "WHERE { " 
					+ "}";
			
		KnowledgeBaseClient.update(dataseturl, null, sparqlupdate2);	

	}
	
	private List<String[]> queryEndPointDataset(String querycontext) {
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}
	
	private String  getDataFromAPI() {
		
		String result="";
		
		return result;
	}
	
	public void executePeriodicUpdate(String cityname) {
		String result=getDataFromAPI();
		//processed the input to have suitable format
		
		
		
	}
	
	
	
	public void updateRepoNewMethod(String context,String propnameclass, String scaledvalue,String prescaledvalue, String newtimestamp) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?vprop ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ."
//				+ " ?proptime   j6:hasBeginning ?proptimestart ."
//				+ " ?proptime   j6:hasEnd ?proptimeend ."
//				+ " ?proptimestart   j6:inXSDDateTimeStamp ?proptimestartval ." 
//				+ " ?proptimeend   j6:inXSDDateTimeStamp ?proptimeendval ."
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)LIMIT1";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		
		String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "WITH <" + context + ">"
				+ "DELETE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue ?oldpropertydata ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue ?oldpropertydata2 ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatatime ."
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "} "
				+ "INSERT {"
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue \""+scaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue \""+prescaledvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp \""+newtimestamp+"\" ." 
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp \""+newtimestampend+"\" ." 
				+ "} "
				+ "WHERE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:scaledNumValue ?oldpropertydata ."	
				+ "<" + keyvaluemapold.get(0)[0]+ "> j4:prescaledNumValue ?oldpropertydata2 ."	
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddatatime ."
				//+ "<" + keyvaluemapold.get(0)[2]+ "> j6:inXSDDateTimeStamp ?olddataend ."
				+ "}";
		
			
			KnowledgeBaseClient.update(dataseturl, null, sparqlupdate); //update the dataset

		
		
	}
	
	public static void main(String[]args) { //used for upload all content locally

		RepositoryConnection con = repo.getConnection();
		String location="singapore";
//		String location="hong kong";
//		String location="berlin";
//		String location="the hague";
		AirQualitySensorAgent a=new AirQualitySensorAgent();
		a.resetRepoTrial(con,location); //currently the context is not used
		int numbersensor=1; //should change if added by AQMesh
		String cityiri= "http://dbpedia.org/resource/Singapore";
		for(int x=1;x<=numbersensor;x++) {
			String index="0"+x;
			if(x<10) {
				index="00"+x;
			}
			String context="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStation-"+index+".owl#AirQualityStation-"+index;
			String name="VirtualSensor-001";
			String name2="AQMeshSensor-001";
			String context2="http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-"+index+".owl#AirQualityStationAQMesh-"+index;
//			String locationname=""; (for AQMESH)
//			String serialnumber="";
			List<String>info= new ArrayList<String>();
			info.add(cityiri);
			info.add(name);
			info.add("0"); //overallpsi
//			info.add(locationname); (for AQMESH)
//			info.add(serialNumber);
			
			a.insertDataRepoContext(info,context);
			//a.insertDataRepoContext(info,context2);
		
		}

			System.out.println("update is done");		

		
	}
	
}



>>>>>>> c4013b6c58790c38177c62add14e48cfeb449296:JPS_DISPERSION/src/uk/ac/cam/cares/jps/dispersion/sensor/AirQualitySensorAgent.java
