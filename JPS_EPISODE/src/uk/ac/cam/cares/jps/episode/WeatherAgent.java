package uk.ac.cam.cares.jps.episode;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns ="/SensorWeatherAgent")
public class WeatherAgent extends JPSHttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	static String rdf4jServer = "http://localhost:8080/rdf4j-server"; //this is only the local repo, changed if it's inside claudius
	static String repositoryID = "weatherstation";
	public static Repository repo = new HTTPRepository(rdf4jServer, repositoryID);
	static String fileprefix="C:/Users/KADIT01/TOMCAT/webapps/ROOT/kb/sgp/singapore/";
	static String iriprefix="http://www.theworldavatar.com/kb/sgp/singapore/";
	public static final String weatherRequest = "http://api.openweathermap.org/data/2.5/weather?units=metric&q=%s&appid=329f65c3f7166977f6751cff95bfcb0a";

	
	//which data should be taken from?
	//should use top node or just context is enough?

	 protected void setLogger() {
	        logger = LoggerFactory.getLogger(WeatherAgent.class);
	    }
	    Logger logger = LoggerFactory.getLogger(WeatherAgent.class);
	    
	    protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			
			return requestParams;
			
		}
	    
		public static String provideCurrentTime() {			//timing format should be year, month, date, hour, minute,second

			DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
			   LocalDateTime now = LocalDateTime.now();
			   String com=dtf.format(now);
			   String date=com.split("/")[2].split(" ")[0];
			   
			   String year=com.split("/")[0];
				String monthdate=com.split("/")[1]+"-"+date;
				String time=com.split("/")[2].split(" ")[1];
				String completeformat=year+"-"+monthdate+"T"+time+"+08:00";
				return completeformat;
		}
		
	
	public void addFiletoRepo(RepositoryConnection con,String filename,String contextiri) {
		File file =new File(fileprefix+filename);
		String baseURI=iriprefix+filename;
		try {
			
			try {
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
		}
		catch (java.io.IOException e) {
			System.out.println("fail 2");
		}
	}
	
	public void queryValueLatestfromRepo(RepositoryConnection con, String context) { //should we use top node concept or the name graph to categorize some triples??
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "SELECT ?entity ?class ?propval ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?entity a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 10";
		
		TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
		TupleQueryResult result = query.evaluate();
		int d=0;
		try {
			while (result.hasNext()) {
				BindingSet bindingSet = result.next();
				String time = bindingSet.getValue("proptimeval").stringValue();
				String inst = bindingSet.getValue("entity").stringValue();
				String propclass = bindingSet.getValue("class").stringValue();
				String value = bindingSet.getValue("propval").stringValue();

				// String time="random";
				System.out.println("measured property= " + propclass);
				System.out.println("measured property value= " + value);
				System.out.println("instance sensor= "+inst);
				System.out.println(" at the time= " + time);
				// logger.info("species-uri: " + speciesUri);
				d++;
			}
			System.out.println("total data=" + d);
		} catch (Exception e) {

			System.out.println(e.getMessage());
		}
//		con.commit();
//		con.close();
	}
	
	public List<String[]> provideDataRepoRoutine(RepositoryConnection con, String context,String propnameclass) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "SELECT ?vprop ?propval ?proptime ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				//+ " ?entity a j5:T-Sensor ." 
				+ "  ?entity j4:observes ?prop ."
				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)";
		
		TupleQuery query = con.prepareTupleQuery(QueryLanguage.SPARQL, sensorinfo);
		TupleQueryResult result = query.evaluate();
		int d=0;
		List<String[]> keyvaluemapold= new ArrayList<String[]>();
		
		try {
			while (result.hasNext()) {
				BindingSet bindingSet = result.next();
				String timevalue = bindingSet.getValue("proptimeval").stringValue();
				String timeinstance = bindingSet.getValue("proptime").stringValue();
				String propvalue = bindingSet.getValue("propval").stringValue();
				String propinstance = bindingSet.getValue("vprop").stringValue();
				String[]keyelement= {propinstance,propvalue,timeinstance,timevalue};
				keyvaluemapold.add(keyelement);
				d++;
			}
		} catch (Exception e) {

			System.out.println(e.getMessage());
		}
		
		return keyvaluemapold;
		
		
	}
	
    private static String getWeatherDataFromGovAPI(String path, String json) {
        URIBuilder builder = new URIBuilder().setScheme("https").setHost("api.data.gov.sg")
                .setPath(path);
        builder.setParameter("query", json);

        try {
            HttpGet request = new HttpGet(builder.build());
            request.setHeader("Accept", "application/json");
            request.setHeader("Content-type", "application/json");

            return AgentCaller.executeGet(request);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }
    
    private static String getWeatherDataFromAccuweatherAPI(String cityName) throws URISyntaxException {
    	String requestURL = String.format(weatherRequest, cityName);
    	URI obj = new URI(requestURL);

        try {
            HttpGet request = new HttpGet(obj);
            request.setHeader("Accept", "application/json");
            request.setHeader("Content-type", "application/json");

            return AgentCaller.executeGet(request);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }
    
    public JSONObject extractedSingleDataFromAccuweather(String property, String cityName) throws URISyntaxException {
    	String result= getWeatherDataFromAccuweatherAPI(cityName);
    	JSONObject joPr= new JSONObject(result);
    	
    	String pressure=joPr.getJSONObject("main").get("pressure").toString(); //in hPa
    	String cloudcover=joPr.getJSONObject("clouds").get("all").toString(); //in %
    	String long1= joPr.getJSONObject("coord").get("lon").toString();
    	String lat1= joPr.getJSONObject("coord").get("lat").toString();
    	String time = joPr.get("dt").toString();
    	System.out.println("time= "+time);

    	//TODO: the time conversion still wrong
    	/*
    	  ZonedDateTime dateTime = Instant.ofEpochMilli(Long.parseLong(time))
    	            .atZone(ZoneId.of("UTC+08:00"));
    	  String timestamp = dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
 */
    	//temporarily just use current time
    	String timestamp=provideCurrentTime();
    	JSONObject returnresult= new JSONObject();
    	returnresult.put("long",long1);
    	returnresult.put("lat",lat1);
    	returnresult.put("timestamp", timestamp);
    	if(property.contains("cloud")) {
    		returnresult.put("value",cloudcover);
    	}else if(property.contains("pressure")) {
    		returnresult.put("value",pressure);
    	}
    	return returnresult;
    }
    
    public JSONObject provideJSONDataFromAPI() throws URISyntaxException {
    	
    	JSONObject jo1 = extractedSingleDataFromGov("/v1/environment/rainfall","clementi");//in mm
    	JSONObject jo2 = extractedSingleDataFromGov("/v1/environment/air-temperature","clementi");//in celcius
    	JSONObject jo3 = extractedSingleDataFromGov("/v1/environment/relative-humidity","clementi"); //in percent
    	JSONObject jo4 = extractedSingleDataFromGov("/v1/environment/wind-speed","clementi"); //knots
    	JSONObject jo5 = extractedSingleDataFromGov("/v1/environment/wind-direction","clementi"); //degree
    	JSONObject jo6 = extractedSingleDataFromGov("/v1/environment/wind-speed","ubin"); //knots
    	JSONObject jo7 = extractedSingleDataFromGov("/v1/environment/wind-direction","ubin"); //degree
    	JSONObject jo8=extractedSingleDataFromAccuweather("cloud", "singapore");//in percent
    	JSONObject jo9=extractedSingleDataFromAccuweather("pressure", "singapore");//in hPa exactly the same as millibar
    	//missing solar irradiation
    	
    	JSONObject finaljo= new JSONObject();
    	finaljo.put("precipitation", jo1);
    	finaljo.put("temperature", jo2);
    	finaljo.put("relativehumidity", jo3);
    	finaljo.put("windspeed1", jo4);
    	finaljo.put("winddirection1", jo5);
    	finaljo.put("windspeed2", jo6);
    	finaljo.put("winddirection2", jo7);
    	finaljo.put("cloudcover", jo8);
    	finaljo.put("pressure", jo9);
    	return finaljo;
    }

	private JSONObject extractedSingleDataFromGov(String path, String stnname) {
		String precipitation = getWeatherDataFromGovAPI(path, null);
		JSONObject joPr = new JSONObject(precipitation);

		// input stn name:
		// output sequence index
		JSONArray stn = joPr.getJSONObject("metadata").getJSONArray("stations");
		int size = stn.length();
		String stnid;
		int index = -1;
		for (int r = 0; r < size; r++) {
			String name = stn.getJSONObject(r).get("name").toString();
			if (name.toLowerCase().contains(stnname)) {
				stnid = stn.getJSONObject(r).get("id").toString();
				index = r;
			}
		}

		// index 7 for the clementi road
		String lat1 = joPr.getJSONObject("metadata").getJSONArray("stations").getJSONObject(index)
				.getJSONObject("location").get("latitude").toString();
		String long1 = joPr.getJSONObject("metadata").getJSONArray("stations").getJSONObject(index)
				.getJSONObject("location").get("longitude").toString();
		String timestamp = joPr.getJSONArray("items").getJSONObject(0).get("timestamp").toString();
		String propertyValue = joPr.getJSONArray("items").getJSONObject(0).getJSONArray("readings").getJSONObject(index)
				.get("value").toString(); // in mm
		JSONObject jo1 = new JSONObject();
		jo1.put("long", long1);
		jo1.put("lat", lat1);
		jo1.put("timestamp", timestamp);
		jo1.put("value", propertyValue);
		return jo1;
	}
    
	public void removeDataRepoRoutine(RepositoryConnection con,List<String[]>oldcontent) {
				
			int d=oldcontent.size();
			ValueFactory f=repo.getValueFactory();
			IRI numval=f.createIRI("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
			IRI timeval=f.createIRI("http://www.w3.org/2006/time#inXSDDateTimeStamp");
			for(int x=0; x<d;x++) {
				IRI prop1=f.createIRI(oldcontent.get(x)[0]);
				Literal lit1=f.createLiteral(new Double(oldcontent.get(x)[1]));
				con.remove(prop1,numval,lit1); //remove all triples realted to propval
				System.out.println(prop1+ " is removed");
				IRI prop2=f.createIRI(oldcontent.get(x)[2]);
				Literal lit2=f.createLiteral(oldcontent.get(x)[3]);
				con.remove(prop2,timeval,lit2); //remove all triples realted to timeval	
			}
	}
	
	public void insertDataRepo(RepositoryConnection con, List<String[]>oldcontent,String newpropvalue, String newtimestamp,String context) {
		List<String[]> valuemapold= new ArrayList<String[]>();
		ValueFactory f=repo.getValueFactory();
		for(int r=0;r<oldcontent.size();r++) {
			String[]content= {oldcontent.get(r)[1],oldcontent.get(r)[3]};
			valuemapold.add(content);
		}
		valuemapold.remove(0);
		String []newcontent= {newpropvalue,newtimestamp};
		valuemapold.add(newcontent);
		System.out.println("size of data= "+valuemapold.size());
		IRI numval=f.createIRI("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		IRI timeval=f.createIRI("http://www.w3.org/2006/time#inXSDDateTimeStamp");
		IRI contextiri= f.createIRI(context);
		for(int x=0; x<oldcontent.size();x++) {
			IRI prop1=f.createIRI(oldcontent.get(x)[0]);
			IRI prop2=f.createIRI(oldcontent.get(x)[2]);
			con.add(prop1, numval, f.createLiteral(new Double(valuemapold.get(x)[0])), contextiri);
			con.add(prop2,timeval,f.createLiteral(valuemapold.get(x)[1]),contextiri);
			
		}
	}
	
	public void resetRepoTrial(RepositoryConnection con,String context) {

		String[] filenames= {"SGCloudCoverSensor-001.owl","SGTemperatureSensor-001.owl","SGWindSpeedSensor-001.owl","SGSolarIrradiationSensor-001.owl","SGPrecipitationSensor-001.owl","SGPressureSensor-001.owl","SGRelativeHumiditySensor-001.owl","SGWindDirectionSensor-001.owl"};
		String[] filenames2= {"SGWindSpeedSensor-002.owl","SGWindDirectionSensor-002.owl"};
		for(String el:filenames) {
			new WeatherAgent().addFiletoRepo(con,el,context);
			
		}
	}
	
	public void updateRepo(RepositoryConnection con,String context,String propnameclass, String newpropvalue, String newtimestamp) {
		List<String[]>currentdatarepo= provideDataRepoRoutine(con,context,propnameclass);
		removeDataRepoRoutine(con,currentdatarepo);
		insertDataRepo(con, currentdatarepo,newpropvalue, newtimestamp,context);
		System.out.println("update for "+propnameclass+" in context "+context+" is done");
	}
	
	public void executeFunctionPeriodically(RepositoryConnection con,String context,String context2) throws URISyntaxException {
		String completeformat=WeatherAgent.provideCurrentTime();
		JSONObject result=new WeatherAgent().provideJSONDataFromAPI();	
		Double convertedspeed1=Double.valueOf(result.getJSONObject("windspeed1").get("value").toString())*0.514444;
		Double convertedspeed2=Double.valueOf(result.getJSONObject("windspeed2").get("value").toString())*0.514444;
		Double convertedcloud=Double.valueOf(result.getJSONObject("cloudcover").get("value").toString())/100;
		Double convertedRH=Double.valueOf(result.getJSONObject("relativehumidity").get("value").toString())/100;
		new WeatherAgent().updateRepo(con,context,"OutsideAirTemperature",result.getJSONObject("temperature").get("value").toString(),completeformat);//stored in Celcius
		new WeatherAgent().updateRepo(con,context,"OutsideWindSpeed",""+convertedspeed1,completeformat); //stored in m/s instead of knot
		new WeatherAgent().updateRepo(con,context,"OutsideWindDirection",result.getJSONObject("winddirection1").get("value").toString(),completeformat);//stored in degree
		new WeatherAgent().updateRepo(con,context,"OutsideAirCloudCover",""+convertedcloud,completeformat);//stored in decimal instead of %
		new WeatherAgent().updateRepo(con,context,"OutsideAirPressure",result.getJSONObject("pressure").get("value").toString(),completeformat);//stored in mBar, no need conversion from hPa
		new WeatherAgent().updateRepo(con,context,"OutsideAirPrecipitation",result.getJSONObject("precipitation").get("value").toString(),completeformat);// stored in mm
		new WeatherAgent().updateRepo(con,context,"OutsideAirRelativeHumidity",""+convertedRH,completeformat);//stored in decimal instead of %
		//new WeatherAgent().updateRepo(con,context,"OutsideAirProperties","25.4",completeformat); //it's for solar irradiation
		new WeatherAgent().updateRepo(con,context2,"OutsideWindSpeed",""+convertedspeed2,completeformat);//stored in m/s instead of knot
		new WeatherAgent().updateRepo(con,context2,"OutsideWindDirection",result.getJSONObject("winddirection2").get("value").toString(),completeformat); //stored in degree
		
	}
	

	
	public static void main(String[]args) {

		RepositoryConnection con = repo.getConnection();
		String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-001.owl#WeatherStation-001";
		String context2="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002";
//		new WeatherAgent().resetRepoTrial(con,context);
//		new WeatherAgent().queryValuefromRepo(con,context); only for query testing
		String completeformat=WeatherAgent.provideCurrentTime();

			System.out.println("currenttime= "+ completeformat);

			JSONObject apidata = null;
			try {
				apidata = new WeatherAgent().extractedSingleDataFromAccuweather("pressure", "singapore");
			} catch (URISyntaxException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			System.out.println("result from singapore= "+apidata.toString());
			

//		new WeatherAgent().addinstancetoRepo(con);
		//new WeatherAgent().deleteValuetoRepo(con);
		
	}
	
}
