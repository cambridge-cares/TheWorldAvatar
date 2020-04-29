package uk.ac.cam.cares.jps.dispersion.episode;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

@WebServlet(urlPatterns ="/SensorWeatherAgent")
public class WeatherAgent extends JPSHttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static String rdf4jServer = "http://localhost:8080/rdf4j-server"; //this is only the local repo, changed if it's inside claudius
	public static String repositoryID = "weatherstation";
	public static Repository repo = new HTTPRepository(rdf4jServer, repositoryID);

	public static final String weatherRequest = "http://api.openweathermap.org/data/2.5/weather?units=metric&q=%s&appid=329f65c3f7166977f6751cff95bfcb0a";

	 protected void setLogger() {
	        logger = LoggerFactory.getLogger(WeatherAgent.class);
	    }
	    Logger logger = LoggerFactory.getLogger(WeatherAgent.class);
	    
	    protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	    	String cityiri=requestParams.get("city").toString();
	    	JSONObject region = requestParams.getJSONObject("region");
			String sourceCRSName = region.optString("srsname"); //assuming from the front end of jpsship, it is in epsg 3857 for universal
		    if ((sourceCRSName == null) || sourceCRSName.isEmpty()) { //regarding the composition, it will need 4326, else, will be universal 3857 coordinate system
		    	sourceCRSName = CRSTransformer.EPSG_4326; 
		    }
			String lowx = region.getJSONObject("lowercorner")
					.get("lowerx").toString();
			String lowy = region.getJSONObject("lowercorner")
					.get("lowery").toString();
			String upx = region.getJSONObject("uppercorner")
					.get("upperx").toString();
			String upy = region.getJSONObject("uppercorner")
					.get("uppery").toString();
			double proclowx = Double.valueOf(lowx);
			double procupx = Double.valueOf(upx);
			double proclowy = Double.valueOf(lowy);
			double procupy = Double.valueOf(upy);
			double[] center = CalculationUtils.calculateCenterPoint(procupx, procupy, proclowx, proclowy);
			double[] centerPointConverted = CRSTransformer.transform(sourceCRSName,CRSTransformer.EPSG_4326,
					center);
				
		
		List<String[]> listmap = extractAvailableContext(cityiri,centerPointConverted[0],centerPointConverted[1]);
		 String context=listmap.get(0)[0]; //main stn
		 String context2=listmap.get(1)[0]; // the furthest station	 	
		 try {
			//new WeatherAgent().executeFunctionPeriodically(listmap,cityiri);
			 new WeatherAgent().executePeriodicUpdate(cityiri);
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    	JSONObject response= new JSONObject();
	    	JSONArray station= new JSONArray();
	    	station.put(context);
	    	station.put(context2);
	    	response.put("stationiri",station);
	    	
			return response;
			
		}
	    	
	    
	private Map<String,String> extractMappingname(){
		  String querycontext = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT DISTINCT ?graph ?stnname " 
				+ "{ graph ?graph  " 
				+ "{?graph j2:enumerationValue ?stnname .  " 
				+" } " 
				+" }";

		List<String[]> listmap = queryEndPointDataset(querycontext); //it will give 20
		Map<String, String> map = new HashMap<>();
		for(int x=0;x<listmap.size();x++) {
			map.put(listmap.get(x)[1],listmap.get(x)[0]);
		}
		return map;
	}

	public List<String[]> extractAvailableContext(String cityiri,double x0,double y0) {
		
		  String querycontext = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT DISTINCT ?graph ?stnname ?xval ?yval ?proptimeval ?class " 
				+ "{ graph ?graph " 
				+ "{ "
				+ "?entity j4:observes ?prop ." 
				+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
                + "?xent j2:hasValue ?vxent ."
                + "?vxent   j2:numericalValue ?xval ." // xvalue
                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
                + "?yent j2:hasValue ?vyent ."
                + "?vyent   j2:numericalValue ?yval ." // yvalue
				+ "?graph j2:hasAddress <"+cityiri+"> ."
				+ "?graph j2:enumerationValue ?stnname ."
				+ "?prop a ?class ."
				+ "?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j6:hasTime ?proptime ."
				+ "?proptime   j6:inXSDDateTimeStamp ?proptimeval ."
				
				+ "}" 
				+ "}ORDER BY DESC(?proptimeval) Limit30";
		  
		  
		  List<String[]> listiristn = new ArrayList<String[]>();

		//String dataseturl = rdf4jServer + "/repositories/" + repositoryID;// which is the weather stn dataset
		List<String[]> listmap = queryEndPointDataset(querycontext); //it will give 30 data
			
		double yfinal= 0.0; //temporary
		double xfinal=0.0; //temporary
		String mainstniri="";//temporary
		String mainstnname="";
			//find the main station now
			double distmin=CalculationUtils.distanceWGS84(y0,x0, Double.valueOf(listmap.get(0)[3]),Double.valueOf(listmap.get(0)[2]),"K");
			String latesttimereference=listmap.get(0)[5];
			for(int r=0;r<listmap.size();r++) {
				if(listmap.get(r)[5].contains(latesttimereference)) {
					String yval=listmap.get(r)[3];
					String xval=listmap.get(r)[2];
					double dist=CalculationUtils.distanceWGS84(y0,x0, Double.valueOf(yval),Double.valueOf(xval),"K");
					if(Math.abs(dist)<distmin) {
						mainstniri=listmap.get(r)[0];
						distmin=Math.abs(dist);
						yfinal=Double.valueOf(yval);
						xfinal=Double.valueOf(xval);
						mainstnname=listmap.get(r)[1];
					}
				}
			}
		
			//find the 2nd station now
			double distmax=0.0;
			String secondiri="";
			String secondstnname="";
			for(int r=0;r<listmap.size();r++) {
				if(listmap.get(r)[5].contains(latesttimereference)) {
					String yval=listmap.get(r)[3];
					String xval=listmap.get(r)[2];
					double dist=CalculationUtils.distanceWGS84(yfinal,xfinal, Double.valueOf(yval),Double.valueOf(xval),"K");

					if(Math.abs(dist)>distmax) { 
						secondiri=listmap.get(r)[0];
						distmax=Math.abs(dist);
						secondstnname=listmap.get(r)[1];
					}	
				}
			}
			
			System.out.println ("main stn final= "+mainstniri);
			System.out.println("dist minimum final from center point= "+distmin);
			System.out.println ("2nd stn final= "+secondiri);
			System.out.println("dist maximum final from the 1st stn= "+distmax);
			String[]mainstndata= {mainstniri,mainstnname};
			String[]secondstndata= {secondiri,secondstnname};
			listiristn.add(mainstndata); //as the main stn
			listiristn.add(secondstndata); //as the 2nd stn



		
		return listiristn; //expected to return list of 2 stn iri
	}


	private List<String[]> queryEndPointDataset(String querycontext) {
		String dataseturl = KeyValueManager.get(IKeys.DATASET_WEATHER_URL);
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
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
		
	
	public void addFiletoRepo(RepositoryConnection con,String filename,String contextiri, String midfix) {
		
		String fileprefix="C:/Users/KADIT01/TOMCAT/webapps/ROOT/kb/"+midfix+"/";
		String iriprefix="http://www.theworldavatar.com/kb/"+midfix+"/";
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
	
	public List<String[]> provideDataRepoRoutine(String context,String propnameclass) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?vprop ?propval ?proptime ?proptimeval "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
				//+ " ?entity a j5:T-Sensor ." 
				+ "  ?entity j4:observes ?prop ."
                + "?entity   j7:hasGISCoordinateSystem ?coordsys ."
                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
                + "?xent j2:hasValue ?vxent ."
                + "?vxent   j2:numericalValue ?xval ." // xvalue
                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
                + "?yent j2:hasValue ?vyent ."
                + "?vyent   j2:numericalValue ?yval ." // yvalue
				
				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		
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
    
    public static String getWeatherDataFromAccuweatherAPI(String cityIRI) throws URISyntaxException {
    	String cityname=null;
    	if(cityIRI.toLowerCase().contains("singapore")) {
    		cityname="singapore";
    	}else if(cityIRI.toLowerCase().contains("kong")) {
    		cityname="hongkong";
    	}else if(cityIRI.toLowerCase().contains("berlin")) {
    		cityname="berlin";
    	}else if(cityIRI.toLowerCase().contains("hague")) {
    		cityname="Den%20Haag"; //unique case
    	}
    	String requestURL = String.format(weatherRequest, cityname);
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
  		
	public void updateRepoNewMethod(String context,String propnameclass, String newpropvalue, String newtimestamp) {
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT ?vprop ?proptime "
//				+ "WHERE " //it's replaced when named graph is used
				+ "{graph "+"<"+context+">"
				+ "{ "
		
				+ "  ?entity j4:observes ?prop ."

				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
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
				+ "<" + keyvaluemapold.get(0)[0]+ "> j2:numericalValue ?oldpropertydata ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddata ."
				+ "} "
				+ "INSERT {"
				+ "<" + keyvaluemapold.get(0)[0]+ "> j2:numericalValue \""+newpropvalue+"\"^^xsd:double ."
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp \""+newtimestamp+"\" ." 
				+ "} "
				+ "WHERE { "
				+ "<" + keyvaluemapold.get(0)[0]+ "> j2:numericalValue ?oldpropertydata ."	
				+ "<" + keyvaluemapold.get(0)[1]+ "> j6:inXSDDateTimeStamp ?olddata ."
				+ "}";
		
			
			KnowledgeBaseClient.update(KeyValueManager.get(IKeys.DATASET_WEATHER_URL), null, sparqlupdate);

		
		
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
		
		for(int r=0;r<oldcontent.size();r++) {
			String[]content= {oldcontent.get(r)[1],oldcontent.get(r)[3]};
			valuemapold.add(content);
		}
		valuemapold.remove(0);
		String []newcontent= {newpropvalue,newtimestamp};
		valuemapold.add(newcontent);
		ValueFactory f=repo.getValueFactory();
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
					+ "} "
					+ "WHERE { " 
					+ "}";
			
		KnowledgeBaseClient.update(KeyValueManager.get(IKeys.DATASET_WEATHER_URL), null, sparqlupdate2);	

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
			String[] filenames = { index+"CloudCoverSensor-" + number + ".owl",
					index+"TemperatureSensor-" + number + ".owl", index+"WindSpeedSensor-" + number + ".owl",
					index+"PrecipitationSensor-" + number + ".owl", index+"PressureSensor-" + number + ".owl",
					index+"RelativeHumiditySensor-" + number + ".owl", index+"WindDirectionSensor-" + number + ".owl" };
			String context = "http://www.theworldavatar.com/kb/"+midfix+"/WeatherStation-" + number
					+ ".owl#WeatherStation-" + number;
			System.out.println("upload files for graph");
			for (String el : filenames) {
				new WeatherAgent().addFiletoRepo(con, el, context,midfix);

			}
		}
	}
	
	public void updateRepo(RepositoryConnection con,String context,String propnameclass, String newpropvalue, String newtimestamp) {
		List<String[]>currentdatarepo= provideDataRepoRoutine(context,propnameclass);
		System.out.println("current size= "+currentdatarepo.size());
		removeDataRepoRoutine(con,currentdatarepo);
		insertDataRepo(con, currentdatarepo,newpropvalue, newtimestamp,context);
		System.out.println("update for "+propnameclass+" in context "+context+" is done");
	}
		
	public void updatePropertiesFromDataGov(Map<String,String> map,String properties, JSONObject datasource,String completeformattime) {
		JSONArray data = datasource.getJSONObject("metadata").getJSONArray("stations");
		for (int r = 0; r < data.length(); r++) {
			String name = data.getJSONObject(r).get("name").toString();
			System.out.println("name= "+name);
			if(map.get(name)!=null) {
				String propertyValue = datasource.getJSONArray("items").getJSONObject(0).getJSONArray("readings").getJSONObject(r)
						.get("value").toString(); 
				if(properties.contentEquals("OutsideWindSpeed")){
					String newpropertyValue =""+Double.valueOf(propertyValue)*0.514444;
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),properties,newpropertyValue,completeformattime);
				}else if(properties.contentEquals("OutsideAirRelativeHumidity")){
					String newpropertyValue =""+Double.valueOf(propertyValue)/100;
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),properties,newpropertyValue,completeformattime);
				}else {
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),properties,propertyValue,completeformattime);
				}
			}
		}

	}
	
	public void updatePropertiesFromDataAccuWeather(Map<String,String> map,String properties, JSONObject datasource,String completeformattime,String cityIRI) throws URISyntaxException {		
		String result= getWeatherDataFromAccuweatherAPI(cityIRI);
    	JSONObject joPr= new JSONObject(result);
    	String precipitation="0.0"; //in mm
    	if (joPr.has("rain")) {
    		precipitation = joPr.getJSONObject("rain").optString("3h",joPr.getJSONObject("rain").get("1h").toString());
		}
    	String pressure=joPr.getJSONObject("main").get("pressure").toString(); //in hPa
    	
    	String cloudcover=joPr.getJSONObject("clouds").get("all").toString(); //in %
    	
    	String windspeed=joPr.getJSONObject("wind").get("speed").toString(); // in m/s
    	String winddirection="0.0";
    	if(joPr.getJSONObject("wind").has("deg")) {
    		winddirection=joPr.getJSONObject("wind").get("deg").toString();
    	}
    	String relativehumidity=joPr.getJSONObject("main").get("humidity").toString();
    	String temperature=joPr.getJSONObject("main").get("temp").toString();
    	
    	
    	
		if (cityIRI.contains("singapore")) {
			JSONArray data = datasource.getJSONObject("metadata").getJSONArray("stations");
			for (int r = 0; r < data.length(); r++) {
				String name = data.getJSONObject(r).get("name").toString();
				if (properties.contentEquals("OutsideAirCloudCover")) {
					String newcloudcover = "" + Double.valueOf(cloudcover) / 100; //stored in decimal
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), properties, newcloudcover,
							completeformattime);// stored in decimal
				}else if (properties.contentEquals("OutsideAirPressure")) {
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), properties, pressure,
							completeformattime);
				}

			}
		}else if (cityIRI.contains("kong")) {
			
			JSONArray stnCollection = datasource.getJSONArray("HKweather");
			for (int r = 0; r < stnCollection.length(); r++) {
				String name = stnCollection.getJSONObject(r).get("stnname").toString();
				if (properties.contentEquals("OutsideAirCloudCover")) {
					String newcloudcover= "" + Double.valueOf(cloudcover) / 100;//stored in decimal
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), properties, newcloudcover,
							completeformattime);// stored in decimal
				} else if (properties.contentEquals("OutsideAirPrecipitation")) {
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), properties, precipitation,
							completeformattime);// stored in decimal
				}

			}
			
		}else {
			String name=null;
			if (cityIRI.contains("berlin")) {
				name ="Berlin-Alexanderplatz";
//				iri="http://www.theworldavatar.com/kb/deu/berlin/WeatherStation-001.owl#WeatherStation-001";
			}else if (cityIRI.contains("hague")) {
				name ="Scheveningen";
//				iri="http://www.theworldavatar.com/kb/nld/thehague/WeatherStation-001.owl#WeatherStation-001";
			}
			String iri=map.get(name).toString();
			if (properties.contentEquals("OutsideAirCloudCover")) {
				String newcloudcover= "" + Double.valueOf(cloudcover) / 100;//stored in decimal
				new WeatherAgent().updateRepoNewMethod(iri, properties, newcloudcover,
						completeformattime);// stored in decimal
			} else if (properties.contentEquals("OutsideAirPrecipitation")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, precipitation,
						completeformattime);// stored in decimal
			}else if (properties.contentEquals("OutsideAirPressure")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, pressure,
						completeformattime);
			}else if (properties.contentEquals("OutsideAirTemperature")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, temperature,
					completeformattime);
			}else if (properties.contentEquals("OutsideAirRelativeHumidity")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, relativehumidity,
						completeformattime);
			}else if (properties.contentEquals("OutsideWindSpeed")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, windspeed,
						completeformattime);
			}else if (properties.contentEquals("OutsideWindDirection")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, winddirection,
						completeformattime);
			}
			
		}
			
	}

	public void executePeriodicUpdate(String cityIRI) throws URISyntaxException {
		String completeformat=WeatherAgent.provideCurrentTime();
		Map<String,String>stnmap=extractMappingname();
		if(cityIRI.toLowerCase().contains("singapore")) {
			
			String weatherPrecipitation = getWeatherDataFromGovAPI("/v1/environment/rainfall", null);
			JSONObject joPrecipitation = new JSONObject(weatherPrecipitation);//in mm
			String weatherTemperature = getWeatherDataFromGovAPI("/v1/environment/air-temperature", null);
			JSONObject joTemperature = new JSONObject(weatherTemperature);//in celcius
			String weatherhum = getWeatherDataFromGovAPI("/v1/environment/relative-humidity", null);
			JSONObject joRH = new JSONObject(weatherhum);//in percent
			String weatherwindspeed = getWeatherDataFromGovAPI("/v1/environment/wind-speed", null);
			JSONObject jowindspeed = new JSONObject(weatherwindspeed);//knots
			String weatherwinddir = getWeatherDataFromGovAPI("/v1/environment/wind-direction", null);
			JSONObject jowinddirection = new JSONObject(weatherwinddir);//degree
			
	    	updatePropertiesFromDataGov(stnmap,"OutsideAirTemperature",joTemperature,completeformat);
	    	updatePropertiesFromDataGov(stnmap,"OutsideWindDirection",jowinddirection,completeformat);
	    	updatePropertiesFromDataGov(stnmap,"OutsideWindSpeed",jowindspeed,completeformat); 
	    	updatePropertiesFromDataGov(stnmap,"OutsideAirRelativeHumidity",joRH,completeformat);
	    	updatePropertiesFromDataGov(stnmap,"OutsideAirPrecipitation",joPrecipitation,completeformat);
	    	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirCloudCover",jowinddirection,completeformat,cityIRI);//accu
	    	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirPressure",jowinddirection,completeformat,cityIRI);//accu
	    	//in accuweather the json data is needed from the data gov to update each stn written in data gov
	  
		}else if(cityIRI.toLowerCase().contains("kong")) {
			JSONObject jo = new JSONObject();
			String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/GetHKUWeatherLatestData",jo.toString());
			JSONObject joPr = new JSONObject(result);
			updatePropertiesFromHKU(stnmap,"OutsideAirTemperature",joPr,completeformat);
			updatePropertiesFromHKU(stnmap,"OutsideAirRelativeHumidity",joPr,completeformat);
			updatePropertiesFromHKU(stnmap,"OutsideWindSpeed",joPr,completeformat);
			updatePropertiesFromHKU(stnmap,"OutsideWindDirection",joPr,completeformat);
        	updatePropertiesFromHKU(stnmap,"OutsideAirPressure",joPr,completeformat);//in hPa exactly the same as millibar
        	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirCloudCover",joPr,completeformat,cityIRI);//accu
	    	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirPrecipitation",joPr,completeformat,cityIRI);//accu
	    	
		}else {
			JSONObject joPr = new JSONObject();
			updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirTemperature",joPr,completeformat,cityIRI);
			updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirRelativeHumidity",joPr,completeformat,cityIRI);
			updatePropertiesFromDataAccuWeather(stnmap,"OutsideWindSpeed",joPr,completeformat,cityIRI);
			updatePropertiesFromDataAccuWeather(stnmap,"OutsideWindDirection",joPr,completeformat,cityIRI);
			updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirPressure",joPr,completeformat,cityIRI);//in hPa exactly the same as millibar
        	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirCloudCover",joPr,completeformat,cityIRI);//accu
	    	updatePropertiesFromDataAccuWeather(stnmap,"OutsideAirPrecipitation",joPr,completeformat,cityIRI);//accu
	    	
		}
	}
	

	public void updatePropertiesFromHKU(Map<String,String> map,String propertyname,JSONObject data,String completeformattime) {//later need to be updated


		// input stn name:
		// output sequence index
		JSONArray stnCollection = data.getJSONArray("HKweather");
		int size = stnCollection.length();
		for (int r = 0; r < size; r++) {
			String name = stnCollection.getJSONObject(r).get("stnname").toString();
			String lat1 = stnCollection.getJSONObject(r).get("y").toString();
			String long1 = stnCollection.getJSONObject(r).get("x").toString();
			String timestamp = stnCollection.getJSONObject(r).get("timestamp").toString();
			String propertyValue = stnCollection.getJSONObject(r).get(propertyname).toString(); 
			if (propertyname.contentEquals("OutsideAirRelativeHumidity")) {
				String newpropertyValue = "" + Double.valueOf(propertyValue) / 100; //stored in decimal
				new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),propertyname,newpropertyValue,completeformattime);
			}else if (propertyname.contentEquals("OutsideWindSpeed")) {
				String newpropertyValue = "" + Double.valueOf(propertyValue) *1000/3600; //stored in m/s
				new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),propertyname,newpropertyValue,completeformattime);
			}else {
				new WeatherAgent().updateRepoNewMethod(map.get(name).toString(),propertyname,propertyValue,completeformattime);
			}
			
			
		}


	}
	

	
	public static void main(String[]args) { //used for upload all content locally

		RepositoryConnection con = repo.getConnection();
//		String location="singapore";
//		String location="hong kong";
		String location="berlin";
//		String location="the hague";
		WeatherAgent a=new WeatherAgent();
		a.resetRepoTrial(con,location); //currently the context is not used
		
		String completeformat=WeatherAgent.provideCurrentTime();

			System.out.println("currenttime= "+ completeformat);		

		
	}
	
}
