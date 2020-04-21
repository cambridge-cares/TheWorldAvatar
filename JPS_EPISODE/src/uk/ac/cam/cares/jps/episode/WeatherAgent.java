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
	String stnname1="Sentosa"; //current selected name
	String stnname2="Pulau Ubin"; //current selected name

	public static final String weatherRequest = "http://api.openweathermap.org/data/2.5/weather?units=metric&q=%s&appid=329f65c3f7166977f6751cff95bfcb0a";

	
	//which data should be taken from?
	//should use top node or just context is enough?

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
			String lowx = region.getJSONObject("regioninput").getJSONObject("lowercorner")
					.get("lowerx").toString();
			String lowy = region.getJSONObject("regioninput").getJSONObject("lowercorner")
					.get("lowery").toString();
			String upx = region.getJSONObject("regioninput").getJSONObject("uppercorner")
					.get("upperx").toString();
			String upy = region.getJSONObject("regioninput").getJSONObject("uppercorner")
					.get("uppery").toString();
			double proclowx = Double.valueOf(lowx);
			double procupx = Double.valueOf(upx);
			double proclowy = Double.valueOf(lowy);
			double procupy = Double.valueOf(upy);
			double[] center = CalculationUtils.calculateCenterPoint(procupx, procupy, proclowx, proclowy);
			double[] centerPointConverted = CRSTransformer.transform(sourceCRSName,CRSTransformer.EPSG_4326,
					center);
			
			
		    
		RepositoryConnection con = repo.getConnection();	

		List<String> listmap = extractAvailableContext(cityiri,centerPointConverted[0],centerPointConverted[1]);
		 String context=listmap.get(0); //main stn
		 String context2=listmap.get(1); // the furthest station
		 
		 //here the context will be more than 2, need to decide which two are picked
		 //for now only the 2 stn choosen will be updated
		 //for more stn to be updated, need to update the "executeFunctionPeriodically"
		 		 	
		 try {
			new WeatherAgent().executeFunctionPeriodically(con,context,context2);
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
	    		

	public List<String> extractAvailableContext(String cityiri,double x0,double y0) {
		
		  String querycontext = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "SELECT DISTINCT ?graph ?stnname ?xval ?yval " 
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
				+ "}" 
				+ "}";
		  
		  List<String> listiristn = new ArrayList<String>();
		
		
		//String dataseturl = rdf4jServer + "/repositories/" + repositoryID;// which is the weather stn dataset
		List<String[]> listmap = queryEndPointDataset(querycontext);
		
		double yfinal= 0.0; //temporary
		double xfinal=0.0; //temporary
		String mainstniri="";//temporary
			
			//find the main station now
			double distmin=CalculationUtils.distanceWGS84(y0,x0, Double.valueOf(listmap.get(0)[3]),Double.valueOf(listmap.get(0)[2]),"K");
			for(int r=0;r<listmap.size();r++) {
				String yval=listmap.get(r)[3];
				String xval=listmap.get(r)[2];
				double dist=CalculationUtils.distanceWGS84(y0,x0, Double.valueOf(yval),Double.valueOf(xval),"K");
				if(Math.abs(dist)<distmin) {
					mainstniri=listmap.get(r)[0];
					distmin=Math.abs(dist);
					yfinal=Double.valueOf(yval);
					xfinal=Double.valueOf(xval);
				}	
			}
		
			//find the 2nd station now
			double distmax=0.0;
			String secondiri="";
			for(int r=0;r<listmap.size();r++) {
				String yval=listmap.get(r)[3];
				String xval=listmap.get(r)[2];
				double dist=CalculationUtils.distanceWGS84(yfinal,xfinal, Double.valueOf(yval),Double.valueOf(xval),"K");
				if(Math.abs(dist)>distmax) {
					secondiri=listmap.get(r)[0];
					distmax=Math.abs(dist);
				}
				
			}
			
			System.out.println ("main stn final= "+mainstniri);
			System.out.println("dist minimum final from center point= "+distmin);
			System.out.println ("2nd stn final= "+secondiri);
			System.out.println("dist maximum final from the 1st stn= "+distmax);
			listiristn.add(mainstniri); //as the main stn
			listiristn.add(secondiri); //as the 2nd stn



		
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
		
	
	public void addFiletoRepo(RepositoryConnection con,String filename,String contextiri) {
		String dir="";
		if(filename.contains("SG")) {
			dir="sgp/singapore";
		}else if(filename.contains("HK")) {
			dir="hkg/hongkong";
		}
		
		String fileprefix="C:/Users/KADIT01/TOMCAT/webapps/ROOT/kb/"+dir+"/";
		String iriprefix="http://www.theworldavatar.com/kb/"+dir+"/";
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
	
	public void updateRepoNew(String context,String propnameclass, String newpropvalue, String newtimestamp) {
		List<String[]>oldcontent= provideDataRepoRoutine(context,propnameclass);
		List<String[]> valuemapold= new ArrayList<String[]>();
		valuemapold.addAll(oldcontent);
		valuemapold.remove(0);
		String[]newcontent= {"key1prop",newpropvalue,"key2time",newtimestamp};
		valuemapold.add(newcontent);
		
		for(int x=0; x<oldcontent.size();x++) {
			String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "WITH <" + context + ">"
					+ "DELETE { "
					+ "<" + oldcontent.get(x)[0]+ "> j2:numericalValue \""+oldcontent.get(x)[1]+"\"^^xsd:double ."
					+ "<" + oldcontent.get(x)[2]+ "> j6:inXSDDateTimeStamp \""+ oldcontent.get(x)[3]+"\" ."
					+ "} "
					+ "INSERT {"
					+ "<" + oldcontent.get(x)[0]+ "> j2:numericalValue \""+valuemapold.get(x)[1]+"\"^^xsd:double ."
					+ "<" + oldcontent.get(x)[2]+ "> j6:inXSDDateTimeStamp \""+valuemapold.get(x)[3]+"\" ." 
					+ "} "
					+ "WHERE { " 
					+ "}";
			
			KnowledgeBaseClient.update(KeyValueManager.get(IKeys.DATASET_WEATHER_URL), null, sparqlupdate);

		}
		
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
	
	public void resetRepoTrial(RepositoryConnection con,String location) { //unused for the servlet
		if (location.contains("singapore")) {
			for (int d = 1; d <= 12; d++) {
				String number = "00" + d;
				if (d > 9) {
					number = "0" + d;
				}
				String[] filenames = { "SGCloudCoverSensor-" + number + ".owl",
						"SGTemperatureSensor-" + number + ".owl", "SGWindSpeedSensor-" + number + ".owl",
						"SGPrecipitationSensor-" + number + ".owl", "SGPressureSensor-" + number + ".owl",
						"SGRelativeHumiditySensor-" + number + ".owl", "SGWindDirectionSensor-" + number + ".owl" };
				String context = "http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-" + number
						+ ".owl#WeatherStation-" + number;
				System.out.println("upload files for graph");
				for (String el : filenames) {
					new WeatherAgent().addFiletoRepo(con, el, context);

				}

			}
		}else if(location.contains("kong")) {
			for (int d = 1; d <= 8; d++) {
				String number = "00" + d;
				if (d > 9) {
					number = "0" + d;
				}
				String[] filenames = { "HKCloudCoverSensor-" + number + ".owl",
						"HKTemperatureSensor-" + number + ".owl", "HKWindSpeedSensor-" + number + ".owl",
						"HKPrecipitationSensor-" + number + ".owl", "HKPressureSensor-" + number + ".owl",
						"HKRelativeHumiditySensor-" + number + ".owl", "HKWindDirectionSensor-" + number + ".owl" };
				String context = "http://www.theworldavatar.com/kb/hkg/hongkong/WeatherStation-" + number
						+ ".owl#WeatherStation-" + number;
				System.out.println("upload files for graph");
				for (String el : filenames) {
					new WeatherAgent().addFiletoRepo(con, el, context);

				}

			}
			
		}
//		String[] filenames= {"SGCloudCoverSensor-001.owl","SGTemperatureSensor-001.owl","SGWindSpeedSensor-001.owl","SGSolarIrradiationSensor-001.owl","SGPrecipitationSensor-001.owl","SGPressureSensor-001.owl","SGRelativeHumiditySensor-001.owl","SGWindDirectionSensor-001.owl"};
		
//		String[] filenames2= {"SGWindSpeedSensor-002.owl","SGWindDirectionSensor-002.owl"};

	}
	
	public void updateRepo(RepositoryConnection con,String context,String propnameclass, String newpropvalue, String newtimestamp) {
		List<String[]>currentdatarepo= provideDataRepoRoutine(context,propnameclass);
		System.out.println("current size= "+currentdatarepo.size());
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
		
		//new WeatherAgent().updateRepo(con,context,"OutsideAirTemperature",result.getJSONObject("temperature").get("value").toString(),completeformat);//stored in Celcius
		//new WeatherAgent().updateRepo(con,context,"OutsideWindSpeed",""+convertedspeed1,completeformat); //stored in m/s instead of knot
		//new WeatherAgent().updateRepo(con,context,"OutsideWindDirection",result.getJSONObject("winddirection1").get("value").toString(),completeformat);//stored in degree
		//new WeatherAgent().updateRepo(con,context,"OutsideAirCloudCover",""+convertedcloud,completeformat);//stored in decimal instead of %
		//new WeatherAgent().updateRepo(con,context,"OutsideAirPressure",result.getJSONObject("pressure").get("value").toString(),completeformat);//stored in mBar, no need conversion from hPa
		//new WeatherAgent().updateRepo(con,context,"OutsideAirPrecipitation",result.getJSONObject("precipitation").get("value").toString(),completeformat);// stored in mm
		//new WeatherAgent().updateRepo(con,context,"OutsideAirRelativeHumidity",""+convertedRH,completeformat);//stored in decimal instead of %
		//new WeatherAgent().updateRepo(con,context,"OutsideAirProperties","25.4",completeformat); //it's for solar irradiation
		//new WeatherAgent().updateRepo(con,context2,"OutsideWindSpeed",""+convertedspeed2,completeformat);//stored in m/s instead of knot
		//new WeatherAgent().updateRepo(con,context2,"OutsideWindDirection",result.getJSONObject("winddirection2").get("value").toString(),completeformat); //stored in degree
		
		new WeatherAgent().updateRepoNew(context,"OutsideAirTemperature",result.getJSONObject("temperature").get("value").toString(),completeformat);//stored in Celcius
		new WeatherAgent().updateRepoNew(context,"OutsideWindSpeed",""+convertedspeed1,completeformat); //stored in m/s instead of knot
		new WeatherAgent().updateRepoNew(context,"OutsideWindDirection",result.getJSONObject("winddirection1").get("value").toString(),completeformat);//stored in degree
		new WeatherAgent().updateRepoNew(context,"OutsideAirCloudCover",""+convertedcloud,completeformat);//stored in decimal instead of %
		new WeatherAgent().updateRepoNew(context,"OutsideAirPressure",result.getJSONObject("pressure").get("value").toString(),completeformat);//stored in mBar, no need conversion from hPa
		new WeatherAgent().updateRepoNew(context,"OutsideAirPrecipitation",result.getJSONObject("precipitation").get("value").toString(),completeformat);// stored in mm
		new WeatherAgent().updateRepoNew(context,"OutsideAirRelativeHumidity",""+convertedRH,completeformat);//stored in decimal instead of %
		//new WeatherAgent().updateRepo(con,context,"OutsideAirProperties","25.4",completeformat); //it's for solar irradiation
		new WeatherAgent().updateRepoNew(context2,"OutsideWindSpeed",""+convertedspeed2,completeformat);//stored in m/s instead of knot
		new WeatherAgent().updateRepoNew(context2,"OutsideWindDirection",result.getJSONObject("winddirection2").get("value").toString(),completeformat); //stored in degree
	}
	

	
	public static void main(String[]args) {

		RepositoryConnection con = repo.getConnection();
//		String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-001.owl#WeatherStation-001";
//		String context2="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-002.owl#WeatherStation-002";
		String location="singapore";
//		String location="hong kong";
		WeatherAgent a=new WeatherAgent();
		a.resetRepoTrial(con,location); //currently the context is not used
		
//		new WeatherAgent().queryValuefromRepo(con,context); only for query testing
		String completeformat=WeatherAgent.provideCurrentTime();

			System.out.println("currenttime= "+ completeformat);

			JSONObject apidata = null;
			

//		new WeatherAgent().addinstancetoRepo(con);
		//new WeatherAgent().deleteValuetoRepo(con);
		
	}
	
}
