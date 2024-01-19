package uk.ac.cam.cares.jps.dispersion.episode;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.ws.rs.BadRequestException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.cts.CRSFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
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

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

@WebServlet(urlPatterns = {"/SensorWeatherAgent"})
public class WeatherAgent extends JPSHttpServlet {

	/**
	 * Summary of weather agent:
	 * Main inputs: City and the coordinates where we wish to model dispersion
	 * Outputs: IRI of 2 weather stations. The first station is closest to the centre, most of the weather data used
	 * are taken from this station. The second station is the furthest away and this provides the wind speed and direction 
	 * for modelling.
	 */
	private static final long serialVersionUID = 1L;
	public static String rdf4jServer = "http://localhost:8080/rdf4j-server"; //this is only the local repo, changed if it's inside claudius
	public static String repositoryID = "weatherstation";
	public static Repository repo = new HTTPRepository(rdf4jServer, repositoryID);
	public static final String dataseturl=KeyValueManager.get(IKeys.DATASET_WEATHER_URL);
	public static final String weatherRequest = "http://api.openweathermap.org/data/2.5/weather?units=metric&q=%s&appid=329f65c3f7166977f6751cff95bfcb0a";
	
	protected void setLogger() {
	        logger = LoggerFactory.getLogger(WeatherAgent.class);
	    }
	    Logger logger = LoggerFactory.getLogger(WeatherAgent.class);
	    
	    protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
	    	JSONObject response= new JSONObject();
	    	
    		if(validateInput(requestParams)) {
				String path = request.getServletPath();
				System.out.println("path= " + path);

				// First the centre of the query location is calculated, this is used to select the station later
				String cityiri = requestParams.get("city").toString();
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
				double[] centerPointConverted = CRSTransformer.transform(sourceCRSName, CRSTransformer.EPSG_4326,
						center);

				// The latest update time is obtained, if it is less than an hour ago, the data won't be updated
				String stntimeinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
						+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
						+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
						+ "PREFIX j6:<http://www.w3.org/2006/time#>"
						+ "SELECT ?class ?propval ?proptimeval "
						+ "{ GRAPH ?gr "
						+ "{ "

						+ "  ?entity j4:observes ?prop ."
						+ " ?prop a ?class ."
						+ " ?prop   j2:hasValue ?vprop ."
						+ " ?vprop   j2:numericalValue ?propval ."
						+ " ?vprop   j6:hasTime ?proptime ."
						+ " ?proptime   j6:inXSDDateTime ?proptimeval ."
						+ "}"
						+ "}"
						+ "ORDER BY DESC(?proptimeval)LIMIT 1";

				List<String[]> listtime = queryEndPointDataset(stntimeinfo);

				String timelatest = listtime.get(0)[2];
				boolean needupdate = isUpdateNeeded(timelatest);

				try {
					if (needupdate == true) {
						executePeriodicUpdate("singapore");
						executePeriodicUpdate("kong");
						executePeriodicUpdate("hague");
						executePeriodicUpdate("berlin");
					}
				} catch (URISyntaxException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				//extractAvailableContext will provide the two stations for modelling
				List<String[]> listmap = extractAvailableContext(cityiri, centerPointConverted[0], centerPointConverted[1]);
				String context = listmap.get(0)[0]; //main stn
				String context2 = listmap.get(1)[0]; // the furthest station
				JSONArray station = new JSONArray();
				station.put(context);
				station.put(context2);
				response.put("stationiri", station);
			}

			return response;
		}

    private boolean validateInput(JSONObject input) {
        boolean valid = false;
        try {
            String cityiri=input.get("city").toString();

            // check whether it's IRI
            new URL(cityiri).toURI();

            JSONObject region = input.getJSONObject("region");
            String lowx = region.getJSONObject("lowercorner").get("lowerx").toString();
            String lowy = region.getJSONObject("lowercorner").get("lowery").toString();
            String upx = region.getJSONObject("uppercorner").get("upperx").toString();
            String upy = region.getJSONObject("uppercorner").get("uppery").toString();

            // check if provided coordinates are valid doubles
            Double.valueOf(lowx);
            Double.valueOf(upx);
            Double.valueOf(lowy);
            Double.valueOf(upy);

            // validate coordinates
            String sourceCRSName = region.optString("srsname");
            if ((sourceCRSName == null) || sourceCRSName.isEmpty()) { 
                sourceCRSName = CRSTransformer.EPSG_4326; 
            }

            CRSFactory crsFact = new CRSFactory();
            RegistryManager registryManager = crsFact.getRegistryManager();
            registryManager.addRegistry(new EPSGRegistry());
            crsFact.getCRS(sourceCRSName);

            valid = true;
        } catch (Exception e) {
            throw new BadRequestException(e);
        }

		return valid;
	}

	private Map<String,String> extractMappingname(){
		// This method simply gives all the stations in a given city
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
		// Obtain a list of weather stations in the given city, it's not clear why the limit is set to 30
		String querygraph = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "SELECT DISTINCT ?graph " 
					+ "{ graph ?graph " 
					+ "{ "
					+ "?graph j2:hasAddress <"+cityiri+"> ."
					+ "}" 
					+ "}Limit30";
		List<String[]> listsgstn = queryEndPointDataset(querygraph); //it will give 30 data
		
		// The following method gives two sets of time stamps from the two most up-to-date stations
		List<String> time2 = getLatestTimeStationUpdate(listsgstn);

		// If there is at least one available station that satisfies the query in the latest time stamp, the second one will be skipped
		// The current query requires the station to have exactly seven properties, not more not less
		// I suspect this is a requirement of the episode agent to have all 7 properties
		// There are a number of stations in Singapore that give 5 properties
		// The indices of the queried properties could be improved to use variables instead

		// listmap = list of stations that contain exactly 7 data points at the queried time stamp
		
		// The two main reasons for the following implementation 
		// 1) Sometimes a weather station might be offline, so it is preferable to get a station further away but more up-to-date
		// 2) Each station has properties from two weather sources to form a complete set for the episode model
		// This is a bad and very inefficient way to do this because the queried time from the previous function belongs
		// to a single station, and it is looping through all the weather stations to find the right one.
		// Also, the properties are queried 1 by 1, so they might have different time stamp if the code hangs halfway?
		List<String[]> listmap=new ArrayList<String[]>();
		for(int y=0;y<time2.size();y++) { 
			for(int x=0;x<listsgstn.size();x++) {
				String context= listsgstn.get(x)[0];
				String query = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
							+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
							+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
							+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
							+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
							+ "SELECT ?entity ?graph ?stnname ?xval ?yval " 
							+ "{ graph <"+context+"> " 
							+ "{ "
							+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
			                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
			                + "?xent j2:hasValue ?vxent ."
			                + "?vxent   j2:numericalValue ?xval ."
			                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
			                + "?yent j2:hasValue ?vyent ."
			                + "?vyent   j2:numericalValue ?yval ."
							+ "?graph j2:hasAddress <"+cityiri+"> ."
							+ "?graph j2:enumerationValue ?stnname ."
							+ "?entity j4:observes ?prop ."
							+ "?prop   j2:hasValue ?vprop ."
							+ " ?vprop   j6:hasTime ?proptime ."
							+ "?proptime   j6:inXSDDateTime \""+time2.get(y)+"\"^^xsd:dateTime ."			
							+ "}" 
							+ "}ORDER BY DESC(?proptimeval) Limit7";
				List<String[]> listsgstndata = queryEndPointDataset(query);
				if(listsgstndata.size()==7) {
					String[]res= {listsgstndata.get(0)[1],listsgstndata.get(0)[2],listsgstndata.get(0)[3],listsgstndata.get(0)[4],time2.get(y)};
					listmap.add(res);
				}
			}
			if(listmap.size()>0) {
				break; // so far in my observation, this will be always executed
			}
		}

		// listiristn will have the 2 stations used for modelling
		List<String[]> listiristn = new ArrayList<String[]>();

		double yfinal= 0.0; //temporary
		double xfinal=0.0; //temporary
		String mainstniri=listmap.get(0)[0]; //temporary
		String mainstnname=listmap.get(0)[1];
		
		//find the main station = station closest to the centre 
		//this station will provide the majority of data for the dispersion model
		//it is unclear why do we use the time reference of the very first station (they are the same)
		double distmin=CalculationUtils.distanceWGS84(y0,x0, Double.valueOf(listmap.get(0)[3]),Double.valueOf(listmap.get(0)[2]),"K");
		String latesttimereference=listmap.get(0)[4];
		for(int r=0;r<listmap.size();r++) {
			if(listmap.get(r)[4].contains(latesttimereference)) {
				String yval=listmap.get(r)[3]; // these hard-coded indices were defined in the initial query
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
		
		//find the 2nd station = furthest from the centre
		//This station provides the wind speed and direction for modelling
		double distmax=0.0;
		String secondiri="";
		String secondstnname="";
		for(int r=0;r<listmap.size();r++) {
			if(listmap.get(r)[4].contains(latesttimereference)) {
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
		String[]mainstndata= {mainstniri,mainstnname,latesttimereference};
		String[]secondstndata= {secondiri,secondstnname,latesttimereference};
		listiristn.add(mainstndata); //as the main stn
		listiristn.add(secondstndata); //as the 2nd stn

		return listiristn; //expected to return list of 2 stn iri
	}


	private List<String> getLatestTimeStationUpdate(List<String[]> listsgstn) {
		List<String>time= new ArrayList<String>();
        // For each station obtain the two latest updated properties' update time
		// This is probably because data were obtained from two different sources and within the same stations
		// there might be different timestamps. 
		// This method will provide two sets of timestamps from two stations, I believe the time is sorted according
		// to the latest timestamp from each station using the 'collection' sort method
		for(int x=0;x<listsgstn.size();x++) {
			String context= listsgstn.get(x)[0];
			String querydata = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
						+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
						+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
						+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
						+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
						+ "SELECT DISTINCT ?stnname ?xval ?yval ?proptimeval " 
						+ "{ graph <"+context+"> " 
						+ "{ "
						+ "?entity   j7:hasGISCoordinateSystem ?coordsys ."
		                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
		                + "?xent j2:hasValue ?vxent ."
		                + "?vxent   j2:numericalValue ?xval ."
		                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
		                + "?yent j2:hasValue ?vyent ."
		                + "?vyent   j2:numericalValue ?yval ."
						+ "?graph j2:enumerationValue ?stnname ."
						+ "?prop   j2:hasValue ?vprop ."
						+ " ?vprop   j6:hasTime ?proptime ."

						+ "?proptime   j6:inXSDDateTime ?proptimeval ."
						
						+ "}" 
						+ "}ORDER BY DESC(?proptimeval) Limit2";
			
			List<String[]> listsgstndata = queryEndPointDataset(querydata);
			String timelatest=listsgstndata.get(0)[3];			  
			String timelatest2=listsgstndata.get(1)[3];

			time.add(timelatest);		
			time.add(timelatest2);
		}
		Collections.sort(time, Collections.reverseOrder()); 
		System.out.println("Sorted ArrayList "
                + "in Descending order : "
                + time);

		List<String> time2 = time.stream().distinct().collect(Collectors.toList()); //set order to have some latest data
		return time2;
	}


	private List<String[]> queryEndPointDataset(String querycontext) {
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}
	    

	public static String provideCurrentTime() {			//timing format should be year, month, date, hour, minute,second		   
			DateTimeFormatter dtf = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
			   ZonedDateTime now = ZonedDateTime.now();
			  
			   String com=dtf.format(now);			   
//			   String date=com.split("/")[2].split(" ")[0];
//			   
//			   String year=com.split("/")[0];
//				String monthdate=com.split("/")[1]+"-"+date;
//				String time=com.split("/")[2].split(" ")[1];
//				String completeformat=year+"-"+monthdate+"T"+time+"+08:00";
				return com;
		}
	
	public boolean isUpdateNeeded(String timelatest) {
		boolean need=true;
		String currentime=provideCurrentTime();
		System.out.println("now= "+currentime);
		DateFormat pstFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.sssXXX");
		try {
			Date dcurrent=pstFormat.parse(currentime);
			Date dlatest=pstFormat.parse(timelatest);
			long diff = dcurrent.getTime() - dlatest.getTime();
			long diffHours = diff / (60 * 60 * 1000) % 24;
			long diffDays = diff / (24 * 60 * 60 * 1000);
			if(diffHours<1&&diffDays==0) {
				need=false;
				return need;
			}
			
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		  		
		return need;
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

				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		
		return keyvaluemapold;
		
		
	}
	
    public static String getWeatherDataFromGovAPI(String path, String json) {
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
    	// These names should be stored in a database or obtained
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
		
//				+ "  ?entity j4:observes ?prop ."

				+ " ?prop a j4:"+propnameclass+" ."
				+ " ?prop   j2:hasValue ?vprop ." 
				+ " ?vprop   j6:hasTime ?proptime ."

				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY ASC(?proptimeval)LIMIT1";
		
		List<String[]> keyvaluemapold =queryEndPointDataset(sensorinfo);
		String propiri=keyvaluemapold.get(0)[0];
		String timeiri=keyvaluemapold.get(0)[1];
		
		String sparqlupdate = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> "
				+ "WITH <" + context + ">"
				+ "DELETE { "
				+ "<" + propiri+ "> j2:numericalValue ?oldpropertydata ."
				+ "<" + timeiri+ "> j6:inXSDDateTime ?olddata ."
				+ "} "
				+ "INSERT {"
				+ "<" + propiri+ "> j2:numericalValue \""+newpropvalue+"\"^^xsd:double ."

				+ "<" + timeiri+ "> j6:inXSDDateTime \""+newtimestamp+"\"^^xsd:dateTime ." 
				+ "} "
				+ "WHERE { "
				+ "<" + propiri+ "> j2:numericalValue ?oldpropertydata ."	

				+ "<" + timeiri+ "> j6:inXSDDateTime ?olddata ."
				+ "}";
		
			
			KnowledgeBaseClient.update(dataseturl, null, sparqlupdate);

		
		
	}
	
	public void removeDataRepoRoutine(RepositoryConnection con,List<String[]>oldcontent) {

			int d=oldcontent.size();
			ValueFactory f=repo.getValueFactory();
			IRI numval=f.createIRI("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");

			IRI timeval=f.createIRI("http://www.w3.org/2006/time#inXSDDateTime");
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

		IRI timeval=f.createIRI("http://www.w3.org/2006/time#inXSDDateTime");
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
			
		KnowledgeBaseClient.update(dataseturl, null, sparqlupdate2);	

	}
		
	public void resetRepoTrial(RepositoryConnection con, String location,String context,String number) {// unused for the servlet
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
			//stnnumber=14;
			stnnumber=1;
			index="SG";
			midfix="sgp/singapore";
		}
		for (int d = 1; d <= stnnumber; d++) {
//			String number = "00" + d;
			if (d > 9&& d<=99) {
//				number = "0" + d;
			}
			String[] filenames = { index+"CloudCoverSensor-" + number + ".owl",
					index+"TemperatureSensor-" + number + ".owl", index+"WindSpeedSensor-" + number + ".owl",
					index+"PrecipitationSensor-" + number + ".owl", index+"PressureSensor-" + number + ".owl",
					index+"RelativeHumiditySensor-" + number + ".owl", index+"WindDirectionSensor-" + number + ".owl" };
			//String context = "http://www.theworldavatar.com/kb/"+midfix+"/WeatherStation-" + number
//					+ ".owl#WeatherStation-" + number;
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
	
	public void updatePropertiesFromDataAccuWeather(Map<String, String> map, String properties, JSONObject datasource,
			String completeformattime, String cityIRI, String apiresult) throws URISyntaxException {
		JSONObject joPr = new JSONObject(apiresult);
		String precipitation = "0.0"; // in mm
		if (joPr.has("rain")) {
			precipitation = joPr.getJSONObject("rain").optString("3h", joPr.getJSONObject("rain").get("1h").toString());
		}
		String pressure = joPr.getJSONObject("main").get("pressure").toString(); // in hPa

		String cloudcover = joPr.getJSONObject("clouds").get("all").toString(); // in %

		String windspeed = joPr.getJSONObject("wind").get("speed").toString(); // in m/s
		String winddirection = "0.0";
		if (joPr.getJSONObject("wind").has("deg")) {
			winddirection = joPr.getJSONObject("wind").get("deg").toString();
		}
		String relativehumidity = joPr.getJSONObject("main").get("humidity").toString();
		String temperature = joPr.getJSONObject("main").get("temp").toString();

		// System.out.println("cityIRI= "+cityIRI);

		if (cityIRI.toLowerCase().contains("singapore")) {
			JSONArray data = datasource.getJSONObject("metadata").getJSONArray("stations");
			for (int r = 0; r < data.length(); r++) {
				String name = data.getJSONObject(r).get("name").toString();
				try {
					String mappedname = map.get(name).toString();
					if (properties.contentEquals("OutsideAirCloudCover")) {
						String newcloudcover = "" + Double.valueOf(cloudcover) / 100; // stored in decimal
						new WeatherAgent().updateRepoNewMethod(mappedname, properties, newcloudcover,
								completeformattime);// stored in decimal
					} else if (properties.contentEquals("OutsideAirPressure")) {
						new WeatherAgent().updateRepoNewMethod(mappedname, properties, pressure, completeformattime);
					}
				} catch (Exception e) {
					System.out.println("new station unrecorded is found");
				}
			}
			// HERE IS FOR BACKUP ACCUWEATHER STATION
			String name = "SGAccuWeather-001";
			String iri = map.get(name).toString();
			if (properties.contentEquals("OutsideAirCloudCover")) {
				String newcloudcover = "" + Double.valueOf(cloudcover) / 100;// stored in decimal
				new WeatherAgent().updateRepoNewMethod(iri, properties, newcloudcover, completeformattime);// stored in
																											// decimal
			} else if (properties.contentEquals("OutsideAirPrecipitation")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, precipitation, completeformattime);// stored in
																											// decimal
			} else if (properties.contentEquals("OutsideAirPressure")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, pressure, completeformattime);
			} else if (properties.contentEquals("OutsideAirTemperature")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, temperature, completeformattime);
			} else if (properties.contentEquals("OutsideAirRelativeHumidity")) {
				String newhumidity = "" + Double.valueOf(relativehumidity) / 100; // stored in decimal
				new WeatherAgent().updateRepoNewMethod(iri, properties, newhumidity, completeformattime);
			} else if (properties.contentEquals("OutsideWindSpeed")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, windspeed, completeformattime);
			} else if (properties.contentEquals("OutsideWindDirection")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, winddirection, completeformattime);
			}

		} else if (cityIRI.toLowerCase().contains("kong")) {
			JSONArray stnCollection = null;
			try {
				stnCollection = datasource.getJSONArray("HKweather");
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
			if (stnCollection != null) {
				for (int r = 0; r < stnCollection.length(); r++) {
					String name = stnCollection.getJSONObject(r).get("stnname").toString();
					try {
						String mappedname = map.get(name).toString();
						if (properties.contentEquals("OutsideAirCloudCover")) {
							String newcloudcover = "" + Double.valueOf(cloudcover) / 100;// stored in decimal
							new WeatherAgent().updateRepoNewMethod(mappedname, properties, newcloudcover,
									completeformattime);// stored in decimal
						} else if (properties.contentEquals("OutsideAirPrecipitation")) {
							new WeatherAgent().updateRepoNewMethod(mappedname, properties, precipitation,
									completeformattime);// stored in decimal
						}
					} catch (Exception e) {
						System.out.println("new station unrecorded is found");
					}
				}
			} 
		} else {
			String name = null;
			if (cityIRI.toLowerCase().contains("berlin")) {
				name = "Berlin-Alexanderplatz";
//					iri="http://www.theworldavatar.com/kb/deu/berlin/WeatherStation-001.owl#WeatherStation-001";
			} else if (cityIRI.toLowerCase().contains("hague")) {
				name = "Scheveningen";
//					iri="http://www.theworldavatar.com/kb/nld/thehague/WeatherStation-001.owl#WeatherStation-001";
			}
			String iri = map.get(name).toString();
			if (properties.contentEquals("OutsideAirCloudCover")) {
				String newcloudcover = "" + Double.valueOf(cloudcover) / 100;// stored in decimal
				new WeatherAgent().updateRepoNewMethod(iri, properties, newcloudcover, completeformattime);// stored in
																											// decimal
			} else if (properties.contentEquals("OutsideAirPrecipitation")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, precipitation, completeformattime);// stored in
																											// decimal
			} else if (properties.contentEquals("OutsideAirPressure")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, pressure, completeformattime);
			} else if (properties.contentEquals("OutsideAirTemperature")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, temperature, completeformattime);
			} else if (properties.contentEquals("OutsideAirRelativeHumidity")) {
				String newhumidity = "" + Double.valueOf(relativehumidity) / 100; // stored in decimal
				new WeatherAgent().updateRepoNewMethod(iri, properties, newhumidity, completeformattime);
			} else if (properties.contentEquals("OutsideWindSpeed")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, windspeed, completeformattime);
			} else if (properties.contentEquals("OutsideWindDirection")) {
				new WeatherAgent().updateRepoNewMethod(iri, properties, winddirection, completeformattime);
			}

		}

	}

	public void executePeriodicUpdate(String cityIRI) throws URISyntaxException {
		String completeformat = WeatherAgent.provideCurrentTime();
		Map<String, String> stnmap = extractMappingname();
		if (cityIRI.toLowerCase().contains("singapore")) {
			String APIresult = "";
			try {
				APIresult = getWeatherDataFromAccuweatherAPI(cityIRI);
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
			String weatherPrecipitation = getWeatherDataFromGovAPI("/v1/environment/rainfall", null);
			JSONObject joPrecipitation = new JSONObject(weatherPrecipitation);// in mm
			String weatherTemperature = getWeatherDataFromGovAPI("/v1/environment/air-temperature", null);
			JSONObject joTemperature = new JSONObject(weatherTemperature);// in celcius
			String weatherhum = getWeatherDataFromGovAPI("/v1/environment/relative-humidity", null);
			JSONObject joRH = new JSONObject(weatherhum);// in percent
			String weatherwindspeed = getWeatherDataFromGovAPI("/v1/environment/wind-speed", null);
			JSONObject jowindspeed = new JSONObject(weatherwindspeed);// knots
			String weatherwinddir = getWeatherDataFromGovAPI("/v1/environment/wind-direction", null);
			JSONObject jowinddirection = new JSONObject(weatherwinddir);// degree

			updatePropertiesFromDataGov(stnmap, "OutsideAirTemperature", joTemperature, completeformat);
			updatePropertiesFromDataGov(stnmap, "OutsideWindDirection", jowinddirection, completeformat);
			updatePropertiesFromDataGov(stnmap, "OutsideWindSpeed", jowindspeed, completeformat);
			updatePropertiesFromDataGov(stnmap, "OutsideAirRelativeHumidity", joRH, completeformat);
			updatePropertiesFromDataGov(stnmap, "OutsideAirPrecipitation", joPrecipitation, completeformat);
			if (!APIresult.isEmpty()) {
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirCloudCover", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirPressure", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
				// in accuweather the json data is needed from the data gov to update each stn
				// written in data gov
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirTemperature", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideWindDirection", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideWindSpeed", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirRelativeHumidity", jowinddirection,
						completeformat, cityIRI, APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirPrecipitation", jowinddirection, completeformat,
						cityIRI, APIresult);// accu
			}

		} else if (cityIRI.toLowerCase().contains("kong")) {
			String APIresult = "";
			JSONObject jo = new JSONObject();
			JSONObject joPr = new JSONObject();

			try {
				String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/GetHKUWeatherLatestData",
						jo.toString());
				joPr = new JSONObject(result);
			} catch (JPSRuntimeException e) {
				APIresult = getWeatherDataFromAccuweatherAPI(cityIRI);
				joPr = new JSONObject(APIresult);
			} finally {
				updatePropertiesFromHKU(stnmap, "OutsideAirTemperature", joPr, completeformat);
				updatePropertiesFromHKU(stnmap, "OutsideAirRelativeHumidity", joPr, completeformat);
				updatePropertiesFromHKU(stnmap, "OutsideWindSpeed", joPr, completeformat);
				updatePropertiesFromHKU(stnmap, "OutsideWindDirection", joPr, completeformat);
				updatePropertiesFromHKU(stnmap, "OutsideAirPressure", joPr, completeformat);// in hPa exactly the
																							// same as millibar
			}
			if (!APIresult.isEmpty()) {
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirCloudCover", joPr, completeformat, cityIRI,
						APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirPrecipitation", joPr, completeformat, cityIRI,
						APIresult);// accu
			}

		} else {
			JSONObject joPr = new JSONObject();
			String APIresult = "";
			try {
				APIresult = getWeatherDataFromAccuweatherAPI(cityIRI);
			} catch (Exception e) {
				logger.error(e.getMessage());
			}
			if (!APIresult.isEmpty()) {
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirTemperature", joPr, completeformat, cityIRI,
						APIresult);
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirRelativeHumidity", joPr, completeformat, cityIRI,
						APIresult);
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideWindSpeed", joPr, completeformat, cityIRI,
						APIresult);
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideWindDirection", joPr, completeformat, cityIRI,
						APIresult);
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirPressure", joPr, completeformat, cityIRI,
						APIresult);// in hPa exactly the same as millibar
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirCloudCover", joPr, completeformat, cityIRI,
						APIresult);// accu
				updatePropertiesFromDataAccuWeather(stnmap, "OutsideAirPrecipitation", joPr, completeformat, cityIRI,
						APIresult);// accu
			}
		}
	}
	

	public void updatePropertiesFromHKU(Map<String, String> map, String propertyname, JSONObject data,
			String completeformattime) {// later need to be updated

		// input stn name:
		// output sequence index
		JSONArray stnCollection = new JSONArray();
		String name = "";
		String propertyValue = "";
		try {
			stnCollection = data.getJSONArray("HKweather");
			int size = stnCollection.length();
			for (int r = 0; r < size; r++) {
				name = stnCollection.getJSONObject(r).get("stnname").toString();

				propertyValue = stnCollection.getJSONObject(r).get(propertyname).toString();
				if (propertyname.contentEquals("OutsideAirRelativeHumidity")) {
					String newpropertyValue = "" + Double.valueOf(propertyValue) / 100; // stored in decimal
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else if (propertyname.contentEquals("OutsideWindSpeed")) {
					String newpropertyValue = "" + Double.valueOf(propertyValue) * 1000 / 3600; // stored in m/s
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else {
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, propertyValue,
							completeformattime);
				}

			}
		} catch (Exception e) {
			for (String stn : map.keySet()) {
				if (map.values().contains("hongkong")) {
					stnCollection.put(stn);
				}
			}
			for (int r = 0; r < stnCollection.length(); r++) {
				name = stnCollection.get(r).toString();
				if (propertyname.contentEquals("OutsideAirTemperature")) {
					propertyValue = data.getJSONObject("main").getString("temp");
					String newpropertyValue = "" + Double.valueOf(propertyValue);
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else if (propertyname.contentEquals("OutsideAirRelativeHumidity")) {
					propertyValue = data.getJSONObject("main").getString("humidity");
					String newpropertyValue = "" + Double.valueOf(propertyValue) / 100; // stored in decimal
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else if (propertyname.contentEquals("OutsideWindSpeed")) {
					propertyValue = data.getJSONObject("wind").getString("speed");
					String newpropertyValue = "" + Double.valueOf(propertyValue) * 1000 / 3600; // stored in m/s
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else if (propertyname.contentEquals("OutsideWindDirection")) {
					propertyValue = data.getJSONObject("wind").getString("deg");
					String newpropertyValue = "" + Double.valueOf(propertyValue);
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, newpropertyValue,
							completeformattime);
				} else {
					propertyValue = data.getJSONObject("main").getString("pressure");
					new WeatherAgent().updateRepoNewMethod(map.get(name).toString(), propertyname, propertyValue,
							completeformattime);
				}

			}
		}

	}

	public static void main(String[]args) { //used for upload all content locally

		RepositoryConnection con = repo.getConnection();
		String location="singapore";
//		String location="hong kong";
//		String location="berlin";
//		String location="the hague";
		WeatherAgent a=new WeatherAgent();
		String context="http://www.theworldavatar.com/kb/sgp/singapore/WeatherStation-015.owl#WeatherStation-015";
		a.resetRepoTrial(con,location,context,"015"); //currently the context is not used
		String cityiri= "http://dbpedia.org/resource/Singapore";
		String name="SGAccuWeather-001";
		List<String>info= new ArrayList<String>();
		info.add(cityiri);
		info.add(name);
		new WeatherAgent().insertDataRepoContext(info,context);
		String completeformat=WeatherAgent.provideCurrentTime();

			System.out.println("currenttime= "+ completeformat);		

		
	}
	
}
