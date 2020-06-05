
package uk.ac.cam.cares.jps.dispersion.general;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.servlet.annotation.WebServlet;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MiscUtil;



@WebServlet("/ADMSAgent")
public class ADMSAgent extends DispersionModellingAgent {
	
    private static final long serialVersionUID = 1L;
    private static final String PARAM_KEY_SHIP = "ship";
    private static final String PARAM_KEY_PLANT = "plant";
    private static final String PARAM_KEY_CHIMNEY = "waste";
    private static final String DATA_KEY_COLLECTION = "collection";
    private static final String DATA_KEY_ITEMS = "items";
    private static final String DATA_KEY_LAT = "lat";
    private static final String DATA_KEY_LON = "lon";
    private static final String DATA_KEY_MMSI = "mmsi";
    private static final String FILENAME_ADMS_PROCESSOR = "adms_processor.py";
	public static final String BERLIN_IRI = "http://dbpedia.org/resource/Berlin";
	public static final String THE_HAGUE_IRI = "http://dbpedia.org/resource/The_Hague"; // The IRIs have be changed to /resource instead of /page
	public static final String SINGAPORE_IRI = "http://dbpedia.org/resource/Singapore";
	public static final String HONG_KONG_IRI = "http://dbpedia.org/resource/Hong_Kong";
    private String precipitationdata="0";
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(ADMSAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(ADMSAgent.class);

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams) {
    	logger.info("enter adms request parameter");
        JSONObject region = requestParams.getJSONObject("region");
        String cityIRI = requestParams.getString("city");
        
        JSONArray stnIRI=requestParams.getJSONArray("stationiri"); //ok
        List<String> stnList = MiscUtil.toList(stnIRI);
        //JSONArray buildingIRI=requestParams.getJSONArray("building");
    	logger.info("getting  source");
    	logger.info("getting the region,city, weather, and source");
    	
        double upperx = Double.parseDouble("" + region.getJSONObject("uppercorner").get("upperx"));
        double uppery = Double.parseDouble("" + region.getJSONObject("uppercorner").get("uppery"));
        double lowerx = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowerx"));
        double lowery = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowery"));
        // for default universal coordinate system that is use in the input browser
        String sourceCRSName = region.optString("srsname");
        logger.info("getting crs= "+sourceCRSName);
        
      if ((sourceCRSName == null) || sourceCRSName.isEmpty()) { //regarding the composition, it will need 4326, else, will be universal 3857 coordinate system
    	sourceCRSName = CRSTransformer.EPSG_4326; 
    }
        String targetCRSName = getTargetCRS(cityIRI);
        String dataPath = QueryBroker.getLocalDataPath();
        String fullPath = dataPath + "/JPS_ADMS"; // only applies for ship at the moment
        String newBuildingData = getBuildingData(region,cityIRI);
        
     
       
       
        JSONObject newRegion = getNewRegionData(upperx, uppery, lowerx, lowery, targetCRSName, sourceCRSName);
        
        JSONObject bkgjson = region; // temporary only to test 1/7,right
        // now it is hardcoded in python and
        // don't take any from the bkgjson
        
        
        try {
            if (requestParams.has(PARAM_KEY_SHIP)) {
                JSONArray coords  = getEntityCoordinates(requestParams.getJSONObject(PARAM_KEY_SHIP));
                QueryBroker broker = new QueryBroker();
                
                // write extra info to a file: 1. buildings, ships, coordinates
                
                JSONObject extra_info = new JSONObject();
                extra_info.put("buildings", newBuildingData);
                extra_info.put("coordinates", region);
                extra_info.put("ships", coords);
                QueryBroker extra_file_broker = new QueryBroker();
                extra_file_broker.putLocal(fullPath + "/extra_info.json",extra_info.toString());
                
                
                broker.putLocal(fullPath + "/arbitrary.txt", "text to assign something arbitrary");
                String coordinates = new Gson().toJson(coords.toString());
                createWeatherInput(fullPath,null,stnList);
                createEmissionInput(PARAM_KEY_SHIP, newBuildingData, coordinates, newRegion, targetCRSName, fullPath, precipitationdata);
                writeBkgFile(bkgjson, fullPath);

            } else {
                String plantIRI = getSourceData(requestParams, cityIRI);
                fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
                createWeatherInput(fullPath,null,stnList); //still cannot use this one at the moment
                createEmissionInput(PARAM_KEY_PLANT, newBuildingData, plantIRI, newRegion, targetCRSName);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        executeModel(fullPath);
        
        String target = fullPath + "/test.levels.gst";
        File name=new File(target);
        if(name.length()!=0&&name.exists()) {
        	String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
            //String timestamp = MetaDataAnnotator.getTimeInXsdTimeStampFormat(System.currentTimeMillis());
        	//MetaDataAnnotator.annotateWithTimeAndAgent(target, timestamp, agent);	
        	List<String> topics = new ArrayList<String>();
        	topics.add(cityIRI);
        	MetaDataAnnotator.annotate(target, null, agent, true, topics);
        }
        JSONObject responseParams = new JSONObject();

        responseParams.put("folder", fullPath);

        return responseParams;
    }

    @Override
	public void createWeatherInput(String dataPath, String filename,List<String>stniri) {	
    	System.out.println("going to create the weather input");
		//in here file name is not used as it is hard-coded in python
		logger.info("going to weather creation");
		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>"
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>"
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>"
				+ "PREFIX j6:<http://www.w3.org/2006/time#>" 
				+ "SELECT ?class ?propval ?proptimeval "
				+ "{ GRAPH <"+stniri.get(0)+"> "
				+ "{ "
				 
				+ "  ?entity j4:observes ?prop ." 
				+ " ?prop a ?class ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." 
				+ " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." 
				+ "}" 
				+ "}" 
				+ "ORDER BY DESC(?proptimeval)LIMIT 7";
		
		List<String[]> listmap = queryEndPointDataset(sensorinfo); //taken from dispersion modelling agent

		 System.out.println("size="+listmap.size());
		 logger.info("query is successful");
	        JSONObject weather = new JSONObject();
		    JSONObject cloudcover= new JSONObject();
		    JSONObject precipitation= new JSONObject();
		    JSONObject temperature= new JSONObject();
		    JSONObject wind= new JSONObject();
		    JSONObject relativehumidity= new JSONObject();
		    
	        for(int r=0;r<listmap.size();r++) {
	        	
	        		System.out.println("it goes number 1");
	        		if(listmap.get(r)[0].toLowerCase().contains("speed")) {
	        			wind.put("hasspeed", listmap.get(r)[1]);
		        	}else if(listmap.get(r)[0].toLowerCase().contains("direction")) {
		        		wind.put("hasdirection", listmap.get(r)[1]);
		        	}else if(listmap.get(r)[0].toLowerCase().contains("temperature")) {
		        		  temperature.put("hasvalue", listmap.get(r)[1]);
		        		  
		        	}else if(listmap.get(r)[0].toLowerCase().contains("humidity")) {
		        		String decimalhumidity=listmap.get(r)[1];
		        		double percent=Double.valueOf(decimalhumidity)*100;
		        		relativehumidity.put("hasvalue",""+percent);
		        		  
		        	}else if(listmap.get(r)[0].toLowerCase().contains("precipitation")) {
		        		precipitation.put("hasintensity", listmap.get(r)[1]);
		        		precipitationdata=listmap.get(r)[1];
		        	}else if(listmap.get(r)[0].toLowerCase().contains("cloud")) {
		        		cloudcover.put("hascloudcovervalue", listmap.get(r)[1]);
		        	
		        	}else if(listmap.get(r)[0].toLowerCase().contains("pressure")) {

		        	}
	        	
	        }
	        weather.put("haswind", wind);
	        weather.put("hasexteriortemperature", temperature);
	        weather.put("hashumidity", relativehumidity);
	        weather.put("hasprecipation", precipitation);
	        weather.put("hascloudcover", cloudcover);
	        
	        System.out.println("weather data= "+weather.toString());
	        writeMetFile(weather, dataPath);
	}

    private String getTargetCRS(String cityIRI) {
        String targetCRSName;
        if (cityIRI.equalsIgnoreCase(THE_HAGUE_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857; //added currently 23/4
            targetCRSName = CRSTransformer.EPSG_28992;
        } else if (cityIRI.equalsIgnoreCase(BERLIN_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_25833;
        } else if (cityIRI.equalsIgnoreCase(SINGAPORE_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_3414;
        } else if (cityIRI.equalsIgnoreCase(HONG_KONG_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_2326;
        } else {
            targetCRSName = CRSTransformer.EPSG_3857;
        }
        return targetCRSName;
    }

    private String getBuildingData(JSONObject region, String city) {
//        double[] sourceXY = null;

//        if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong")) {
//            sourceXY = new double[]{(lowerx + upperx) / 2, (lowery + uppery) / 2};
//        } else {
//            sourceXY = new double[]{(lowerx + upperx) / 2, (lowery + uppery) / 2};
//        }

        //String newBuildingData = retrieveBuildingDataInJSONOLD(cityIRI, sourceXY[0], sourceXY[1], lowerx, lowery, upperx, uppery);
        String newBuildingData = retrieveBuildingDataInJSON(region,city);  //23/4 the new version that remove the duplicate query, but the composition must be changed first
        newBuildingData = newBuildingData.replace('\"', '\'');
        return newBuildingData;
    }

    private String getSourceData(JSONObject input, String cityIRI) {
        String plantIRI = null;
        if (!(cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong"))) {
            plantIRI = input.getString("plant"); //
        }
        return plantIRI;
    }

    /**
     * Method to extract latitude and longitude of each entity in a collection of entities.
     *
     * @param input An item or collection of items wrapped in JSONObject
     * @return Coordinates in JSONArray
     */
    private JSONArray getEntityCoordinates(JSONObject input) {
        JSONArray coordinates = new JSONArray();

        if (input.has(DATA_KEY_COLLECTION)) {
            JSONObject entities = input.getJSONObject(DATA_KEY_COLLECTION);
            if (entities.has(DATA_KEY_ITEMS)) {
                JSONArray items = entities.getJSONArray(DATA_KEY_ITEMS);
                for (Iterator<Object> i = items.iterator(); i.hasNext();) {
                    JSONObject item = (JSONObject) i.next();
                    if (item.has(DATA_KEY_LAT) & item.has(DATA_KEY_LON)) {
                        JSONObject latlon = new JSONObject();
                        latlon.put(DATA_KEY_LAT, item.getDouble(DATA_KEY_LAT));
                        latlon.put(DATA_KEY_LON, item.getDouble(DATA_KEY_LON));
                        latlon.put(DATA_KEY_MMSI, item.get(DATA_KEY_MMSI));
                        coordinates.put(latlon);
                    }
                }
            }
        }

        return coordinates;
    }

    public void writeMetFile(JSONObject weatherInJSON, String fullPath) {

        // fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);

        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add("admsMetWriter.py");
        args.add(fullPath);
        // TODO-AE replacing " by $, maybe better by ' as is done in method writeAPLFile
        args.add(weatherInJSON.toString().replace("\"", "$"));

        CommandHelper.executeCommands(targetFolder, args);
    }

    public void writeBkgFile(JSONObject bkgInJSON, String fullPath) {

        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add("admsBgdWriter.py");
        args.add(fullPath);
        args.add(bkgInJSON.toString().replace("\"", "$"));

        CommandHelper.executeCommands(targetFolder, args);
    }

    @Override
    public void createEmissionInput(String entityType, String buildingInString, String plantIRI, JSONObject regionInJSON, String targetCRSName) {
        String fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
        //system.out.println("==================== full path ====================");
        //system.out.println(fullPath);
        //system.out.println("===================================================");
        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add(FILENAME_ADMS_PROCESSOR);

        args.add(entityType);
        args.add(buildingInString.replace("\"", "'"));
        logger.info(buildingInString.replace("\"", "'"));

        args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all
        logger.info(regionInJSON.toString().replace("\"", "'"));

        args.add(plantIRI.replace("\"", "'"));
        logger.info(plantIRI.replace("\"", "'"));

        args.add(fullPath);
        logger.info(fullPath);
        args.add(targetCRSName);
        // TODO-AE use PythonHelper instead of CommandHelper
        String result = CommandHelper.executeCommands(targetFolder, args);
        logger.info("ARGUMENTS");
        ////system.out.println(args.toString());
        logger.info(result);
    }

    @Override
    public void createEmissionInput(String entityType, String buildingInString, String sourceJSONFormatData, JSONObject regionInJSON, String targetCRSName, String fullPath, String precipitation) {

        //fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";

        //system.out.println("==================== full path ====================");
        //system.out.println(fullPath);
        //system.out.println("===================================================");
        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);

        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add(FILENAME_ADMS_PROCESSOR);
        args.add(entityType);

        args.add(buildingInString.replace("\"", "'"));
        logger.info(buildingInString.replace("\"", "'"));

        args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all
        logger.info(regionInJSON.toString().replace("\"", "'"));
// 		args.add(plantIRI.replace("\"", "'"));
        args.add(sourceJSONFormatData);
// 		//system.out.println(plantIRI.replace("\"", "'"));
        logger.info(sourceJSONFormatData);

        args.add(fullPath);
        logger.info(fullPath);

        args.add(targetCRSName);
        logger.info(targetCRSName);
        args.add(precipitation);
        // TODO-AE use PythonHelper instead of CommandHelper
        String result = CommandHelper.executeCommands(targetFolder, args);
        logger.info("ARGUMENTS");
        logger.info(args.toString());
        logger.info("APL FOR SHIP WRITTEN= " + result);
    }

    
//    private String retrieveBuildingDataInJSONOLD(String city, double plantx, double planty, double lowerx, double lowery, double upperx, double uppery) {
//
//        logger.info("retrieveBuildingDataInJSON, city=" + city + ", plantx=" + plantx + ", planty=" + planty
//                + ", lowerx=" + lowerx + ", lowery=" + lowery + ", upperx=" + upperx + ", uppery=" + uppery);
//
//        List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(city, plantx, planty, 25, lowerx, lowery, upperx, uppery);
//        //iri=plant x and y and scope origin is from 4326(if null) or 3857; target to 25833/28992/4326
//        logger.info("building iris in ADMS Agent: " + buildingIRIs.toString());
//        SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
//        String argument = new Gson().toJson(result);
//        return argument;
//    }

    private String retrieveBuildingDataInJSON(JSONObject region, String city) {
    	JSONObject req= new JSONObject();
    	req.put("region",region);
    	req.put("city",city);

//        int buildingnum = building.length();
//        List<String> buildingIRIs = new ArrayList<String>();
//        for (int a = 0; a < buildingnum; a++) {
//            buildingIRIs.add(building.getString(a));
//        }
//
//        String buildinglist = String.valueOf(building.length()); 
//        //iri=plant x and y and scope , origin is from 4326(if null) or 3857; converted to target = 28992 or 3857
//        System.out.println("what is building list??? " + buildinglist);
//        System.out.println("element-0??? " + buildingIRIs.get(0));
//        SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
//        String argument = new Gson().toJson(result);
//        return argument;
        
        String resultdata=execute("/JPS/BuildingsData", req.toString());
        return resultdata;
    }


    @Override
    public void executeModel(String targetFolder) {
        String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
        CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
    }

}
