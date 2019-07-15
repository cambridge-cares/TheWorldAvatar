package uk.ac.cam.cares.jps.servicespool;

import com.google.gson.Gson;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.building.BuildingQueryPerformer;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


@WebServlet("/ADMSAgent")
public class ADMSAgent extends JPSHttpServlet {
    private static final long serialVersionUID = 1L;
    private static final String PARAM_KEY_SHIP = "ship";
    private static final String DATA_KEY_COLLECTION = "collection";
    private static final String DATA_KEY_ITEMS = "items";
    private static final String DATA_KEY_LAT = "lat";
    private static final String DATA_KEY_LON = "lon";
    private JSONArray mCoordinates = new JSONArray();

    private void setLogger() {
        logger = LoggerFactory.getLogger(ADMSAgent.class);
    }

    @Override
    protected void doGetJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        setLogger();
        super.doGetJPS(request, response);
    }

    @Override
    protected void doPostJPS(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        setLogger();
        super.doPostJPS(request, response);
    }

    @Override
    protected void processRequestParameters() {
        JSONObject region = requestParams.getJSONObject("region");
        String cityIRI = requestParams.getString("city");
        JSONObject weather = requestParams.getJSONObject("weatherstate");

        String precipitation = weather.getJSONObject("hasprecipation").getString("hasintensity");
        double upperx = Double.parseDouble("" + region.getJSONObject("uppercorner").get("upperx"));
        double uppery = Double.parseDouble("" + region.getJSONObject("uppercorner").get("uppery"));
        double lowerx = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowerx"));
        double lowery = Double.parseDouble("" + region.getJSONObject("lowercorner").get("lowery"));
        // for default universal coordinate system that is use in the input browser
        String sourceCRSName = CRSTransformer.EPSG_3857;
        String targetCRSName = getTargetCRS(cityIRI);
        String dataPath = QueryBroker.getLocalDataPath();
        String fullPath = dataPath + "/JPS_ADMS"; // only applies for ship at the moment

        String newBuildingData = getBuildingData(cityIRI, upperx, uppery, lowerx, lowery);
        JSONObject newRegion = getNewRegionData(upperx, uppery, lowerx, lowery, targetCRSName, sourceCRSName);
        JSONObject bkgjson = requestParams.getJSONObject("weatherstate"); // temporary only to test 1/7,right
        // now it is hardcoded in python and
        // don't take any from the bkgjson

        try {
            if (requestParams.has(PARAM_KEY_SHIP)) {
                getEntityCoordinates(requestParams.getJSONObject(PARAM_KEY_SHIP));
                QueryBroker broker = new QueryBroker();
                broker.put(fullPath + "/arbitrary.txt", "text to assign something arbitrary");
                String coordinates = new Gson().toJson(mCoordinates.toString());

                writeAPLFileShip(newBuildingData, mCoordinates.toString(), newRegion, targetCRSName, fullPath, precipitation);
                writeMetFile(weather, fullPath);
                writeBkgFile(bkgjson, fullPath);

            } else {
                String plantIRI = getSourceData(requestParams, cityIRI);
                writeAPLFile(newBuildingData, plantIRI, newRegion, targetCRSName);
                fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
                writeMetFile(weather, fullPath);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        String agentIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";

        String timestamp = MetaDataAnnotator.getTimeInXsdTimeStampFormat(System.currentTimeMillis());

        startADMS(fullPath);
        MetaDataAnnotator.annotateWithTimeAndAgent(fullPath + "/test.levels.gst", timestamp, agentIRI);

        responseParams.put("folder", fullPath);
    }


    private String getTargetCRS(String cityIRI) {
        String targetCRSName;
        if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.THE_HAGUE_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857; //added currently 23/4
            targetCRSName = CRSTransformer.EPSG_28992;
        } else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.BERLIN_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_25833;
        } else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.SINGAPORE_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_3414;
        } else if (cityIRI.equalsIgnoreCase(BuildingQueryPerformer.HONG_KONG_IRI)) {
            //sourceCRSName = CRSTransformer.EPSG_3857;
            targetCRSName = CRSTransformer.EPSG_2326;
        } else {
            targetCRSName = CRSTransformer.EPSG_3857;
        }
        return targetCRSName;
    }

    private JSONObject getNewRegionData(double upperx, double uppery, double lowerx, double lowery,
                                        String targetCRSName, String sourceCRSName) {
        double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[]{lowerx, lowery});
        String lx = String.valueOf(p[0]);
        String ly = String.valueOf(p[1]);
        p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[]{upperx, uppery});
        String ux = String.valueOf(p[0]);
        String uy = String.valueOf(p[1]);

        String regionTemplate = "{\r\n" +
                "	\"uppercorner\":\r\n" +
                "    	{\r\n" +
                "        	\"upperx\" : \"%s\",\r\n" +
                "            \"uppery\" : \"%s\"      	\r\n" +
                "        },\r\n" +
                "          \r\n" +
                "     \"lowercorner\":\r\n" +
                "     {\r\n" +
                "       \"lowerx\" : \"%s\",\r\n" +
                "       \"lowery\" : \"%s\"\r\n" +
                "     }\r\n" +
                "}";


        JSONObject newRegion = new JSONObject(String.format(regionTemplate, ux, uy, lx, ly));
        return newRegion;
    }

    private String getBuildingData(String cityIRI, double upperx, double uppery, double lowerx, double lowery) {
        double[] sourceXY = null;

        if (cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Singapore") || cityIRI.equalsIgnoreCase("http://dbpedia.org/resource/Hong_Kong")) {
            sourceXY = new double[]{(lowerx + upperx) / 2, (lowery + uppery) / 2};
        } else {
            //sourceXY = getPlantXY(plantIRI); //CHANGE EASY TEMPORARILY TO SEE IF IT WORKS WITH NEW COORDINATE
            sourceXY = new double[]{(lowerx + upperx) / 2, (lowery + uppery) / 2};
        }

        String newBuildingData = retrieveBuildingDataInJSONOLD(cityIRI, sourceXY[0], sourceXY[1], lowerx, lowery, upperx, uppery);
        //String newBuildingData = retrieveBuildingDataInJSON(input);  //23/4 the new version that remove the duplicate query, but the composition must be changed first
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
     * Recursive method to extract latitude and longitude of an entity or collection of entities.
     * Stores coordinates in the class member variable mCoordinates.
     *
     * @param input An item or collection of items wrapped in JSONObject
     */
    private void getEntityCoordinates(JSONObject input) {

        if (input.has(DATA_KEY_LAT) & input.has(DATA_KEY_LON)) {
            JSONObject latlon = new JSONObject();
            latlon.put(DATA_KEY_LAT, input.getDouble(DATA_KEY_LAT));
            latlon.put(DATA_KEY_LON, input.getDouble(DATA_KEY_LON));
            mCoordinates.put(latlon);
        } else if (input.has(DATA_KEY_COLLECTION)) {
            JSONObject entities = input.getJSONObject(DATA_KEY_COLLECTION);
            if (entities.has(DATA_KEY_ITEMS)) {
                JSONArray items = entities.getJSONArray(DATA_KEY_ITEMS);
                for (Iterator<Object> i = items.iterator(); i.hasNext();) {
                    getEntityCoordinates((JSONObject) i.next());
                }
            }
        }
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

    public String writeAPLFile(String buildingInString, String plantIRI, JSONObject regionInJSON, String targetCRSName) {
        String fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";
        //system.out.println("==================== full path ====================");
        //system.out.println(fullPath);
        //system.out.println("===================================================");
        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add("admsTest.py");

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
        return result;
    }

    public String writeAPLFileShip(String buildingInString, String plantIRI, JSONObject regionInJSON, String targetCRSName, String fullPath, String precipitation) {

        //fullPath = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS";

        //system.out.println("==================== full path ====================");
        //system.out.println(fullPath);
        //system.out.println("===================================================");
        String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);

        ArrayList<String> args = new ArrayList<String>();
        args.add("python");
        args.add("admsTestShip.py");
        args.add(buildingInString.replace("\"", "'"));
        logger.info(buildingInString.replace("\"", "'"));

        args.add(regionInJSON.toString().replace("\"", "'")); //TODO ZXC: We should solve the encoding problem once for all
        logger.info(regionInJSON.toString().replace("\"", "'"));
// 		args.add(plantIRI.replace("\"", "'"));
        args.add(plantIRI);
// 		//system.out.println(plantIRI.replace("\"", "'"));
        logger.info(plantIRI);

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
        return result;
    }

    private String retrieveBuildingDataInJSONOLD(String city, double plantx, double planty, double lowerx, double lowery, double upperx, double uppery) {

        logger.info("retrieveBuildingDataInJSON, city=" + city + ", plantx=" + plantx + ", planty=" + planty
                + ", lowerx=" + lowerx + ", lowery=" + lowery + ", upperx=" + upperx + ", uppery=" + uppery);

        List<String> buildingIRIs = new BuildingQueryPerformer().performQueryClosestBuildingsFromRegion(city, plantx, planty, 25, lowerx, lowery, upperx, uppery);
        logger.info("building iris in ADMS Agent: " + buildingIRIs.toString());
        SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
        String argument = new Gson().toJson(result);
        return argument;
    }

    private String retrieveBuildingDataInJSON(JSONObject input) {

        String city = input.getString("city");
        int buildingnum = input.getJSONArray("building").length();
        List<String> buildingIRIs = new ArrayList<String>();
        for (int a = 0; a < buildingnum; a++) {
            buildingIRIs.add(input.getJSONArray("building").getString(a));
        }

        String buildinglist = String.valueOf(input.getJSONArray("building").length());
        System.out.println("what is building list??? " + buildinglist);
        System.out.println("element-0??? " + buildingIRIs.get(0));
        SimpleBuildingData result = new BuildingQueryPerformer().performQuerySimpleBuildingData(city, buildingIRIs);
        String argument = new Gson().toJson(result);
        return argument;
    }


    private void startADMS(String targetFolder) {
        String startADMSCommand = "\"C:\\\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
        CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
    }

}
