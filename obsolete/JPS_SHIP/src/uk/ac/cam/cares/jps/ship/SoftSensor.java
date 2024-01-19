package uk.ac.cam.cares.jps.ship;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


@WebServlet("/SoftSensor")
public class SoftSensor extends JPSAgent {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String METADATA_START_RDF4J = "2019-12-10T01:57:26";
    private Logger logger = LoggerFactory.getLogger(SoftSensor.class);
    double minx = 0.0;
    double miny = 0.0;
    double minz = 0.0;
    double maxx = 0.0;
    double maxy = 0.0;
    double maxz = 0.0;
    int zamount = 0;

    public List<String> findtheclosest(List<String[]> simulationResult, double numberx, double numbery, double numberz) {
        int a = 2;
        List<Double> xgroup = new ArrayList<Double>();
        List<Double> ygroup = new ArrayList<Double>();
        List<Double> zgroup = new ArrayList<Double>();
        List<String> closest = new ArrayList<String>();
        minx = Double.valueOf(simulationResult.get(1)[4]);
        miny = Double.valueOf(simulationResult.get(1)[5]);
        //find the x array
        xgroup=addToXGroup(simulationResult,a,minx,xgroup);
        maxx = xgroup.get(xgroup.size() - 1);
        //find the overall point for y(as y shows different pattern)
        //find the yarray
        ygroup=addToYGroup(simulationResult, miny, ygroup);
        maxy = ygroup.get(ygroup.size() - 1);
        //find z array
        int arraysize = simulationResult.get(0).length;
        List<Double> lstZGroup=constructZGroup( arraysize,  simulationResult,  zgroup);
        zgroup = new ArrayList<>(new LinkedHashSet<>(lstZGroup));
        minz = zgroup.get(0);
        maxz = zgroup.get(zgroup.size() - 1);
        zamount = zgroup.size();
        closest=constructClosest(numberx, numbery,  numberz, xgroup,  ygroup, zgroup,
                                  minx, maxx, miny,  maxy,  minz,  maxz, closest);
        return closest;
    }

    private List<Double> addToXGroup(List<String[]>  simulationResult,int a,double minx, List<Double> xgroup){
        while (Double.valueOf(simulationResult.get(a)[4]) - minx != 0) {
            double x = Double.valueOf(simulationResult.get(a)[4]);
            xgroup.add(x);
            a++;
        }
        return xgroup;
    }

    private List<Double> addToYGroup(List<String[]>simulationResult,double miny, List<Double> ygroup){
        int b = 2;
        while (Double.valueOf(simulationResult.get(b)[5]) - miny == 0) {
            b++;
        }
        //find y array
        for (int anumber = 1; anumber < simulationResult.size(); anumber += b) {
            ygroup.add(Double.valueOf(simulationResult.get(anumber)[5]));
        }
        return ygroup;
    }

    private List<Double> constructZGroup(int arraysize, List<String[]> simulationResult, List<Double> zgroup){
        for (int d = 0; d < arraysize; d++) {
            if (simulationResult.get(0)[d].contains("Z=")) {
                String zvalue = simulationResult.get(0)[d].split("Z=")[1].split("m")[0];
                //System.out.println(zvalue);
                zgroup.add(Double.valueOf(zvalue));
            }
        }
        return zgroup;
    }

    private List<String> constructClosest(double numberx, double numbery, double numberz, List<Double> xgroup, List<Double> ygroup,List<Double> zgroup,
                                  double minx, double maxx, double miny, double maxy, double minz, double maxz,List<String> closest){

        int xch = closestIndex(numberx, xgroup);
        int ych = closestIndex(numbery, ygroup);
        int zch = closestIndex(numberz, zgroup);

        if ((numberx < minx || numberx > maxx) || (numbery < miny || numbery > maxy) || (numberz < minz || numberz > maxz)) {
            closest.add("-999");
            closest.add("-999");
            closest.add("-999");
        } else {
            closest.add("" + xgroup.get(xch));
            closest.add("" + ygroup.get(ych));
            closest.add("" + zgroup.get(zch));
        }
        return closest;
    }

    public int closestIndex(double number, List<Double> group) {
        double comparatory = Math.abs(number - group.get(0));
        int ych = 0;
        for (int r = 1; r < group.size(); r++) {
            if (Math.abs(number - group.get(r)) < comparatory) {
                comparatory = Math.abs(number - group.get(r));
                ych = r;
            }
        }
        return ych;
    }

    public List<String> findtheconcentration(List<String[]> simulationResult, double datanumberx, double datanumbery, double datanumberz) {
        List<String> conc = new ArrayList<String>();
        int sizerow = simulationResult.size();
        int selectedindex = 1;
        for (int ind = 0; ind < sizerow; ind++) {
            if (simulationResult.get(ind)[4].contains(String.valueOf(datanumberx)) && simulationResult.get(ind)[5].contains(String.valueOf(datanumbery))) {
                selectedindex = ind;
            }
        }
        if (datanumberx != -999.0 && datanumbery != -999.0 && datanumberz != -999.0) {
            for (int ind2 = 0; ind2 < simulationResult.get(0).length; ind2++) {
                if (simulationResult.get(0)[ind2].contains("Z=")) {
                    if (Double.valueOf(simulationResult.get(0)[ind2].split("Z=")[1].split("m")[0]) - datanumberz == 0.0) {
                        conc.add(simulationResult.get(0)[ind2]);
                        conc.add(simulationResult.get(selectedindex)[ind2]);
                    }
                }
            }
        } else {//find out the index that is the starting point of concentration of poll
            int k = 0;
            for (int ind2 = 0; ind2 < simulationResult.get(0).length; ind2++) {
                if (!simulationResult.get(0)[ind2].contains("Z=")) {
                    k++;
                }
            }//put the empty result to the array
            int j = (simulationResult.get(0).length - k) / zamount;
            for (int i = 0; i < j; i++) {
                conc.add(simulationResult.get(0)[i + k]);
                conc.add("unknown");
            }
        }//System.out.println("concentration size= "+conc.size());
        return conc;
    }


    private List<String[]> getMetadata(MediaType mediaType, String fromCreationTime, String toCreationTime,
                                       String iriCreatingAgent, String fromSimulationTime, String toSimulationTime, String iriScenario, List<String> topics) {
        String metadataResult;
        List<String[]> listmap;
        try {
            DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            Date fromRdf4J = formatter.parse(METADATA_START_RDF4J);
            Date from = formatter.parse(fromSimulationTime);
            Date to = formatter.parse(toSimulationTime);

            if (from.before(fromRdf4J) || to.before(fromRdf4J)) {
                metadataResult = MetaDataQuery.queryOldResources(iriCreatingAgent,fromSimulationTime, toSimulationTime,topics);
            } else {
                metadataResult = MetaDataQuery.queryResources(null, null, null, iriCreatingAgent,  fromSimulationTime, toSimulationTime, null, topics);
            }
            String[] keys = JenaResultSetFormatter.getKeys(metadataResult);
            listmap = JenaResultSetFormatter.convertToListofStringArrays(metadataResult, keys);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
        return listmap;
    }

  /*  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String jsonInput = AgentCaller.readJsonParameter(request).toString();
        JSONObject result = new JSONObject(jsonInput);
        JSONArray coordinatelist = result.getJSONArray("coordinates");
        String timefrom = result.getJSONObject("timeinterval").optString("from", "none");
        String timeto = result.getJSONObject("timeinterval").optString("to", "none");
        String agentiri = result.optString("agent", null);
        String city = result.optString("cityname", "empty");
        //String resultfromfuseki = MetaDataQuery.queryResources(agentiri, timefrom, timeto);
        List<String> topics = new ArrayList<String>();
        if(city.toLowerCase().contains("kong")) {
        	topics.add("http://dbpedia.org/resource/Hong_Kong");
        }else if(city.toLowerCase().contains("singapore")) {
        	topics.add("http://dbpedia.org/resource/Singapore");
        }
        List<String[]> listmap = getMetadata(null, null, null, agentiri, timefrom, timeto, null, topics);

        List<String[]> propercsv = new ArrayList<String[]>();
        String[] header = {"time", "x", "y", "z", "crs", "pollutant", "observes", "value", "unit"};
        propercsv.add(header);
        logger.info("size= " + listmap.size());
        for (int v = 0; v < listmap.size(); v++) {
            //System.out.println("agent involved= "+listmap.get(v)[3]);
            File name = new File(listmap.get(v)[0]);
            //System.out.println("name= "+listmap.get(v)[0]);


            if (name.exists() && name.length() != 0) {
                String csv = new QueryBroker().readFileLocal(listmap.get(v)[0]);
                List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);

                for (int v2 = 0; v2 < coordinatelist.length(); v2++) {
                    double x = coordinatelist.getJSONObject(v2).getDouble("x");
                    double y = coordinatelist.getJSONObject(v2).getDouble("y");
                    double z = coordinatelist.getJSONObject(v2).getDouble("z");
                    double realx = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(0));
                    double realy = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(1));
                    double realz = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(2));

                    if( realx!=-999.0&&realy!=-999.0&&realz!=-999.0) {
                        logger.info("realx= " + realx);
                        logger.info("realy= " + realy);
                        logger.info("realz= " + realz);
//                        logger.info("directory selected by query= " + listmap.get(v)[0]);
//                        System.out.println("directory selected by query= " + listmap.get(v)[0]);
//						String plantupdate2 = "PREFIX dcterms:<http://purl.org/dc/terms/> "
//								+ "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> "
//								+ "PREFIX j1:<https://www.w3.org/2006/time#> " 
//								+ "INSERT {<" + listmap.get(v)[0]
//								+ "> dcterms:subject <http://dbpedia.org/resource/Hong_Kong>" + " .} "
//								+ "WHERE { " + "}";
//                		UpdateProcessor upp = UpdateExecutionFactory.createRemote(UpdateFactory.create(plantupdate2),
//                				"http://www.theworldavatar.com:80/damecoolquestion/jpsmetadata/update");
//                			upp.execute();
//                        
                    }

                    List<String> concentration = findtheconcentration(simulationResult, realx, realy, realz);

                    String timeinst = listmap.get(v)[4];
                    double sumpm10 = 0;
                    double sumpm25 = 0;
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

                        //logger.info("content[7]= "+content[7]);
                        if (content[5].toLowerCase().contains("pm2.5")) {
                            if (content[7].contains("unknown")) {
                                content[7] = "0";
                            }
                            sumpm25 = sumpm25 + Double.valueOf(content[7]);
                        } else if (content[5].toLowerCase().contains("pm10")) {
                            if (content[7].contains("unknown")) {
                                content[7] = "0";
                            }
                            sumpm10 = sumpm10 + Double.valueOf(content[7]);
                        } else {
                            propercsv.add(content);
                        }
                    }
                    String content[] = new String[9];
                    content[0] = timeinst;
                    content[1] = "" + x;
                    content[2] = "" + y;
                    content[3] = "" + z;
                    content[4] = "EPSG:2326";
                    content[5] = "PM10"; // later need to be mapped to iri
                    content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
                    content[7] = String.valueOf(sumpm10 + sumpm25);
                    content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
                    propercsv.add(content);
                    String content2[] = new String[9];
                    content2[0] = timeinst;
                    content2[1] = "" + x;
                    content2[2] = "" + y;
                    content2[3] = "" + z;
                    content2[4] = "EPSG:2326";
                    content2[5] = "PM2.5"; // later need to be mapped to iri
                    content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
                    content2[7] = "" + sumpm25;
                    content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
                    propercsv.add(content2);
                }
            }
        }


        System.out.println("size= " + propercsv.size());
        String arrayinstring = null;
        for (int n = 0; n < propercsv.size(); n++) {
            //System.out.println(convertArrayToStringMethod(propercsv.get(n)));
            arrayinstring = arrayinstring + convertArrayToStringMethod(propercsv.get(n)) + "\n";
        }

        String[] headertype = {"xsd:dateTime", "xsd:number", "xsd:number", "xsd:number", "literal", "literal", "uri", "xsd:number", "uri"};

        //System.out.println("result csv format= "+arrayinstring);

        //System.out.println("result json format= "+new JenaResultSetFormatter().createJSONfromCSV(propercsv,headertype));
        JSONObject dataSet = new JSONObject(new JenaResultSetFormatter().createJSONfromCSV(propercsv, headertype));
        AgentCaller.writeJsonParameter(response, dataSet);

    }
    */

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams){
        if(validateInput(requestParams)){
            JSONArray coordinatelist = requestParams.getJSONArray("coordinates");
            String timefrom = requestParams.getJSONObject("timeinterval").optString("from", "none");
            String timeto = requestParams.getJSONObject("timeinterval").optString("to", "none");
            String agentiri = requestParams.optString("agent", null);
            String city = requestParams.optString("cityname", "empty");
            List<String> topics = new ArrayList<String>();
            if(city.toLowerCase().contains("kong")) {
                topics.add("http://dbpedia.org/resource/Hong_Kong");
            }else if(city.toLowerCase().contains("singapore")) {
                topics.add("http://dbpedia.org/resource/Singapore");
            }
            List<String[]> listmap = getMetadata(null, null, null, agentiri, timefrom, timeto, null, topics);
            List<String[]> propercsv = new ArrayList<String[]>();
            String[] header = {"time", "x", "y", "z", "crs", "pollutant", "observes", "value", "unit"};
            propercsv.add(header);
            logger.info("size= " + listmap.size());
            for (int v = 0; v < listmap.size(); v++) {
                File name = new File(listmap.get(v)[0]);
                if (name.exists() && name.length() != 0) {
                    String csv = new QueryBroker().readFileLocal(listmap.get(v)[0]);
                    List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
                    propercsv=addConcentrationToCSV(v,coordinatelist,simulationResult,listmap,propercsv);
                }
            }
            JSONObject dataSet=createDataSet(propercsv);
            requestParams=dataSet;
        }
        return  requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean validate=true;
        if (requestParams.isEmpty()) {
            throw new BadRequestException("RequestParam is empty.");
        }else if(!checkAgent(requestParams)) {
            throw new BadRequestException("In the requestParam object either the key:agent is missing or is null or is empty.");
        }else if(!checkCity(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:city is missing or is null or is empty.");
        }else if(!checkTime(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:timeinterval is missing or is null or is empty or similar problems are present" +
                    "for key:to and/or key:from found within the key:timeinterval object.");
        }else if(!checkCoordinateList(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:coordinates is missing or is null or is empty.");
        }else{
            if (!checkX(requestParams)) {
                throw new BadRequestException("In the coordinates array the key:x is either not present, null or empty.");
            }
            if (!checkY(requestParams)) {
                throw new BadRequestException("In the coordinates array the key:y is either not present, null or empty.");
            }
            if (!checkZ(requestParams)) {
                throw new BadRequestException("In the coordinates array the key:z is either not present, null or empty.");
            }
        }
        return validate;
    }

    private boolean checkAgent(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("agent") || requestParams.isNull("agent")){
            validate=false;
        }
        if(validate){
            String agent=requestParams.getString("agent");
            if(agent.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkCity(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("cityname") || requestParams.isNull("cityname")){
            validate=false;
        }
        if(validate){
            String agent=requestParams.getString("cityname");
            if(agent.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkTime(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("timeinterval") || requestParams.isNull("timeinterval")){
            validate=false;
        }
        if(validate){
            JSONObject time=requestParams.getJSONObject("timeinterval");
            if(time.isEmpty())
                validate=false;
            else{
                return checkToAndFrom(requestParams);
            }
        }
        return validate;
    }

    private boolean checkToAndFrom(JSONObject requestParams){
        boolean validate = true;
        JSONObject timeInterval=requestParams.getJSONObject("timeinterval");
        if(!timeInterval.has("to") || !timeInterval.has("from") || timeInterval.isNull("to") || timeInterval.isNull("from")){
            validate=false;
        }
        if(validate){
            String to= timeInterval.optString("to");
            String from=timeInterval.optString("from");
            if( to.isEmpty() || from.isEmpty()){
                validate=false;
            }
        }
        return validate;
    }

    private boolean checkCoordinateList(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("coordinates") || requestParams.isNull("coordinates")){
            validate=false;
        }
        if(validate){
            JSONArray coordinates=requestParams.getJSONArray("coordinates");
            for (int i=0; i<coordinates.length();i++) {
                if(coordinates.getJSONObject(i).isEmpty()) {
                    validate = false;
                    break;
                }
            }
        }
        return validate;
    }

    private boolean checkX(JSONObject requestParams){
        boolean validate=true;
        JSONArray coordinatesList=requestParams.getJSONArray("coordinates");
        for (int i=0; i<coordinatesList.length();i++){
            if(!coordinatesList.getJSONObject(i).has("x") || coordinatesList.getJSONObject(i).isNull("x")){
                validate=false;
                break;
            }
            if(validate){
                String x=  coordinatesList.getJSONObject(i).get("x").toString();
                if(x.isEmpty()){
                    validate=false;
                    break;
                }
            }
        }
        return validate;
    }

    private boolean checkY(JSONObject requestParams){
        boolean validate=true;
        JSONArray coordinatesList=requestParams.getJSONArray("coordinates");
        for (int i=0; i<coordinatesList.length();i++){
            if(!coordinatesList.getJSONObject(i).has("y") || coordinatesList.getJSONObject(i).isNull("y")){
                validate=false;
                break;
            }
            if(validate){
                String y=  coordinatesList.getJSONObject(i).get("y").toString();
                if(y.isEmpty()){
                    validate=false;
                    break;
                }
            }
        }
        return validate;
    }

    private boolean checkZ(JSONObject requestParams){
        boolean validate=true;
        JSONArray coordinatesList=requestParams.getJSONArray("coordinates");
        for (int i=0; i<coordinatesList.length();i++){
            if(!coordinatesList.getJSONObject(i).has("z") || coordinatesList.getJSONObject(i).isNull("z")){
                validate=false;
                break;
            }
            if(validate){
                String z=  coordinatesList.getJSONObject(i).get("z").toString();
                if(z.isEmpty()){
                    validate=false;
                    break;
                }
            }
        }
        return validate;
    }

    private List<String[]> addConcentrationToCSV(int v,JSONArray coordinatelist,List<String[]> simulationResult, List<String[]> listmap,List<String[]> propercsv){
        for (int v2 = 0; v2 < coordinatelist.length(); v2++) {
            double x = coordinatelist.getJSONObject(v2).getDouble("x");
            double y = coordinatelist.getJSONObject(v2).getDouble("y");
            double z = coordinatelist.getJSONObject(v2).getDouble("z");
            double realx = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(0));
            double realy = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(1));
            double realz = Double.valueOf(findtheclosest(simulationResult, x, y, z).get(2));
            if( realx!=-999.0&&realy!=-999.0&&realz!=-999.0) {
                logger.info("realx= " + realx);
                logger.info("realy= " + realy);
                logger.info("realz= " + realz);
            }
            List<String> concentration = findtheconcentration(simulationResult, realx, realy, realz);
            String timeinst = listmap.get(v)[4];
            propercsv=addContentToCSV(x,y,z, timeinst, concentration, propercsv);
        }
        return propercsv;
    }

    private List<String[]> addContentToCSV(double x, double y, double z, String timeinst, List<String> concentration, List<String[]> propercsv){
        double sumpm10 = 0;
        double sumpm25 = 0;
        for (int r = 0; r < concentration.size(); r += 2) {
            String content []=constructContent(timeinst,x,y,z,concentration,r);
            //logger.info("content[7]= "+content[7]);
            if (content[5].toLowerCase().contains("pm2.5")) {
                if (content[7].contains("unknown")) {
                    content[7] = "0";
                }
                sumpm25 = sumpm25 + Double.valueOf(content[7]);
            } else if (content[5].toLowerCase().contains("pm10")) {
                if (content[7].contains("unknown")) {
                    content[7] = "0";
                }
                sumpm10 = sumpm10 + Double.valueOf(content[7]);
            } else {
                propercsv.add(content);
            }
        }
        String content []= constructContentUpdated(timeinst,x,y,z,sumpm10,sumpm25);
        String content2 []= constructContent2(timeinst,x,y,z,sumpm25);
        propercsv.add(content);
        propercsv.add(content2);
        return  propercsv;
    }

    private String [] constructContent(String timeinst,double x, double y, double z, List<String> concentration,int r){
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
        return content;
    }

    private String [] constructContentUpdated(String timeinst,double x,double y,double z, double sumpm10, double sumpm25){
        String content[] = new String[9];
        content[0] = timeinst;
        content[1] = "" + x;
        content[2] = "" + y;
        content[3] = "" + z;
        content[4] = "EPSG:2326";
        content[5] = "PM10"; // later need to be mapped to iri
        content[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content[7] = String.valueOf(sumpm10 + sumpm25);
        content[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
        return content;
    }

    private String [] constructContent2(String timeinst,double x,double y,double z, double sumpm25){
        String content2[] = new String[9];
        content2[0] = timeinst;
        content2[1] = "" + x;
        content2[2] = "" + y;
        content2[3] = "" + z;
        content2[4] = "EPSG:2326";
        content2[5] = "PM2.5"; // later need to be mapped to iri
        content2[6] = "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#MassConcentration";
        content2[7] = "" + sumpm25;
        content2[8] = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m";
        return content2;
    }

    private JSONObject createDataSet(List<String[]> propercsv){
        System.out.println("size= " + propercsv.size());
        String arrayinstring = null;
        for (int n = 0; n < propercsv.size(); n++) {
            arrayinstring = arrayinstring + convertArrayToStringMethod(propercsv.get(n)) + "\n";
        }
        String[] headertype = {"xsd:dateTime", "xsd:number", "xsd:number", "xsd:number", "literal", "literal", "uri", "xsd:number", "uri"};
        JSONObject dataSet = new JSONObject(new JenaResultSetFormatter().createJSONfromCSV(propercsv, headertype));
        return dataSet;
    }

    public static String convertArrayToStringMethod(String[] strArray) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(strArray[0]);
        for (int i = 1; i < strArray.length; i++) {
            stringBuilder.append(",");
            stringBuilder.append(strArray[i]);
        }
        return stringBuilder.toString();
    }


}
