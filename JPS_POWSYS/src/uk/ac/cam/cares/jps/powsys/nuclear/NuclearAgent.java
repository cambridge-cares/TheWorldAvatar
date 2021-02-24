package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.io.FileUtils;
import org.apache.http.client.methods.HttpPost;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.util.Util;

@WebServlet(urlPatterns = {"/NuclearAgent/startsimulation", "/NuclearAgent/processresult"})
public class NuclearAgent extends JPSAgent {

    public static final String SIM_START_PATH = "/NuclearAgent/startsimulation";
    public static final String SIM_PROCESS_PATH = "/NuclearAgent/processresult";
    public static final String KEY_WATCH = "watch";
    public static final String KEY_CALLBACK_URL = "callback";
    private static final long serialVersionUID = -4199209974912271432L;
    public static final String AGENT_TAG = "GAMS_NuclearAgent";
    //private String modelname="Parallel_wrld_location.gms";
    private String modelname="Location_5hr_solveropt.gms";
    Logger logger = LoggerFactory.getLogger(NuclearAgent.class);
    
    public void runGAMSAsync(String baseUrl) throws IOException, InterruptedException {
    	modifyTemplate(baseUrl, modelname);
        String executablelocation = Util.getGAMSLocation();
        String folderlocation = baseUrl;
        String content=executablelocation+" "+baseUrl+"/"+modelname+",WDIR="+folderlocation+",PROCDIR="+folderlocation+",CURDIR="+folderlocation+",SCRDIR="+folderlocation+" LO=2";
        
//      CommandHelper.executeSingleCommand(baseUrl, cmdArrayinstring);
        Process p = Runtime.getRuntime().exec(content); //used w/o waitFor() to be async
        System.out.println("Done");
    }

	public void modifyTemplate(String newdir, String filename) throws IOException { 
		String destinationUrl = newdir + "/"+filename;
		String indicator = KeyValueManager.get(IKeys.LONG_NUCLEAR_GAMS);
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/res/"+filename);
		if(indicator.contentEquals("true")) {
			file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/res/ori/"+filename);
			logger.info("it's running for the 30 hr!!!");
		}
		else {
			logger.info("running for the 4 hr!");
		}

		
        String fileContext = FileUtils.readFileToString(file);
        fileContext = fileContext.replaceAll("parameters_req_existing.gdx",newdir+"/parameters_req_existing.gdx");
        fileContext = fileContext.replaceAll("parameters_req.gdx",newdir+"/parameters_req.gdx");
        fileContext = fileContext.replaceAll("constants_req.gdx",newdir+"/constants_req.gdx");
        fileContext = fileContext.replaceAll("inputlandlots.gdx",newdir+"/inputlandlots.gdx");
        fileContext = fileContext.replaceAll("inputloadpoints.gdx",newdir+"/inputloadpoints.gdx");
        
        fileContext = fileContext.replaceAll("parameters_req_existing.csv",newdir+"/parameters_req_existing.csv output="+newdir+"/parameters_req_existing.gdx"); 
        fileContext = fileContext.replaceAll("parameters_req.csv",newdir+"/parameters_req.csv output="+newdir+"/parameters_req.gdx");
        fileContext = fileContext.replaceAll("constants_req.csv",newdir+"/constants_req.csv output="+newdir+"/constants_req.gdx");
        fileContext = fileContext.replaceAll("inputlandlots.csv",newdir+"/inputlandlots.csv output="+newdir+"/inputlandlots.gdx");
        fileContext = fileContext.replaceAll("inputloadpoints.csv",newdir+"/inputloadpoints.csv output="+newdir+"/inputloadpoints.gdx");
       
		
		new QueryBroker().putLocal(destinationUrl, fileContext);
	}


    public void runGAMS(String baseUrl) throws IOException, InterruptedException { // need gdx files to be in directory location

        modifyTemplate(baseUrl, modelname);

        logger.info("Start");
        //logger.info("separator= "+File.separator);
        String executablelocation = Util.getGAMSLocation(); //depends where is in claudius
        String folderlocation =baseUrl;
        //String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/parallelworld/";
        String[] cmdArray = new String[5];

        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "/" + modelname;
        cmdArray[2] = "WDIR=" + folderlocation;
        cmdArray[3] = "SCRDIR=" + folderlocation;
        cmdArray[4] = "LO=2";

        String cmdArrayinstring = cmdArray[0] + " " + cmdArray[1] + "," + cmdArray[2] + "," + cmdArray[3] + " " + cmdArray[4];

        logger.info(cmdArrayinstring);
      //TODO: Disable this until we get a waitFor run time
//      CommandHelper.executeSingleCommand(baseUrl, cmdArrayinstring);
        Process p = Runtime.getRuntime().exec(cmdArray);
        p.waitFor();

        logger.info("Done");
    }

    private void pseudoRunGAMS(String baseUrl) {
        logger.info("RIGHT NOW IT IS STARTING PSEUDOGAMS NUCLEAR");
        String scenarioUrl = BucketHelper.getScenarioUrl();
        String usecaseUrl = BucketHelper.getUsecaseUrl();
        logger.info("starting GAMS for simulation for scenarioUrl = " + scenarioUrl + ", usecaseUrl = " + usecaseUrl);
        logger.info("GAMS started successfully, post processing of GAMS results by callback");
        String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/res" + "/results.csv";
        File file = new File(source);
        String destinationUrl = baseUrl + "/" + "/results.csv";
        new QueryBroker().putLocal(destinationUrl, file);
    }

    private void notifyWatcher(JSONObject agentArgs, String filePath, String callbackIRI) {
        agentArgs.put(KEY_WATCH, filePath);
        agentArgs.put(KEY_CALLBACK_URL, callbackIRI);
        execute(KeyValueMap.getInstance().get("url.jps_aws"), agentArgs.toString(), HttpPost.METHOD_NAME);
    }

    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(NuclearAgent.class);
    }
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
    	if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);
	        String lotiri = requestParams.getString("landlot");	        
	        boolean v = InputValidator.checkIfValidIRI(lotiri);
	        JSONArray ja =  requestParams.getJSONArray("substitutionalgenerators");
	        List<String> subGenerators = MiscUtil.toList(ja);
			if (ja.length()!= 0) {
				for (int i = 0; i< subGenerators.size(); i++) {
					if (subGenerators.get(i)!= null) {
						boolean t = InputValidator.checkIfValidIRI(subGenerators.get(i));
						if (t == false) {
							return false;
						}
					}
				}
			}else {
				return false;
			}
	        return w&v;
        } catch (JSONException ex) {
        	ex.printStackTrace();
        }
        return false;
    }
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

        String path = requestParams.getString("scenarioagentoperation");

        System.out.println("path= " + path);

        if (path.contains(SIM_START_PATH)) {


            try {
            	if (!validateInput(requestParams)) {
            		throw new JSONException("Nuclear Agent input parameters invalid");
            	};
                String lotiri = requestParams.getString("landlot");
                String iriofnetwork = requestParams.getString("electricalnetwork");

                JSONArray ja = requestParams.getJSONArray("substitutionalgenerators");
    			List<String> listofplant = MiscUtil.toList(ja);
                boolean runGams = true;
                if (!requestParams.isNull(JPSConstants.RUN_SIMULATION)) {
                    runGams = requestParams.getBoolean(JPSConstants.RUN_SIMULATION);
                }

                String dataPath = requestParams.optString("baseUrl", QueryBroker.getLocalDataPath());
                startSimulation(lotiri, iriofnetwork, listofplant, dataPath, runGams);
                notifyWatcher(requestParams, dataPath + "/" + AGENT_TAG + "/" + NuclearKBCreator.GAMS_OUTPUT_FILENAME,
                		path.replace(SIM_START_PATH, SIM_PROCESS_PATH));

            } catch (Exception e) {
                logger.error(e.getMessage(), e);
                throw new JPSRuntimeException(e.getMessage(), e);
            }

        } else {//is the process result still used???

            try {
                JSONObject jo = requestParams;
                String scenarioUrl = BucketHelper.getScenarioUrl();
                String usecaseUrl = BucketHelper.getUsecaseUrl();
                logger.info("processing result of GAMS simulation for scenarioUrl = " + scenarioUrl + ", usecaseUrl = " + usecaseUrl);


                String dataPath = QueryBroker.getLocalDataPath();
                List<String> plants = processSimulationResult(dataPath);
                JSONArray plantsja = new JSONArray(plants);
                jo.put("plants", plantsja);

                // TODO-AE SC 20190913 replace hard-coded call back to coordination agent
                AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/processresult", jo.toString()); //no need to call back the coordination and retrofit agent

                // no need to return jo or plants here; this is just for asserting the simulation result in a junit test
                return jo;

            } catch (NumberFormatException | URISyntaxException | IOException e) {
                logger.error(e.getMessage(), e);
                throw new JPSRuntimeException(e.getMessage(), e);
            }
        }
        return requestParams;
    }

    public void startSimulation(String lotiri, String iriofnetwork,List<String> plantlist, String dataPath, boolean runGams) throws IOException, InterruptedException {

        String baseUrl = dataPath + "/" + AGENT_TAG;
        System.out.println("go here datapath= "+baseUrl);
        if (logger == null) {
        	logger = LoggerFactory.getLogger(NuclearAgent.class);
        }
        logger.info("starting simulation for local path =" + baseUrl);

        QueryBroker broker = new QueryBroker();

        prepareCSVLandlot(lotiri, baseUrl); //used to create csv

        //-----------------------------------------1st input file finished-------------------------------------------------------------------

        prepareCSVLoad(iriofnetwork, baseUrl); //used to create csv

        //-----------------------------------------2nd input file finished-------------------------------------------------------------------		

        String resourceDir = Util.getResourceDir(this);
        File file = new File(resourceDir + "/constants_req.csv");
        broker.putLocal(baseUrl + "/constants_req.csv", file);

        //-----------------------------------------3rd input file finished-------------------------------------------------------------------

        file = new File(resourceDir + "/parameters_req.csv");
        broker.putLocal(baseUrl + "/parameters_req.csv", file);

        //-----------------------------------------4th input file finished-------------------------------------------------------------------

        prepareCSVPartialRemaining(plantlist, iriofnetwork, baseUrl);
        //-----------------------------------------5th input file finished-------------------------------------------------------------------
        file = new File(resourceDir + "/baron.opt");
        broker.putLocal(baseUrl + "/baron.opt", file);
        
        if (runGams) {
        	//runGAMS(baseUrl);
        	runGAMSAsync(baseUrl);
        }
        else {
        pseudoRunGAMS(baseUrl);
        }
    }

    public List<String> processSimulationResult(String dataPath) throws NumberFormatException, IOException, URISyntaxException {

        String baseUrl = dataPath + "/" + AGENT_TAG;

        logger.info("processing simulation result for local path =" + baseUrl);

        //   recreate the nuclear powerplant on flight
        NuclearKBCreator in = new NuclearKBCreator();
        Map<String, OntModel> mapIri2Model = in.startConversion(baseUrl);
        storeModels(mapIri2Model);

        List<String> result = new ArrayList<String>();
        for (String current : mapIri2Model.keySet()) {
            if (current.contains("NucPP")) {
                result.add(current);
            }
        }

        return result;
    }

    private void storeModels(Map<String, OntModel> mapIri2Model) {

        QueryBroker broker = new QueryBroker();

        for (String current : mapIri2Model.keySet()) {
            OntModel model = mapIri2Model.get(current);
            String content = JenaHelper.writeToString(model);
            broker.put(current, content);
        }
    }


    public void prepareCSVPartialRemaining(List<String> plantlist, String iriofnetwork, String baseUrl) throws IOException {
        OntModel model = Util.readModelGreedy(iriofnetwork);
        String genplantinfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "SELECT ?entity ?plant ?Pmaxvalue ?xval ?yval "
                + "WHERE {?entity  a  j1:PowerGenerator  ."
                + "?entity   j2:isSubsystemOf ?plant ."
                + "?entity   j2:isModeledBy ?model ."
                + "?model   j5:hasModelVariable ?pmax ."
                + "?pmax  a  j3:PMax  ."
                + "?pmax  j2:hasValue ?vpmax ."
                + "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax
                + "?entity   j7:hasGISCoordinateSystem ?coordsys ."
                + "?coordsys   j7:hasProjectedCoordinate_x ?xent ."
                + "?xent j2:hasValue ?vxent ."
                + "?vxent   j2:numericalValue ?xval ." // xvalue
                + "?coordsys   j7:hasProjectedCoordinate_y ?yent ."
                + "?yent j2:hasValue ?vyent ."
                + "?vyent   j2:numericalValue ?yval ." // xvalue
                + "}";

        ResultSet resultSet = JenaHelper.query(model, genplantinfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
        int x = 0;
        double sumcapreplaced = 0;
        List<String[]> csvresult = new ArrayList<String[]>();
		String[]header= {"type","Co","x","y"}; //Co=capacity
        csvresult.add(header);
       
		while (x < resultList.size()) {
			if (!plantlist.contains(resultList.get(x)[0])) { // has been switched to 0 instead of 1 cause we use
																// generator scale
				System.out.println("generator remains= " + resultList.get(x)[0]);
				System.out.println("P max= " + resultList.get(x)[2]);
				System.out.println("x= " + resultList.get(x)[3]);
				System.out.println("y= " + resultList.get(x)[4]);
				String[] content = new String[4];
				content[0] = "c" + x;
				content[1] = resultList.get(x)[2];
				content[2] = resultList.get(x)[3];
				content[3] = resultList.get(x)[4];
				csvresult.add(content);

			} else {
				sumcapreplaced = sumcapreplaced + Double.valueOf(resultList.get(x)[2]);
			}

			x++;
		}
		if (csvresult.size() == 1) {
			String[] content = new String[4];
			content[0] = "c0";
			content[1] = "0.00001";
			content[2] = "0.00001";
			content[3] = "0.00001";
			csvresult.add(content);
			//default value if all generators are removed
		}
            System.out.println("sum replaced= " + sumcapreplaced);
        

        
		/**nuclear should not be included in the remaining generator option since 7/11/19
		String[]content2= new String[4];
		content2[0]="n";
		content2[1]=""+sumcapreplaced;
		content2[2]="0.0";
		content2[3]="0.0";
		csvresult.add(content2);
 
 */
        String s = MatrixConverter.fromArraytoCsv(csvresult);
        QueryBroker broker = new QueryBroker();
		 broker.putLocal(baseUrl + "/parameters_req_existing.csv", s);

    }

    public void prepareCSVLandlot(String lotiri, String baseUrl) {

        String lotsInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "SELECT ?entity ?xvalue ?yvalue ?areavalue ?distancevalue "

                + "WHERE {?entity  a  j1:Landlot  ."
                + "?entity   j5:hasSurfaceGeometry ?sur ."
                + "?sur   j5:has_area ?surarea ."
                + "?surarea   j2:hasValue ?vsurarea ."
                + "?vsurarea   j2:numericalValue ?areavalue ."

                + "?entity   j7:hasGISCoordinateSystem ?coorsys ."
                + "?coorsys   j7:hasProjectedCoordinate_x ?x ."
                + "?x   j2:hasValue ?xval ."
                + "?xval   j2:numericalValue ?xvalue ."
                + "?coorsys   j7:hasProjectedCoordinate_y ?y ."
                + "?y   j2:hasValue ?yval ."
                + "?yval   j2:numericalValue ?yvalue ."

                + "?entity   j1:hasDistanceToClosestWaterSources ?distance ."
                + "?distance   j2:hasValue ?distval ."
                + "?distval   j2:numericalValue ?distancevalue ."

                + "}";

        QueryBroker broker = new QueryBroker();

        String result = broker.queryFile(lotiri, lotsInfo);
        String[] keys = {"entity", "yvalue", "xvalue", "areavalue", "distancevalue"};
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

        logger.info("number of queried lot entities = " + resultList.size());

        IriMapper mapper = new IriMapper();
        for (int i = 0; i < resultList.size(); i++) {
            String[] current = resultList.get(i);
            String id = "s" + (i + 1);
            mapper.add(current[0], id, "lot");
            current[0] = id;
        }

        String csv = mapper.serialize();
        broker.putLocal(baseUrl + "/mappingforlot.csv", csv);

        String[] header = {"id", "ys", "xs", "as", "dcs"};
        resultList.add(0, header);
        String s = MatrixConverter.fromArraytoCsv(resultList);
        broker.putLocal(baseUrl + "/inputlandlots.csv", s);

        logger.info("landlots input ok");
    }

    public void prepareCSVLoad(String iriofnetwork, String baseUrl) {

        String greedySparqlQuery = "PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "SELECT ?component "
                + "WHERE {?entity a sys:CompositeSystem . "
                + "?entity sys:hasSubsystem ?component . "
                + "}";

        String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "SELECT ?entity ?xvalue ?yvalue ?activepowervalue "

                + "WHERE {?entity  a  j1:BusNode  ."
                + "?entity   j2:isModeledBy ?model ."
                + "?model   j5:hasModelVariable ?Pd ."
                + "?Pd  a  j3:PdBus  ."
                + "?Pd  j2:hasValue ?vpd ."
                + "?vpd   j2:numericalValue ?activepowervalue ."

                + "?entity   j7:hasGISCoordinateSystem ?coorsys ."
                + "?coorsys   j7:hasProjectedCoordinate_x ?x ."
                + "?x   j2:hasValue ?xval ."
                + "?xval   j2:numericalValue ?xvalue ."
                + "?coorsys   j7:hasProjectedCoordinate_y ?y ."
                + "?y   j2:hasValue ?yval ."
                + "?yval   j2:numericalValue ?yvalue ."

                + "}";

        QueryBroker broker = new QueryBroker();

        String result = broker.queryFilesGreedy(iriofnetwork, greedySparqlQuery, busInfo);
        String[] keys = new String[]{"entity", "yvalue", "xvalue", "activepowervalue", "null"};
        List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

        logger.info("number of queried bus entities = " + resultList.size());

        double mean = 0.0079;
        double variance = 0.004;
        double stddev = Math.sqrt(variance);
        IriMapper mapper = new IriMapper();

        for (int i = 0; i < resultList.size(); i++) {
            String[] current = resultList.get(i);
            String id = "p" + (i + 1);
            mapper.add(current[0], id, "bus");
            current[0] = id;
            // TODO-AE 2090304 why replace and split? BECAUSE IT IS INTEGER AND HAS ^^iNTEGER

            String stPdvalue = current[3].replace("^^", "@").split("@")[0];
            current[3] = stPdvalue;

            boolean popDensityFound = false;
            while (!popDensityFound) {
                double popDensity = stddev * new Random().nextGaussian() + mean;
                if (popDensity > 0) {
                    popDensityFound = true;
                    current[4] = String.valueOf(popDensity);
                }
            }
        }

        String csv = mapper.serialize();
        broker.putLocal(baseUrl + "/mappingforbus.csv", csv);

        String[] header = {"id", "yp", "xp", "Dp", "rhop"};
        resultList.add(0, header);
        String s = MatrixConverter.fromArraytoCsv(resultList);
        broker.putLocal(baseUrl + "/inputloadpoints.csv", s);

        logger.info("bus input ok");
    }
}
