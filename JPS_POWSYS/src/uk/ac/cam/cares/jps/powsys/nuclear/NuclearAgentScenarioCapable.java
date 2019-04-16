package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = {"/NuclearAgent/startsimulation", "/NuclearAgent/processresult"})
public class NuclearAgentScenarioCapable extends JPSHttpServlet {
	
	private static final long serialVersionUID = -4199209974912271432L;
	private Logger logger = LoggerFactory.getLogger(NuclearAgentScenarioCapable.class);
	public static final String AGENT_TAG = "GAMS_NuclearAgent";
	
	public void runGAMS() throws IOException, InterruptedException {
        System.out.println("Start");
        System.out.println("separator= "+File.separator);
        String executablelocation ="C:/GAMS/win64/26.1/gams.exe";
        //String folderlocation ="D:/Users/KADIT01/Documents/gamsdir/projdir/";
        String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/";
        String[] cmdArray = new String[5];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "final.gms";
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "LO=2";
//      cmdArray[2] = "WDIR="+folderlocation + "TMP";
//      cmdArray[3] = "SCRDIR="+folderlocation + "TMP";
        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4];
        
		//System.out.println(cmdArrayinstring);
        //Process p = Runtime.getRuntime().exec(cmdArray);
		   //p.waitFor();
		String startbatCommand ="C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat";
		
		ArrayList<String> groupcommand= new ArrayList<String>();
		groupcommand.add("start");
		groupcommand.add("C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat");
		
		CommandHelper.executeSingleCommand(folderlocation,startbatCommand);
//		CommandHelper.executeCommands(folderlocation, groupcommand);   
        System.out.println("Done");
	}
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getServletPath();
		System.out.println("path= "+path);
		
		if ("/NuclearAgent/startsimulation".equals(path)) {
		
			JSONObject jofornuc = AgentCaller.readJsonParameter(request);
			
			try {
				String lotiri = jofornuc.getString("landlot");
				String iriofnetwork = jofornuc.getString("electricalnetwork");
				boolean runGams = true;
				if (!jofornuc.isNull(JPSConstants.RUN_SIMULATION)) {
					runGams = jofornuc.getBoolean(JPSConstants.RUN_SIMULATION);
				}
				
				String dataPath = QueryBroker.getLocalDataPath();
				startSimulation(lotiri, iriofnetwork, dataPath, runGams);
				
			} catch (JSONException | InterruptedException e) {
				logger.error(e.getMessage(), e);
				throw new JPSRuntimeException(e.getMessage(), e);
			}
					
		} else if ("/NuclearAgent/processresult".equals(path)) {
			
			try {
				String dataPath = QueryBroker.getLocalDataPath();
				List<String> result = processSimulationResult(dataPath);
				JSONObject resultjson = new JSONObject().put("plantirilist", result);
				AgentCaller.printToResponse(resultjson.toString(), response);	
			} catch (NumberFormatException | URISyntaxException e) {
				logger.error(e.getMessage(), e);
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}	
	}
	
	public void startSimulation(String lotiri, String iriofnetwork, String dataPath, boolean runGams) throws IOException, InterruptedException {
		
		String baseUrl = dataPath + "/" + AGENT_TAG;
		
		logger.info("starting simulation for baseUrl=" + baseUrl);
		
		QueryBroker broker = new QueryBroker();
		
		prepareCSVLandlot(lotiri, baseUrl); //used to create csv
    
		//-----------------------------------------1st input file finished-------------------------------------------------------------------	

        prepareCSVLoad(iriofnetwork, baseUrl); //used to create csv
		
        //-----------------------------------------2nd input file finished-------------------------------------------------------------------		
	
        String resourceDir = NuclearAgentScenarioCapable.getResourceDir(this);
        File file = new File(resourceDir + "/constants_req.csv");
        broker.put(baseUrl + "/constants_req.csv", file);

        //-----------------------------------------3rd input file finished-------------------------------------------------------------------
    
        file = new File(resourceDir + "/parameters_req.csv");
        broker.put(baseUrl + "/parameters_req.csv", file);
 
        //-----------------------------------------4th input file finished-------------------------------------------------------------------
    
        if (runGams) {
        	runGAMS();
        }
	}
	
	public static String getResourceDir(Object thisObject) {
		return AgentLocator.getCurrentJpsAppDirectory(thisObject) + "/testres";
	}
	
	public List<String> processSimulationResult(String dataPath) throws NumberFormatException, IOException, URISyntaxException {
	
		String baseUrl = dataPath + "/" + AGENT_TAG;
		
		logger.info("processing simulation result for baseUrl=" + baseUrl);
		
		//   recreate the nuclear powerplant on flight
		NuclearKBCreatorScenarioCapable in= new NuclearKBCreatorScenarioCapable();
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

	public void prepareCSVLandlot(String lotiri, String baseUrl) {		

		String lotsInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#> " 
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
    	
    	IriMapperScenarioCapable mapper= new IriMapperScenarioCapable();
		for (int i=0; i<resultList.size(); i++) {
			String[] current = resultList.get(i);
			String id = "s"+(i+1);
			mapper.add(current[0], id, "lot");
			current[0]=id;
		}
    
		String csv = mapper.serialize();
	    broker.put(baseUrl + "/mappingforlot.csv", csv);
	    
	    String[] header = {"id","ys","xs","as","dcs"};
	    resultList.add(0, header);
	    String s = MatrixConverter.fromArraytoCsv(resultList);
	    broker.put(baseUrl + "/inputlandlots.csv", s);
	    
	    logger.info("landlots input ok"); 
	}

	public void prepareCSVLoad(String iriofnetwork, String baseUrl) {
		
		String greedySparqlQuery = "PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity a sys:CompositeSystem . " 
				+ "?entity sys:hasSubsystem ?component . "								
				+ "}";
		
		String busInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
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
		String[] keys = new String[] {"entity", "yvalue", "xvalue", "activepowervalue", "null"};
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
		logger.info("number of queried bus entities = " + resultList.size());
		
		double mean = 0.0079;
		double variance = 0.004;
		double stddev = Math.sqrt(variance);
		IriMapperScenarioCapable mapper= new IriMapperScenarioCapable();
		
		for (int i=0; i<resultList.size(); i++) {
			String[] current = resultList.get(i);
			String id = "p"+(i+1);
			mapper.add(current[0], id, "bus");
			current[0]=id;
			// TODO-AE 2090304 why replace and split? BECAUSE IT IS INTEGER AND HAS ^^iNTEGER
			
		    String stPdvalue = current[3].replace("^^","@").split("@")[0];
		    current[3] = stPdvalue;
		    
		    boolean popDensityFound = false;
			while (!popDensityFound) {
				double popDensity = stddev * new Random().nextGaussian() + mean;
				if (popDensity > 0) {
				      popDensityFound = true;
				      current[4]=String.valueOf(popDensity);
				}
			}
		}
		
		String csv = mapper.serialize();
	    broker.put(baseUrl + "/mappingforbus.csv", csv);
		
	    String[] header= {"id","yp","xp","Dp","rhop"};
	    resultList.add(0, header);
	    String s = MatrixConverter.fromArraytoCsv(resultList);
	    broker.put(baseUrl + "/inputloadpoints.csv", s);
	    
	    logger.info("bus input ok");
	}
}
