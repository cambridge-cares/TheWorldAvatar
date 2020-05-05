package uk.ac.cam.cares.jps.dispersion.interpolation;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
@WebServlet({"/InterpolationAgent/startSimulation", "/InterpolationAgent/continueSimulation"})
public class InterpolationAgent  extends JPSHttpServlet {
	public String SIM_START_PATH = "/InterpolationAgent/startSimulation";
	public String SIM_PROCESS_PATH = "/InterpolationAgent/endSimulation";
	
	private static final long serialVersionUID = 1L;
	public static final String KEY_WATCH = "watch";
	public static final String KEY_CALLBACK_URL = "callback";
    protected void setLogger() {
        logger = LoggerFactory.getLogger(InterpolationAgent.class);
    }
    /**
     *  create logger to log changes attached to WasteToEnergyAgent class. 
     */
    protected Logger logger = LoggerFactory.getLogger(InterpolationAgent.class);
		
	public InterpolationAgent() {
		// TODO Auto-generated constructor stub
	}
	
	/** main function. Reads the values in and copies the templates back. 
	 * 
	 */
	@Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		//temporarily until we get an idea of how to read input from Front End
		String baseUrl= QueryBroker.getLocalDataPath()+"/JPS_DIS";
		String coordinates = requestParams.optString("coordinates","[380000 150000 0]");
		String gasType = requestParams.optString("gasType","['NO NO2']");
		String options = requestParams.optString("options","1");
		String dispMatrix;
		if (requestParams.getInt("typ")==1){
			dispMatrix = "3D_instantanous_mainconc_center.dat";
		}else {
			dispMatrix = "test.gst";//has not configured to this step yet. 
		}
		copyTemplate(baseUrl,dispMatrix);
		copyTemplate(baseUrl, "virtual_sensor.m");
		//modify matlab to read 
		if (SIM_START_PATH.equals(path)) {
			try {
				createBat(baseUrl, coordinates,gasType, options, dispMatrix);
				runModel(baseUrl);
				//save in somewhere... readings are read to exp.txt for now
				logger.info("finish Simulation");
	           
			} catch (Exception e) {
				e.printStackTrace();
			}
		 }
		return requestParams;
	}
	/** notifies the watcher to return with the callback. 
	 * 
	 * @param agentArgs JSONObject
	 * @param filePath String
	 * @param callbackIRI String
	 */
    private void notifyWatcher(JSONObject agentArgs, String filePath, String callbackIRI) {
        agentArgs.put(KEY_WATCH, filePath);
        agentArgs.put(KEY_CALLBACK_URL, callbackIRI);
        execute(KeyValueMap.getInstance().get("url.jps_aws"), agentArgs.toString(), HttpPost.METHOD_NAME);
    }
	/** copies over the files in the working directory over to scenario folder. 
	 * 
	 * @param newdir
	 * @param filename
	 */
	public void copyTemplate(String newdir, String filename) { //in this case for SphereDist.m; Main.m; D2R.m
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	/** create the batch file in the mentioned folder. 
	 * 
	 * @param baseUrl
	 * @throws Exception
	 */
	public void createBat(String baseUrl, String coordinates, String gasType, String options, String dispMatrix) throws Exception {
		String loc = " virtual_sensor(" + coordinates +"," +gasType
				+"," +options+",'"+baseUrl+"/','" + dispMatrix+"'";
		String bat = "setlocal" + "\n" + "cd /d %~dp0" + "\n" 
		+ "matlab -nosplash -noFigureWindows -r \"try; cd('"+baseUrl
		+"');"+ loc + "); catch; end; quit\"";
		new QueryBroker().putLocal(baseUrl + "/runm.bat", bat);
	}
	/** runs the batch file. 
	 * 
	 * @param baseUrl
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public void runModel(String baseUrl) throws IOException, InterruptedException {
		String startbatCommand =baseUrl+"/runm.bat";
		String result= executeSingleCommand(baseUrl,startbatCommand);
		logger.info("final after calling: "+result);
	}
	/** executes the process. Called by runModel. 
	 * 
	 * @param targetFolder
	 * @param command
	 * @return
	 * @throws InterruptedException
	 */
	private String executeSingleCommand(String targetFolder , String command) throws InterruptedException 
	{  
	 
		logger.info("In folder: " + targetFolder + " Executed: " + command);
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(command, null, new File(targetFolder)); // IMPORTANT: By specifying targetFolder, all the cmds will be executed within such folder.
			pr.waitFor();
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		
				 
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			
			while((line = bfr.readLine()) != null) {
				resultString += line;

			}
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		return resultString; 
	}
	 /** reads the result from the csv file produced and returns as List<String[]>
		 * 
		 * @param baseUrl String
		 * @param filename name of the file. 
		 * @return
		 * @throws IOException
		 */
		private List<String[]> readResult(String baseUrl,String filename) throws IOException {

	        String outputFile = baseUrl + "/"+filename;
	        String csv = new QueryBroker().readFileLocal(outputFile);
	        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
			
			return simulationResult;
		}
}
