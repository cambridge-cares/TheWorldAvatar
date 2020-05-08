package uk.ac.cam.cares.jps.dispersion.interpolation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
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
		String coordinates = requestParams.optString("coordinates","[364628.312 131794.703 0]");
		String gasType;
		String agentiri = requestParams.optString("agentiri","http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		String location = requestParams.optString("location","http://dbpedia.org/resource/Singapore");
		String directoryFolder = getLastModifiedDirectory(agentiri, location);
		
		ArrayList<String> gsType = determineGas(directoryFolder);
		gasType = gsType.get(0);
		String fileName = gsType.get(1);
		String options = requestParams.optString("options","2");//How do we select the options? 
		
		
		String dispMatrix = copyOverFile(baseUrl,fileName);//to be swapped with copyOverFile
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
	/** find an array of path names that follow dat or match
	 * 
	 * @param dirName
	 * @return
	 */
	public String[] finder(String dirName) {
        File dir = new File(dirName);
        return dir.list(new FilenameFilter(){
        		//checks if file ends with dat or gst file
                 public boolean accept(File dir, String filename){ 
                	 return (filename.endsWith(".dat")||filename.endsWith("levels.gst"));
                }
        });

    }
	public ArrayList<String> determineGasGst(String dirName){
		String[] arrayFile = finder(dirName);
		String fGas = arrayFile[0];
		String fullString = "";
		File lstName = new File(dirName, fGas);//fGas is the name of the test.levels.gst
		//unlike the method below which looks for header in .dat
		try {
			BufferedReader bff = new BufferedReader(new FileReader(lstName));
			String text = bff.readLine(); //get first line of file
			String[] gasTypes = text.split(",Conc"); //Have to insert \\ because of parenthesis
			List<String> newLst = Arrays.asList(gasTypes);
			List<String> gasTypeArr = new ArrayList<String>();
			gasTypeArr.addAll(newLst); //because asList returns a fixed-size list
			gasTypeArr.remove(0);
			List<String> gasTypeArr2 = gasTypeArr.subList(0,(gasTypeArr.size()/4));
			for (int i = 0; i < gasTypeArr2.size(); i ++) {
				String j= gasTypeArr2.get(i).split("\\|")[2];
				gasTypeArr2.set(i, j);
			}
			String[] gasTypes2 = new String[gasTypeArr2.size()];
			gasTypes2 = gasTypeArr2.toArray(gasTypes2);	
			
			fullString = "['" +String.join(" ", gasTypes2) +"']";
		}catch (IOException e) {
			e.printStackTrace();
		}
		
		ArrayList<String> lst_a = new ArrayList<String>();
		lst_a.add(fullString);
		lst_a.add(lstName.getAbsolutePath());
		return lst_a;
	}
	/** determine the types of gas available. 
	 * 
	 * @param dirName
	 * @return {ArrayList} [gases, name of relevantFile]
	 */
	public ArrayList<String> determineGas(String dirName) {
		//Since there are two files with gst, then we'll just choose the first in the array of [1] or [2]
		String[] arrayFile = finder(dirName);
		String fGas = arrayFile[0];
		String fullString = "['NO NO2']";
		File lstName = new File(dirName, fGas);
		try {
			BufferedReader bff = new BufferedReader(new FileReader(lstName));
			String text = bff.readLine(); //get first line of file
			String[] gasTypes = text.split("Z\\(m\\)"); //Have to insert \\ because of parenthesis
			String gasType = gasTypes[gasTypes.length-1].trim();
			//get last element (the gases are separated by six spaces
			
			String[] gasTypes2 = gasType.split("\\s+");
			fullString = "['" +String.join(" ", gasTypes2) +"']";
		}catch (IOException e) {
			e.printStackTrace();
		}
		ArrayList<String> lst_a = new ArrayList<String>();
		lst_a.add(fullString);
		lst_a.add(lstName.getAbsolutePath());
		return lst_a;
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
	public void copyTemplate(String newdir, String filename) { //in this case for main .m file
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
	/** copies over the files in the working directory over to scenario folder. 
	 * 
	 * @param newdir location of directory of scenario folder
	 * @param filename name of actual file without directory
	 */
	public String copyOverFile(String newdir, String filename) { //in this case for source folder
		Path path = Paths.get(filename);
		File file = new File(filename);
		Path fileName = path.getFileName(); 
		logger.info(filename);
		String destinationUrl = newdir + "/"+fileName.toString();
		logger.info(destinationUrl);
		new QueryBroker().putLocal(destinationUrl, file);
		return fileName.toString();
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
	/**
     * Gets the latest file created using rdf4j
     * @return last created file
     */
    public String getLastModifiedDirectory(String agentiri, String location ) {
//    	agentiri = "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service";
		List<String> lst = new ArrayList<String>();
		lst.add(location);
    	System.out.println(lst);
    	String resultfromfuseki = MetaDataQuery.queryResources(null,null,null,agentiri, null, null,null,lst);
		 String[] keys = JenaResultSetFormatter.getKeys(resultfromfuseki);
		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromfuseki, keys);
    	return listmap.get(0)[0];
    }

}
