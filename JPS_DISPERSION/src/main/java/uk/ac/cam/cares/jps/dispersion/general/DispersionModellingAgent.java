package uk.ac.cam.cares.jps.dispersion.general;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.dispersion.episode.EpisodeAgent;

@Controller
///@WebServlet("/DispersionModellingAgent")
@WebServlet(urlPatterns = {"/episode/dispersion","/adms/dispersion"})
public class DispersionModellingAgent extends JPSHttpServlet {
	
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static final String DATA_KEY_COLLECTION = "collection";
    protected static final String DATA_KEY_ITEMS = "items";
    protected static final String DATA_KEY_LAT = "lat";
    protected static final String DATA_KEY_LON = "lon";
    protected static final String DATA_KEY_MMSI = "mmsi";
    private static final String DATA_KEY_SS = "ss";
    private static final String DATA_KEY_CU = "cu";
	public static final String EPISODE_PATH = "/episode/dispersion";
	public static final String ADMS_PATH = "/adms/dispersion";
	public static final String EX_UNKNOWN_DMAGENT = "Unknown Dispersion Modelling Agent Requested";

	static JobSubmission jobSubmission;
	public static SlurmJobProperty slurmJobProperty;
	public static ApplicationContext applicationContext;
	private File jobSpace;
	
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(DispersionModellingAgent.class);
    }
    
    /**
     *  create logger to log changes attached to DispersionModellingAgent class. 
     */
    Logger logger = LoggerFactory.getLogger(DispersionModellingAgent.class);
	
    @Override
	public void init(){
        logger.info("---------- Dispersion Modelling Agent has started ----------");
        System.out.println("---------- Dispersion Modelling Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        DispersionModellingAgent episodeAgent = new DispersionModellingAgent();
       	// the first 60 refers to the delay (in seconds) before the job scheduler
        // starts and the second 60 refers to the interval between two consecu-
        // tive executions of the scheduler.
        executorService.scheduleAtFixedRate(episodeAgent::monitorJobs, 30, 60, TimeUnit.SECONDS);
		// initialising classes to read properties from the dft-agent.properites file
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
			logger.info("slurmjobproperty="+slurmJobProperty.toString());
		}
        logger.info("---------- simulation jobs are being monitored  ----------");
        System.out.println("---------- simulation jobs are being monitored  ----------");
       	
	}
    
    @Override
	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
    	//String cityIRI = requestParams.getString("city");
    	//String agent=requestParams.get("agent").toString();
    	DispersionModellingAgent dmAgent = null;
    	JSONObject responseParams = requestParams;

		if(path.equals(ADMS_PATH)) {
			dmAgent = new ADMSAgent();
		} else if (path.equals(EPISODE_PATH)) {
			dmAgent = new EpisodeAgent();
		}

		if (dmAgent != null) {
			responseParams = dmAgent.processRequestParameters(requestParams);
		} else {
			throw new JPSRuntimeException(EX_UNKNOWN_DMAGENT);
		}

		return responseParams;

    	/*if(cityIRI.toLowerCase().contains("singapore")||cityIRI.toLowerCase().contains("kong")) {
    		if(agent.contains("ADMS")) {
    			s=new ADMSAgent();
    		}else if(agent.contains("Episode")){
    			s= new EpisodeAgent();
    		}
    		responseParams=s.processRequestParameters(requestParams);
    		return responseParams;
    	}
    	else {
    		s=new ADMSAgent();
    		responseParams=s.processRequestParameters(requestParams);
    		return responseParams;
    	}*/
	}
		
	public void createEmissionInput(String dataPath, String filename,JSONObject shipdata) {
		
	}
	
	public void createEmissionInput(String entityType, String buildingInString, String plantIRI, JSONObject regionInJSON, String targetCRSName) {
		
	}
	public void createEmissionInput(String entityType, String buildingInString, String sourceIRI, JSONObject regionInJSON, String targetCRSName, String fullPath, String precipitation) {
		
	}
	
	public void executeModel(String dataPath) {
		
	}
	
    protected JSONArray getEntityData(JSONObject input) {
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
                        latlon.put("speed", item.getDouble(DATA_KEY_SS));
                        latlon.put("angle", item.getDouble(DATA_KEY_CU));
                        coordinates.put(latlon);
                    }
                }
            }
        }

        return coordinates;
    }
    
	private void monitorJobs(){
		System.out.println("monitor job starts");
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		System.out.println("calling process output");
		
		processOutputs();
	}
	
	public void processOutputs()throws JPSRuntimeException {
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
			logger.info("slurmjobproperty="+slurmJobProperty.toString());
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		
		jobSpace = jobSubmission.getWorkspaceDirectory();
		logger.info("getting jobspace= "+jobSpace.getAbsolutePath());
		System.out.println("getting jobspace= "+jobSpace.getAbsolutePath());
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						System.out.println("job is completed");
						if(!annotateOutputs(jobFolder)) {
							System.out.println("output not annotated");
							throw new JPSRuntimeException("annotate output fails");
						}
						
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							logger.info("job output is processed");
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("EpisodeAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		} 

	}
	
    public static void unzip(String zipFilePath, String destDir) {
    	 ZipFile zipFile = new ZipFile(zipFilePath);
 	    try {
 			zipFile.extractAll(destDir);
 		} catch (ZipException e) {
 			// TODO Auto-generated catch block
 			e.printStackTrace();
 		}
    }
    
	public boolean annotateOutputs(File jobFolder) throws IOException {
		System.out.println("annotate output started");
		try {
			
		
		String zipFilePath = jobFolder.getAbsolutePath() + "/output.zip";
		File out= new File(zipFilePath);
		if(out.isFile()) {
			String directory = jobFolder.getAbsolutePath() + "/input.json";
			String destDir = jobFolder.getAbsolutePath() + "/output";

			unzip(zipFilePath, destDir);
			File json = new File(directory);
			String content = FileUtils.readFileToString(json);
			JSONObject jo = new JSONObject(content);
			String cityIRI = jo.getString("city");
			String agent = jo.getString("agent");
			String datapath = jo.getString("datapath");
			String time = jo.getString("expectedtime");
			
			
	    	File file = new File(destDir+"/3D_instantanous_mainconc_center.dat");
			String destinationUrl = datapath + "/3D_instantanous_mainconc_center.dat";
	    	File file2 = new File(destDir+"/icmhour.nc");
			String destinationUrl2 = datapath + "/icmhour.nc";
			File file2des=new File(destinationUrl2);
			FileUtils.copyFile(file2, file2des);
	    	File file3 = new File(destDir+"/plume_segments.dat");
			String destinationUrl3 = datapath + "/plume_segments.dat";
			File file3des=new File(destinationUrl3);
			FileUtils.copyFile(file3, file3des);
			new QueryBroker().putLocal(destinationUrl, file); //put to scenario folder
			//new QueryBroker().putLocal(destinationUrl2, file2); //put to scenario folder
			//new QueryBroker().putLocal(destinationUrl3, file3); //put to scenario folder
			List<String> topics = new ArrayList<String>();
	    	topics.add(cityIRI);
	    	System.out.println("metadata annotation started");
	    	MetaDataAnnotator.annotate(destinationUrl, null, agent, true, topics, time); //annotate
	    	System.out.println("metadata annotation finished");
	    	out.delete();
		}
		}catch(Exception e) {
			logger.error(e.getMessage());
			logger.error("DispersionModellingAgent:Output Annotating Task could not finish");
			System.out.println("DispersionModellingAgent:Output Annotating Task could not finish");
		}
		return true;
	}
	
	protected JSONObject getNewRegionData(double upperx, double uppery, double lowerx, double lowery,
			String targetCRSName, String sourceCRSName) {
		double[] p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] { lowerx, lowery });
		String lx = String.valueOf(p[0]);
		String ly = String.valueOf(p[1]);
		p = CRSTransformer.transform(sourceCRSName, targetCRSName, new double[] { upperx, uppery });
		String ux = String.valueOf(p[0]);
		String uy = String.valueOf(p[1]);

		String regionTemplate = "{\r\n" + "	\"uppercorner\":\r\n" + "    	{\r\n"
				+ "        	\"upperx\" : \"%s\",\r\n" + "            \"uppery\" : \"%s\"      	\r\n" + "        },\r\n"
				+ "          \r\n" + "     \"lowercorner\":\r\n" + "     {\r\n" + "       \"lowerx\" : \"%s\",\r\n"
				+ "       \"lowery\" : \"%s\"\r\n" + "     }\r\n" + "}";

		JSONObject newRegion = new JSONObject(String.format(regionTemplate, ux, uy, lx, ly));
		return newRegion;
	}


	public void createWeatherInput(String dataPath, String filename, List<String> stniri) {
		// TODO Auto-generated method stub
		
	}
	
	protected List<String[]> queryEndPointDataset(String querycontext) {
		String dataseturl = KeyValueManager.get(IKeys.DATASET_WEATHER_URL);
		String resultfromrdf4j = KnowledgeBaseClient.query(dataseturl, null, querycontext);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		return listmap;
	}

}
