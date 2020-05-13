package uk.ac.cam.cares.jps.dispersion.general;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.servlet.annotation.WebServlet;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import uk.ac.cam.cares.jps.dispersion.episode.EpisodeAgent;

@Controller
@WebServlet("/DispersionModellingAgent")

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
	
//    @Override
//	public void init(){
//        logger.info("---------- Episode Agent has started ----------");
//        System.out.println("---------- Episode Agent has started ----------");
//        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
//        DispersionModellingAgent episodeAgent = new DispersionModellingAgent();
//       	// the first 60 refers to the delay (in seconds) before the job scheduler
//        // starts and the second 60 refers to the interval between two consecu-
//        // tive executions of the scheduler.
//        executorService.scheduleAtFixedRate(episodeAgent::monitorJobs, 30, 60, TimeUnit.SECONDS);
//		// initialising classes to read properties from the dft-agent.properites file
//        if (applicationContext == null) {
//			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
//		}
//		if (slurmJobProperty == null) {
//			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
//			logger.info("slurmjobproperty="+slurmJobProperty.toString());
//		}
//        logger.info("---------- simulation jobs are being monitored  ----------");
//        System.out.println("---------- simulation jobs are being monitored  ----------");
//       	
//	}
    
    @Override
	protected JSONObject processRequestParameters(JSONObject requestParams) {
    	String cityIRI = requestParams.getString("city");
    	String agent=requestParams.get("agent").toString();
    	DispersionModellingAgent s=null;
    	JSONObject responseParams=requestParams;
    	if(cityIRI.toLowerCase().contains("singapore")||cityIRI.toLowerCase().contains("kong")) {
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
    	}
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
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	private void processOutputs() {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("EpisodeAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		} 

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
