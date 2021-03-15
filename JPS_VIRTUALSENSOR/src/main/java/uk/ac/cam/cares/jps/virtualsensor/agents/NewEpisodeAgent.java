package uk.ac.cam.cares.jps.virtualsensor.agents;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.annotation.WebServlet;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.PostProcessing;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.virtualsensor.configuration.EpisodeAgentConfiguration;
import uk.ac.cam.cares.jps.virtualsensor.configuration.EpisodeAgentProperty;

@WebServlet(urlPatterns = {"/EpisodeAgent"})
public class NewEpisodeAgent extends JPSAgent{
	private static EpisodeAgentProperty episodeAgentProperty;
	public static ApplicationContext applicationContextEpisodeAgent;
	public static JobSubmission jobSubmission;
	private File jobSpace;
	public static final String FILE_NAME_3D_MAIN_CONC_DATA = "3D_instantanous_mainconc_center.dat";
	public static final String FILE_NAME_ICM_HOUR = "icmhour.nc";
	public static final String FILE_NAME_PLUME_SEGMENT = "plume_segments.dat";
	
	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(NewEpisodeAgent.class);
    }
	
	/**
     *  create logger to log changes attached to DispersionModellingAgent class. 
     */
    Logger logger = LoggerFactory.getLogger(NewEpisodeAgent.class);
    
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject responseParams=new JSONObject();
    	return responseParams;
    }
    
    @Override
	public void init(){
        logger.info("---------- Dispersion Modelling Agent has started ----------");
        System.out.println("---------- Dispersion Modelling Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        NewEpisodeAgent episodeAgent = new NewEpisodeAgent();
		// initialising classes to read properties from the dispersion-agent.properites file
        initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				episodeAgent.monitorJobs();
			} catch (SlurmJobException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}, episodeAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
				episodeAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("---------- Dispersion of pollutant simulation jobs are being monitored  ----------");
        System.out.println("---------- Dispersion of pollutant simulation jobs are being monitored  ----------");
       	
	}
    
    /**
	 * Initialises the unique instance of the DispersionAgentProperty class that<br>
	 * reads all properties of DispersionAgent from the dispersion-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the dispersion-agent property file<br>
	 * through the DispersionModellingAgent class.
	 */
	public void initAgentProperty() {
		// initialising classes to read properties from the dft-agent.properites
		// file
		if (applicationContextEpisodeAgent == null) {
			applicationContextEpisodeAgent = new AnnotationConfigApplicationContext(EpisodeAgentConfiguration.class);
		}
		if (episodeAgentProperty == null) {
			episodeAgentProperty = applicationContextEpisodeAgent.getBean(EpisodeAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(episodeAgentProperty.getAgentClass(), episodeAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(episodeAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty
					.setHpcServerLoginUserPassword(episodeAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(episodeAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty
					.setAgentCompletedJobsSpacePrefix(episodeAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty
					.setAgentFailedJobsSpacePrefix(episodeAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(episodeAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(episodeAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(episodeAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(episodeAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(episodeAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(episodeAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(episodeAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(episodeAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setExecutableFile(episodeAgentProperty.getExecutableFile());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(episodeAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(
					episodeAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty
					.setAgentPeriodicActionInterval(episodeAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
     * Calls the monitorJobs method of the Slurm Job API, which is in the JPS BASE LIB project.
     * 
     * @throws SlurmJobException
     */
	private void monitorJobs() throws SlurmJobException{
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(episodeAgentProperty.getAgentClass(), episodeAgentProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	public void processOutputs() {
		initAgentProperty();
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder) && !Utils.isJobErroneouslyCompleted(jobFolder) && !Utils.isJobOutputProcessed(jobFolder)) {
						System.out.println("job "+jobFolder.getName()+" is completed");
						File outputFolder= new File(jobFolder.getAbsolutePath().concat(File.separator).concat("output"));
						String zipFilePath = jobFolder.getAbsolutePath() + "/output.zip";
						File outputFile= new File(zipFilePath);
						if(!outputFile.isFile() || !outputFile.exists()){
							Utils.modifyStatus(Utils.getStatusFile(jobFolder).getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
							continue;
						}
						// Unzip the output zip file.
						FileUtil.unzip(zipFilePath, outputFolder.getAbsolutePath());
						// Opens the main concentration file.
						File file = new File(outputFolder.getAbsolutePath().concat(File.separator).concat(FILE_NAME_3D_MAIN_CONC_DATA));
						// Checks the existence of the main concentration file.
						if(!file.exists()){
							// If the main concentration file does not exist,
							// the job status is marked with error termination.   
							Utils.modifyStatus(Utils.getStatusFile(jobFolder).getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
							continue;							
						}
						if(annotateOutputs(jobFolder, zipFilePath)) {
							logger.info("DispersionModellingAgent: Annotation has been completed.");
							System.out.println("Annotation has been completed.");
							PostProcessing.updateJobOutputStatus(jobFolder);
						} else {
							logger.error("DispersionModellingAgent: Annotation has not been completed.");
							System.out.println("Annotation has not been completed.");
							// Edit the status file to be error termination
							Utils.modifyStatus(Utils.getStatusFile(jobFolder).getAbsolutePath(),
									Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("EpisodeAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		} catch(SlurmJobException e){
			logger.error("EpisodeAgent: ".concat(e.getMessage()));
			e.printStackTrace();
		}
	}
	
	private boolean annotateOutputs(File jobFolder, String zipFilePath) throws SlurmJobException {
		try {
			System.out.println("Annotating output has started");
			String directory = jobFolder.getAbsolutePath() + "/input.json";
			String destDir = jobFolder.getAbsolutePath() + "/output";
			File json = new File(directory);
			JSONObject jo = new JSONObject(json);
			String datapath = jo.getString("datapath");

			File file = new File(destDir.concat(File.separator).concat(FILE_NAME_3D_MAIN_CONC_DATA));
			String destinationUrl = datapath + "/"+ FILE_NAME_3D_MAIN_CONC_DATA;

			File file2 = new File(destDir + "/"+ FILE_NAME_ICM_HOUR);
			String destinationUrl2 = datapath + "/"+ FILE_NAME_ICM_HOUR;
			File file2des = new File(destinationUrl2);
			FileUtils.copyFile(file2, file2des);

			File file3 = new File(destDir + "/"+ FILE_NAME_PLUME_SEGMENT);
			String destinationUrl3 = datapath + "/"+ FILE_NAME_PLUME_SEGMENT;
			File file3des = new File(destinationUrl3);
			FileUtils.copyFile(file3, file3des);

			new QueryBroker().putLocal(destinationUrl, file); // put to scenario
																// folder
			// new QueryBroker().putLocal(destinationUrl2, file2); //put to
			// scenario folder
			// new QueryBroker().putLocal(destinationUrl3, file3); //put to
			// scenario folder
			System.out.println("metadata annotation started");

			// update triple-store

			System.out.println("metadata annotation finished");
		} catch (Exception e) {
			logger.error(e.getMessage());
			logger.error("DispersionModellingAgent: Output Annotating Task could not finish");
			System.out.println("DispersionModellingAgent: Output Annotating Task could not finish");
			e.printStackTrace();
			return false;
		}
		return true;
	}
}
