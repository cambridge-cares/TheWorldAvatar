package uk.ac.cam.cares.jps.agent.mechanism.datadriven.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import junit.framework.Assert;
import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgent;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class MoDSDataDrivenAgentJobSetupTest extends MoDSDataDrivenAgent {

	/**
	 * Performs a test of the agent's job setup using a sample JSON request (read from a resource file). 
	 */
	private static final long serialVersionUID = -5572888046042546793L;

	@Test
	public void test() throws IOException, MoDSDataDrivenAgentException, SlurmJobException {
		initAgentProperty();
		
		BufferedReader br = FileUtil.openSourceFile(getClass().getClassLoader().getResource(modsDataDrivenAgentProperty.getJsonInputFileName().concat(modsDataDrivenAgentProperty.getJsonFileExtension())).getPath());
		String jsonString = "";
		String line = "";
		while ((line = br.readLine()) != null) {
			jsonString = jsonString.concat(line);
		}
		br.close();
		
		String message = setUpJobOnAgentMachine(jsonString);
		System.out.println("JOB MESSAGE:");
		System.out.println(message);
		
		Assert.assertEquals(new JSONObject(message).get("message"), Status.JOB_SETUP_SUCCESS_MSG.getName());
	}
	
	/**
	 * Initialises the unique instance of the MoDSDataDrivenAgentProperty class that<br>
	 * reads all properties of MoDSDataDrivenAgent from the modsdatadriven-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modsdatadriven-agent property file<br>
	 * through the MoDSDataDrivenAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modsdatadriven-agent.properties file
		if (applicationContextMoDSDataDrivenAgent == null) {
			applicationContextMoDSDataDrivenAgent = new AnnotationConfigApplicationContext(MoDSDataDrivenAgentConfiguration.class);
		}
		if (modsDataDrivenAgentProperty == null) {
			modsDataDrivenAgentProperty = applicationContextMoDSDataDrivenAgent.getBean(MoDSDataDrivenAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsDataDrivenAgentProperty.getAgentClass(), modsDataDrivenAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(modsDataDrivenAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(modsDataDrivenAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(modsDataDrivenAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(modsDataDrivenAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(modsDataDrivenAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(modsDataDrivenAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(modsDataDrivenAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(modsDataDrivenAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(modsDataDrivenAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(modsDataDrivenAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(modsDataDrivenAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(modsDataDrivenAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(modsDataDrivenAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(modsDataDrivenAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(modsDataDrivenAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(modsDataDrivenAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSDataDrivenAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSDataDrivenAgentException, SlurmJobException {
		long timeStamp = Utils.getTimeStamp();
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(modsDataDrivenAgentProperty.getSlurmScriptFileName()).getPath()), 
				new File(getClass().getClassLoader().getResource(modsDataDrivenAgentProperty.getInputFileName().concat(modsDataDrivenAgentProperty.getInputFileExtension())).getPath()), 
				timeStamp);
	}

}
