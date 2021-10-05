package uk.ac.cam.cares.jps.agent.mechanism.moo.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import junit.framework.Assert;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgent;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.moo.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class MoDSMooAgentJobSetupTest extends MoDSMooAgent {

	/**
	 * Performs a test of the agent's job setup using a sample JSON request (read from a resource file). 
	 */
	private static final long serialVersionUID = -5572888046042546793L;

	@Test
	public void test() throws IOException, MoDSMooAgentException, SlurmJobException {
		initAgentProperty();
		
		BufferedReader br = FileUtil.openSourceFile(getClass().getClassLoader().getResource(MoDSMooAgentProperty.getJsonInputFileName().concat(MoDSMooAgentProperty.getJsonFileExtension())).getPath());
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
	 * Initialises the unique instance of the MoDSMooAgentProperty class that<br>
	 * reads all properties of MoDSMooAgent from the modsmoo-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modsmoo-agent property file<br>
	 * through the MoDSMooAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modsdatadriven-agent.properties file
		if (applicationContextMoDSMooAgent == null) {
			applicationContextMoDSMooAgent = new AnnotationConfigApplicationContext(MoDSMooAgentConfiguration.class);
		}
		if (MoDSMooAgentProperty == null) {
			MoDSMooAgentProperty = applicationContextMoDSMooAgent.getBean(MoDSMooAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(MoDSMooAgentProperty.getAgentClass(), MoDSMooAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(MoDSMooAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(MoDSMooAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(MoDSMooAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(MoDSMooAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(MoDSMooAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(MoDSMooAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(MoDSMooAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(MoDSMooAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(MoDSMooAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(MoDSMooAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(MoDSMooAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(MoDSMooAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(MoDSMooAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(MoDSMooAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(MoDSMooAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(MoDSMooAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSMooAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSMooAgentException, SlurmJobException {
		long timeStamp = Utils.getTimeStamp();
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(MoDSMooAgentProperty.getSlurmScriptFileName()).getPath()), 
				new File(getClass().getClassLoader().getResource(MoDSMooAgentProperty.getInputFileName().concat(MoDSMooAgentProperty.getInputFileExtension())).getPath()), 
				timeStamp);
	}

}
