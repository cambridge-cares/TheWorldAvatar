package uk.ac.cam.cares.jps.agent.mechanism.sensana.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import junit.framework.Assert;
import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgent;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class MoDSSensAnaAgentJobSetupTest extends MoDSSensAnaAgent {

	/**
	 * Performs a test of the agent's job setup using a sample JSON request (read from a resource file). 
	 */
	private static final long serialVersionUID = -5572888046042546793L;

	@Test
	public void test() throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		initAgentProperty();
		
		BufferedReader br = FileUtil.openSourceFile(getClass().getClassLoader().getResource(modsSensAnaAgentProperty.getJsonInputFileName().concat(modsSensAnaAgentProperty.getJsonFileExtension())).getPath());
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
	 * Initialises the unique instance of the MoDSSensAnaAgentProperty class that<br>
	 * reads all properties of MoDSSensAnaAgent from the modssensana-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modssensana-agent property file<br>
	 * through the MoDSSensAnaAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modssensana-agent.properties file
		if (applicationContextMoDSSensAnaAgent == null) {
			applicationContextMoDSSensAnaAgent = new AnnotationConfigApplicationContext(MoDSSensAnaAgentConfiguration.class);
		}
		if (modsSensAnaAgentProperty == null) {
			modsSensAnaAgentProperty = applicationContextMoDSSensAnaAgent.getBean(MoDSSensAnaAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsSensAnaAgentProperty.getAgentClass(), modsSensAnaAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(modsSensAnaAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(modsSensAnaAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(modsSensAnaAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(modsSensAnaAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(modsSensAnaAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(modsSensAnaAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(modsSensAnaAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(modsSensAnaAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(modsSensAnaAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(modsSensAnaAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(modsSensAnaAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(modsSensAnaAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(modsSensAnaAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(modsSensAnaAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(modsSensAnaAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(modsSensAnaAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		long timeStamp = Utils.getTimeStamp();
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(modsSensAnaAgentProperty.getSlurmScriptFileName()).getPath()), 
				new File(getClass().getClassLoader().getResource(modsSensAnaAgentProperty.getInputFileName().concat(modsSensAnaAgentProperty.getInputFileExtension())).getPath()), 
				timeStamp);
	}

}
