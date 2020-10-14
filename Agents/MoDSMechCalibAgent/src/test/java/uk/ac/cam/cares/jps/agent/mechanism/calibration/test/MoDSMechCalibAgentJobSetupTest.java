package uk.ac.cam.cares.jps.agent.mechanism.calibration.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import junit.framework.Assert;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgent;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class MoDSMechCalibAgentJobSetupTest extends MoDSMechCalibAgent {

	/**
	 * Performs a test of the agent's job setup using a sample JSON request (read from a resource file). 
	 */
	private static final long serialVersionUID = 3366455762189966807L;

	@Test
	public void test() throws IOException, MoDSMechCalibAgentException, SlurmJobException {
		initAgentProperty();
		
		BufferedReader br = FileUtil.openSourceFile(getClass().getClassLoader().getResource(modsMechCalibAgentProperty.getJsonInputFileName().concat(modsMechCalibAgentProperty.getJsonFileExtension())).getPath());
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
	 * Initialises the unique instance of the MoDSMechCalibAgentProperty class that<br>
	 * reads all properties of MoDSMechCalibAgent from the modsmechcalib-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modsmechcalib-agent property file<br>
	 * through the MoDSMechCalibAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modsmechcalib-agent.properties file
		if (applicationContextMoDSMechCalibAgent == null) {
			applicationContextMoDSMechCalibAgent = new AnnotationConfigApplicationContext(MoDSMechCalibAgentConfiguration.class);
		}
		if (modsMechCalibAgentProperty == null) {
			modsMechCalibAgentProperty = applicationContextMoDSMechCalibAgent.getBean(MoDSMechCalibAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsMechCalibAgentProperty.getAgentClass(), modsMechCalibAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(modsMechCalibAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(modsMechCalibAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(modsMechCalibAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(modsMechCalibAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(modsMechCalibAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(modsMechCalibAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(modsMechCalibAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(modsMechCalibAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(modsMechCalibAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(modsMechCalibAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(modsMechCalibAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(modsMechCalibAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(modsMechCalibAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(modsMechCalibAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(modsMechCalibAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(modsMechCalibAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSMechCalibAgentException, SlurmJobException {
		long timeStamp = Utils.getTimeStamp();
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(modsMechCalibAgentProperty.getSlurmScriptFileName()).getPath()), 
				new File(getClass().getClassLoader().getResource(modsMechCalibAgentProperty.getInputFileName().concat(modsMechCalibAgentProperty.getInputFileExtension())).getPath()), 
				timeStamp);
	}
}
