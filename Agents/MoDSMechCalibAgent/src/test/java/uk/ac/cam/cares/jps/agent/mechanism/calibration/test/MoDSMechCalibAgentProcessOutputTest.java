package uk.ac.cam.cares.jps.agent.mechanism.calibration.test;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MechCalibOutputProcess;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgent;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.kg.OntoKinKG;

public class MoDSMechCalibAgentProcessOutputTest extends MoDSMechCalibAgent {

	/**
	 * Performs a test of the agent's job post-processing using a sample output.zip archive. 
	 */
	private static final long serialVersionUID = 3751007418827774854L;
	private String tempFolderPath;

	@Test
	public void test() throws IOException, MoDSMechCalibAgentException {
		String tempFolderName = "temp_job_"+Utils.getTimeStamp();
		tempFolderPath = System.getProperty("user.home").concat(File.separator).concat("."+tempFolderName);
		initAgentProperty();
		
		File zipFile = new File(getClass().getClassLoader()
				.getResource(modsMechCalibAgentProperty.getOutputFileName().concat(modsMechCalibAgentProperty.getOutputFileExtension()))
				.getPath());
		String mechOwlOnServer = updateCalibMechTest(zipFile);
		Assert.assertTrue("Post-processing failed.", mechOwlOnServer!=null && !mechOwlOnServer.isEmpty());
		
		// delete the uploaded mechanism after testing
		OntoKinKG ontoKinKg = new OntoKinKG(modsMechCalibAgentProperty);
		boolean success = ontoKinKg.deleteMechanism(mechOwlOnServer.substring(mechOwlOnServer.lastIndexOf("/")+1), mechOwlOnServer);
		if (!success) {
			Assert.fail("Failed to delete the uploaded mechanism.");
		}
		
		// delete the generated temporary folder
		try {
			deleteDirectory(new File(tempFolderPath));
		} catch (IOException e) {
			e.printStackTrace();
			Assert.fail("Failed to delete temporary job folder.");
		}
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
	 * Update the mechanism with optimised rate parameters. 
	 * 
	 * @param zipFile
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	private String updateCalibMechTest(File zipFile) throws IOException, MoDSMechCalibAgentException {
		String zipFilePath = zipFile.getAbsolutePath();
		String destDir = tempFolderPath.concat(File.separator).concat(modsMechCalibAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(getClass().getClassLoader()
				.getResource(modsMechCalibAgentProperty.getJsonInputFileName().concat(modsMechCalibAgentProperty.getJsonFileExtension()))
				.getPath()));
		
		MechCalibOutputProcess mechCalibPro = new MechCalibOutputProcess(modsMechCalibAgentProperty);
		return mechCalibPro.processResults(destDir, jsonString);
	}
}
