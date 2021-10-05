package uk.ac.cam.cares.jps.agent.mechanism.moo.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMooAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.moo.MooResultsProcess;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgent;
import uk.ac.cam.cares.jps.agent.mechanism.moo.MoDSMooAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.moo.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;

public class MoDSMooAgentProcessOutputTest extends MoDSMooAgent {

	/**
	 * Performs a test of the agent's job post-processing using a sample output.zip archive. 
	 */
	private static final long serialVersionUID = -6832040567739527313L;
	private String tempFolderPath;
	
	@Test
	public void test() throws IOException, MoDSMooAgentException {
		String tempFolderName = "temp_job_"+Utils.getTimeStamp();
		tempFolderPath = System.getProperty("user.home").concat(File.separator).concat("."+tempFolderName);
		initAgentProperty();
		
		File zipFile = new File(getClass().getClassLoader()
				.getResource(MoDSMooAgentProperty.getOutputFileName().concat(MoDSMooAgentProperty.getOutputFileExtension()))
				.getPath());
		boolean success = selectSensRxnsTest(zipFile);
		Assert.assertTrue("Post_processing failed.", success);
		
		// delete the generated temporary folder
		try {
			deleteDirectory(new File(tempFolderPath));
		} catch (IOException e) {
			e.printStackTrace();
			Assert.fail("Failed to delete temporary job folder.");
		}
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
		// initialising classes to read properties from the modsmoo-agent.properties file
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
	
	private boolean selectSensRxnsTest(File zipFile) throws IOException, MoDSMooAgentException {
		String zipFilePath = zipFile.getAbsolutePath();
		String destDir = tempFolderPath.concat(File.separator).concat(MoDSMooAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(getClass().getClassLoader()
				.getResource(MoDSMooAgentProperty.getJsonInputFileName().concat(MoDSMooAgentProperty.getJsonFileExtension()))
				.getPath()));
		JsonNode inputNode = new ObjectMapper().readTree(jsonString);
		
		MooResultsProcess dataDrivenRePro = new MooResultsProcess(MoDSMooAgentProperty);
		List<String> selectedRxns = dataDrivenRePro.processResults(destDir, jsonString);
		
		if (selectedRxns != null && !selectedRxns.isEmpty()) {
			updateJsonForCalib(inputNode, selectedRxns, destDir.concat(File.separator).concat("modifiedInput.json"));
			return true;
		}
		return false;
	}
}
