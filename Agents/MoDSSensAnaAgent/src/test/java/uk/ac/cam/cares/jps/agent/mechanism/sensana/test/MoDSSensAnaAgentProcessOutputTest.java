package uk.ac.cam.cares.jps.agent.mechanism.sensana.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana.SensAnaResultsProcess;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgent;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;

public class MoDSSensAnaAgentProcessOutputTest extends MoDSSensAnaAgent {

	/**
	 * Performs a test of the agent's job post-processing using a sample output.zip archive. 
	 */
	private static final long serialVersionUID = -6832040567739527313L;
	private String tempFolderPath;
	
	@Test
	public void test() throws IOException, MoDSSensAnaAgentException {
		String tempFolderName = "temp_job_"+Utils.getTimeStamp();
		tempFolderPath = System.getProperty("user.home").concat(File.separator).concat("."+tempFolderName);
		initAgentProperty();
		
		File zipFile = new File(getClass().getClassLoader()
				.getResource(modsSensAnaAgentProperty.getOutputFileName().concat(modsSensAnaAgentProperty.getOutputFileExtension()))
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
	
	private boolean selectSensRxnsTest(File zipFile) throws IOException, MoDSSensAnaAgentException {
		String zipFilePath = zipFile.getAbsolutePath();
		String destDir = tempFolderPath.concat(File.separator).concat(modsSensAnaAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(getClass().getClassLoader()
				.getResource(modsSensAnaAgentProperty.getJsonInputFileName().concat(modsSensAnaAgentProperty.getJsonFileExtension()))
				.getPath()));
		JsonNode inputNode = new ObjectMapper().readTree(jsonString);
		
		SensAnaResultsProcess sensAnaRePro = new SensAnaResultsProcess(modsSensAnaAgentProperty);
		List<String> selectedRxns = sensAnaRePro.processResults(destDir, jsonString);
		
		if (selectedRxns != null && !selectedRxns.isEmpty()) {
			updateJsonForCalib(inputNode, selectedRxns, destDir.concat(File.separator).concat("modifiedInput.json"));
			return true;
		}
		return false;
	}
}
