package uk.ac.cam.cares.jps.agent.mechanism.datadriven.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Ignore;

import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSDataDrivenAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.datadriven.DataDrivenResultsProcess;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgent;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.MoDSDataDrivenAgentException;
import uk.ac.cam.cares.jps.agent.mechanism.datadriven.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;

public class MoDSDataDrivenAgentProcessOutputTest extends MoDSDataDrivenAgent {

	/**
	 * Performs a test of the agent's job post-processing using a sample output.zip archive. 
	 */
	private static final long serialVersionUID = -6832040567739527313L;
	private String tempFolderPath;
	
	@Test
        @Ignore
	public void test() throws IOException, MoDSDataDrivenAgentException {
		String tempFolderName = "temp_job_"+Utils.getTimeStamp();
		tempFolderPath = System.getProperty("user.home").concat(File.separator).concat("."+tempFolderName);
		initAgentProperty();
		
		File zipFile = new File(getClass().getClassLoader()
				.getResource(modsDataDrivenAgentProperty.getOutputFileName().concat(modsDataDrivenAgentProperty.getOutputFileExtension()))
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
	
	private boolean selectSensRxnsTest(File zipFile) throws IOException, MoDSDataDrivenAgentException {
		String zipFilePath = zipFile.getAbsolutePath();
		String destDir = tempFolderPath.concat(File.separator).concat(modsDataDrivenAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(getClass().getClassLoader()
				.getResource(modsDataDrivenAgentProperty.getJsonInputFileName().concat(modsDataDrivenAgentProperty.getJsonFileExtension()))
				.getPath()));
		JsonNode inputNode = new ObjectMapper().readTree(jsonString);
		
		DataDrivenResultsProcess dataDrivenRePro = new DataDrivenResultsProcess(modsDataDrivenAgentProperty);
		List<String> selectedRxns = dataDrivenRePro.processResults(destDir, jsonString);
		
		if (selectedRxns != null && !selectedRxns.isEmpty()) {
			updateJsonForCalib(inputNode, selectedRxns, destDir.concat(File.separator).concat("modifiedInput.json"));
			return true;
		}
		return false;
	}
}
