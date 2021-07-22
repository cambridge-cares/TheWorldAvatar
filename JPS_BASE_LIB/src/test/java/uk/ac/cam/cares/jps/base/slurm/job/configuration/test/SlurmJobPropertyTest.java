package uk.ac.cam.cares.jps.base.slurm.job.configuration.test;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import org.junit.Test;
import static org.junit.Assert.*;

public class SlurmJobPropertyTest {
	
	SlurmJobProperty test = new SlurmJobProperty();
	
	@Test
	public void getHpcServerLoginUserNameTest() {
		// Default value
		assertNull(test.getHpcServerLoginUserName());
		// Set specific value and check whether it is set correctly
		test.setHpcServerLoginUserName("user");
		assertEquals("user", test.getHpcServerLoginUserName());
	}
	
	@Test
	public void getHpcServerLoginUserPasswordTest() {
		assertNull(test.getHpcServerLoginUserPassword());
		test.setHpcServerLoginUserPassword("user");
		assertEquals("user", test.getHpcServerLoginUserPassword());
	}
	
	@Test
	public void getAgentClassTest() {
		assertNull(test.getAgentClass());
		test.setAgentClass("user");
		assertEquals("user", test.getAgentClass());
	}
	
	@Test
	public void getAgentWorkspacePrefixTest() {
		assertNull(test.getAgentWorkspacePrefix());
		test.setAgentWorkspacePrefix("user");
		assertEquals("user", test.getAgentWorkspacePrefix());
	}
	
	@Test
	public void getAgentCompletedJobsSpacePrefixTest() {
		assertNull(test.getAgentCompletedJobsSpacePrefix());
		test.setAgentCompletedJobsSpacePrefix("user");
		assertEquals("user", test.getAgentCompletedJobsSpacePrefix());
	}
	
	@Test
	public void getHpcAddressTest() {
		assertNull(test.getHpcAddress());
		test.setHpcAddress("user");
		assertEquals("user", test.getHpcAddress());
	}
	
	@Test
	public void getInputFileNameTest() {
		assertNull(test.getInputFileName());
		test.setInputFileName("user");
		assertEquals("user", test.getInputFileName());
	}
	
	@Test
	public void getInputFileExtensionTest() {
		assertNull(test.getInputFileExtension());
		test.setInputFileName("user");
		assertEquals("user", test.getInputFileName());
	}
	
	@Test
	public void getJsonInputFileNameTest() {
		assertNull(test.getJsonInputFileName());
		test.setJsonInputFileName("user");
		assertEquals("user", test.getJsonInputFileName());
	}
	
	@Test
	public void getJsonFileExtensionTest() {
		assertNull(test.getJsonFileExtension());
		test.setJsonFileExtension("user");
		assertEquals("user", test.getJsonFileExtension());
	}
	
	@Test
	public void getSlurmScriptFileNameTest() {
		assertNull(test.getSlurmScriptFileName());
		test.setSlurmScriptFileName("user");
		assertEquals("user", test.getSlurmScriptFileName());
	}
	
	@Test
	public void getOutputFileNameTest() {
		assertNull(test.getOutputFileName());
		test.setOutputFileName("user");
		assertEquals("user", test.getOutputFileName());
	}
	
	@Test
	public void getOutputFileExtensionTest() {
		assertNull(test.getOutputFileExtension());
		test.setOutputFileExtension("user");
		assertEquals("user", test.getOutputFileExtension());
	}
	
	@Test
	public void getExecutableFileTest() {
		assertNull(test.getExecutableFile());
		test.setExecutableFile("user");
		assertEquals("user", test.getExecutableFile());
	}
	
	@Test
	public void getAgentFailedJobsSpacePrefixTest() {
		assertNull(test.getAgentFailedJobsSpacePrefix());
		test.setAgentFailedJobsSpacePrefix("user");
		assertEquals("user", test.getAgentFailedJobsSpacePrefix());
	}
	
	
	@Test
	public void getMaxNumberOfHPCJobsTest() {
		assertEquals(test.getMaxNumberOfHPCJobs(), 0);
		test.setMaxNumberOfHPCJobs(123);
		assertEquals(123, test.getMaxNumberOfHPCJobs());
	}
	
	@Test
	public void getAgentInitialDelayToStartJobMonitoringTest() {
		assertEquals(test.getAgentInitialDelayToStartJobMonitoring(), 0);
		test.setAgentInitialDelayToStartJobMonitoring(123);
		assertEquals(123, test.getAgentInitialDelayToStartJobMonitoring());
	}
	
	@Test
	public void getAgentPeriodicActionIntervalTest() {
		assertEquals(test.getAgentPeriodicActionInterval(), 0);
		test.setAgentPeriodicActionInterval(123);
		assertEquals(123, test.getAgentPeriodicActionInterval());
		
	}

}