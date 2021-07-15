package uk.ac.cam.cares.jps.base.slurm.job.configuration.test;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import org.junit.Test;
import static org.junit.Assert.*;

public class SlurmJobPropertyTest{ 
	
	SlurmJobProperty test = new SlurmJobProperty();
	
	@Test
	public void getHpcServerLoginUserNameTest() {
		assertNull(test.getHpcServerLoginUserName());
	}
	
	@Test
	public void getHpcServerLoginUserPasswordTest() {
		assertNull(test.getHpcServerLoginUserPassword());
	}
	
	@Test
	public void getAgentClassTest() {
		assertNull(test.getAgentClass());
	}
	
	@Test
	public void getAgentWorkspacePrefixTest() {
		assertNull(test.getAgentWorkspacePrefix());
	}
	
	@Test
	public void getAgentCompletedJobsSpacePrefixTest() {
		assertNull(test.getAgentCompletedJobsSpacePrefix());
	}
	
	@Test
	public void getHpcAddressTest() {
		assertNull(test.getHpcAddress());
	}
	
	@Test
	public void getInputFileNameTest() {
		assertNull(test.getInputFileName());
	}
	
	@Test
	public void getInputFileExtensionTest() {
		assertNull(test.getInputFileExtension());
	}
	
	@Test
	public void getJsonInputFileNameTest() {
		assertNull(test.getJsonInputFileName());
	}
	
	@Test
	public void getJsonFileExtensionTest() {
		assertNull(test.getJsonFileExtension());
	}
	
	@Test
	public void getSlurmScriptFileNameTest() {
		assertNull(test.getSlurmScriptFileName());
	}
	
	@Test
	public void getOutputFileNameTest() {
		assertNull(test.getOutputFileName());
	}
	
	@Test
	public void getOutputFileExtensionTest() {
		assertNull(test.getOutputFileExtension());
	}
	
	@Test
	public void getExecutableFileTest() {
		assertNull(test.getExecutableFile());
	}
	
	@Test
	public void getAgentFailedJobsSpacePrefixTest() {
		assertNull(test.getAgentFailedJobsSpacePrefix());
	}
	
	
	@Test
	public void getMaxNumberOfHPCJobsTest() {
		assertEquals(test.getMaxNumberOfHPCJobs(), 0);
	}
	
	@Test
	public void getAgentInitialDelayToStartJobMonitoringTest() {
		assertEquals(test.getAgentInitialDelayToStartJobMonitoring(), 0);
	}
	
	@Test
	public void getAgentPeriodicActionIntervalTest() {
		assertEquals(test.getAgentPeriodicActionInterval(), 0);
	}

}