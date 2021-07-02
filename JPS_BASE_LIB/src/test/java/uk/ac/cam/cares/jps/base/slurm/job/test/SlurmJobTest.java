package uk.ac.cam.cares.jps.base.slurm.job.test;

import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import junit.framework.TestCase;

public class SlurmJobTest extends TestCase{
	
	SlurmJob Test = new SlurmJob();
	
	//Test setJobFolderName() and getJobFolderName()
	public void testsetJobFolderName() {
		Test.setJobFolderName("Example_One");
		assertEquals("Example_One",Test.getJobFolderName());
	}
	//Test getNewJobFolderName()
	public void testgetNewJobFolderName() {
		
		assertEquals("Example_Two_".concat("" + Utils.getTimeStamp()),Test.getNewJobFolderName("Example_Two"));
	}
}

