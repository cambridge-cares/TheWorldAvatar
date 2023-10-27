package uk.ac.cam.cares.jps.base.slurm.job;

import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;

public class SlurmJobTest {
	
	SlurmJob test = new SlurmJob();
	
	//Test setJobFolderName() and getJobFolderName()
	@Test
	public void testSetJobFolderName() {
		test.setJobFolderName("Example_One");
		Assert.assertEquals("Example_One", test.getJobFolderName());
	}

	//Test getNewJobFolderName()
	@Test
	public void testGetNewJobFolderName() {
		String folderName = "Example_Two_1234567";
		try (MockedStatic<Utils> theMock = Mockito.mockStatic(Utils.class)) {
			theMock.when(Utils::getTimeStamp).thenReturn(1234567L);
			Assert.assertEquals(folderName, test.getNewJobFolderName("Example_Two"));
		}
		//Assert.assertEquals("Example_Two_".concat("" + Utils.getTimeStamp()), test.getNewJobFolderName("Example_Two"));
	}
}

