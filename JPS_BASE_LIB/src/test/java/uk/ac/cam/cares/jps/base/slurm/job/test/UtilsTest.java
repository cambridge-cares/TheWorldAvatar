package uk.ac.cam.cares.jps.base.slurm.job.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.slurm.job.Property;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.slurm.job.Workspace;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

class UtilsTest {

//	@Test
//	public void getTimeStampTest() {
//		Utils utils = new Utils();
//		utils.previousTimeStamp = System.nanoTime();
//		assertEquals(System.nanoTime(), utils.getTimeStamp());
//		utils.previousTimeStamp = System.nanoTime()+1000;
//		assertEquals(System.nanoTime(), utils.getTimeStamp());
//	}

	@Test
	public void openBufferedWriterTest() throws IOException {
		String tmpdir = System.getProperty("java.io.tmpdir");
		File tempfile = File.createTempFile("test_file", ".txt", new File(tmpdir));

		BufferedWriter bw = Utils.openBufferedWriter(tempfile.getAbsolutePath());
		assertNotNull(bw);
		bw.close();
		tempfile.delete();

	}

	@Test
	public void openSourceFileTest() throws IOException {
		String tmpdir = System.getProperty("java.io.tmpdir");
		File tempfile = File.createTempFile("test_file", ".txt", new File(tmpdir));

		BufferedReader br = Utils.openSourceFile(tempfile.getAbsolutePath());
		assertNotNull(br);
		br.close();
		tempfile.delete();

	}

	@Test
	public void isJobCompletedOneTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");

		// prepare status file for JobStatus:completed
		File statusfile1 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobCompleted(new File(tmpdir)));
		statusfile1.delete();

		// prepare status file for JobStatus:error termination
		File statusfile2 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobCompleted(new File(tmpdir)));
		statusfile2.delete();

		// prepare status file for fail
		File statusfile3 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(" ");
		bw3.close();
		// test
		assertFalse(Utils.isJobCompleted(new File(tmpdir)));
		statusfile3.delete();
	}

	@Test
	public void isJobCompletedTwoTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");
		String tmpdir = System.getProperty("java.io.tmpdir") + "jobFolder\\";
		String path = tmpdir + Status.STATUS_FILE.getName();
		
		// JobStatus:completed and JobOutput:processed
		new File(tmpdir).mkdir();
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName()+"\n");
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName()+Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		String[] src1 = new File(tmpdir).list();
		
		File workspace1 = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());	
		File completedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace1.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));

		assertTrue(Utils.isJobCompleted(new File(tmpdir), slurmJobProperty));
		assertTrue(completedJobsDir.isDirectory());
		for (String file : src1) {
			assertTrue(Arrays.asList(completedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace1.delete();

		// JobStatus:completed
		new File(tmpdir).mkdir();
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName()+"\n");
		bw2.close();

		assertTrue(Utils.isJobCompleted(new File(tmpdir), slurmJobProperty));

		// JobStatus: error termination
		new File(tmpdir).mkdir();
		File statusfile3 = new File(path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw3.close();
		String[] src2 = new File(tmpdir).list();
		
		File workspace2 = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File FailedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(workspace2.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));

		assertTrue(Utils.isJobCompleted(new File(tmpdir), slurmJobProperty));
		assertTrue(FailedJobsDir.isDirectory());
		for (String file : src2) {
			assertTrue(Arrays.asList(FailedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace2.delete();

		// false scenario
		new File(tmpdir).mkdir();
		File statusfile4 = new File(path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		assertFalse(Utils.isJobCompleted(new File(tmpdir), slurmJobProperty));
		statusfile4.delete();
		new File(tmpdir).delete();
	}

	@Test
	public void isJobErroneouslyCompletedOneTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");

		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobErroneouslyCompleted(new File(tmpdir)));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobErroneouslyCompleted(new File(tmpdir)));

	}

	@Test
	public void isJobErroneouslyCompletedTwoTest() throws IOException {

		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();

		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobErroneouslyCompleted(path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobErroneouslyCompleted(path));
		statusfile2.delete();
	}

	@Test
	public void isJobRunningOneTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");

		// prepare status file for JobStatus:running
		File statusfile1 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_RUNNING.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobRunning(new File(tmpdir)));
		statusfile1.delete();

		// prepare status file for JobStatus:completing
		File statusfile2 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETING.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobRunning(new File(tmpdir)));
		statusfile2.delete();

		// prepare status file for JobStatus:pending
		File statusfile3 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_PENDING.getName());
		bw3.close();
		// test
		assertTrue(Utils.isJobRunning(new File(tmpdir)));
		statusfile3.delete();

		// prepare status file for fail
		File statusfile4 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		// test
		assertFalse(Utils.isJobRunning(new File(tmpdir)));
		statusfile4.delete();
	}

	@Test
	public void isJobRunningTwoTest() throws IOException {

		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:running
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_RUNNING.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobRunning(path));
		statusfile1.delete();

		// prepare status file for JobStatus:completing
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETING.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobRunning(path));
		statusfile2.delete();

		// prepare status file for JobStatus:pending
		File statusfile3 = new File(path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_PENDING.getName());
		bw3.close();
		// test
		assertTrue(Utils.isJobRunning(path));
		statusfile3.delete();

		// prepare status file for fail
		File statusfile4 = new File(path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		// test
		assertFalse(Utils.isJobRunning(path));
		statusfile4.delete();
	}

	@Test
	public void isJobNotStartedOneTest() throws IOException, NoSuchFieldException, IllegalAccessException {

		String tmpdir = System.getProperty("java.io.tmpdir");

		// prepare status file for JobStatus:not started
		File statusfile1 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_NOT_STARTED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobNotStarted(new File(tmpdir)));
		statusfile1.delete();

		// False scenario 1
		Utils utils = new Utils();
		Field isStatusFileOpen = utils.getClass().getDeclaredField("isStatusFileOpen");
		isStatusFileOpen.setAccessible(true);
		isStatusFileOpen.setBoolean(isStatusFileOpen, true);
		assertFalse(Utils.isJobNotStarted(new File(tmpdir)));
		isStatusFileOpen.setBoolean(isStatusFileOpen, false);

		// False scenario 2
		File statusfile2 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile1));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobNotStarted(new File(tmpdir)));
		statusfile2.delete();
	}

	@Test
	public void isJobNotStartedTwoTest() throws IOException, NoSuchFieldException, IllegalAccessException {

		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:not started
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_NOT_STARTED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobNotStarted(path));
		statusfile1.delete();

		// False scenario 1
		Utils utils = new Utils();
		Field isStatusFileOpen = utils.getClass().getDeclaredField("isStatusFileOpen");
		isStatusFileOpen.setAccessible(true);
		isStatusFileOpen.setBoolean(isStatusFileOpen, true);
		assertFalse(Utils.isJobNotStarted(path));
		isStatusFileOpen.setBoolean(isStatusFileOpen, false);

		// False scenario 2
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile1));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobNotStarted(path));
		statusfile2.delete();
	}

	@Test
	public void isJobFinishedOneTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");
		String path = tmpdir + Status.STATUS_FILE.getName();
		// prepare status file for JobStatus:completed
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobFinished(new File(tmpdir), path));
		statusfile1.delete();

		// prepare status file for JobStatus:error termination
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobFinished(new File(tmpdir), path));
		statusfile2.delete();

		// prepare status file for fail
		File statusfile3 = new File(path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(" ");
		bw3.close();
		// test
		assertFalse(Utils.isJobFinished(new File(tmpdir), path));
		statusfile3.delete();
	}

	@Test
	public void isJobFinishedTwoTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");
		String tmpdir = System.getProperty("java.io.tmpdir") + "jobFolder\\";
		new File(tmpdir).mkdir();
		String path = tmpdir + Status.STATUS_FILE.getName();
		
		// JobStatus:completed and JobOutput:processed
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName()+"\n");
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName()+Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		String[] src1 = new File(tmpdir).list();
		
		File workspace1 = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File completedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace1.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));

		assertTrue(Utils.isJobFinished(new File(tmpdir), path, slurmJobProperty));
		assertTrue(completedJobsDir.isDirectory());
		for (String file : src1) {
			assertTrue(Arrays.asList(completedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace1.delete();

		// JobStatus:completed
		new File(tmpdir).mkdir();
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName()+"\n");
		bw2.close();

		assertTrue(Utils.isJobFinished(new File(tmpdir), path, slurmJobProperty));

		// JobStatus: error termination
		new File(tmpdir).mkdir();
		File statusfile3 = new File(path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw3.close();
		String[] src2 = new File(tmpdir).list();
		
		File workspace2 = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), slurmJobProperty.getAgentClass());
		File FailedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(workspace2.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));

		assertTrue(Utils.isJobFinished(new File(tmpdir), path, slurmJobProperty));
		assertTrue(FailedJobsDir.isDirectory());
		for (String file : src2) {
			assertTrue(Arrays.asList(FailedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace2.delete();

		// false scenario
		new File(tmpdir).mkdir();
		File statusfile4 = new File(path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		assertFalse(Utils.isJobFinished(new File(tmpdir), path, slurmJobProperty));
		statusfile4.delete();
		new File(tmpdir).delete();

	}

	@Test
	public void isJobPostProcessedTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");
		String path = tmpdir + Status.STATUS_FILE.getName();
		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName()+Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobPostProcessed(new File(tmpdir), path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobPostProcessed(new File(tmpdir), path));

	}

	@Test
	public void isJobOutputProcessedOneTest() throws IOException {

		String tmpdir = System.getProperty("java.io.tmpdir");

		// prepare status file for JobOutput:processed
		File statusfile1 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName()+Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobOutputProcessed(new File(tmpdir)));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(tmpdir + Status.STATUS_FILE.getName());
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobOutputProcessed(new File(tmpdir)));

	}

	@Test
	public void isJobOutputProcessedTwoTest() throws IOException {

		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();

		// prepare status file for JobOutput:processed
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName()+Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobOutputProcessed(path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobOutputProcessed(path));
		statusfile2.delete();
	}

	@Test
	public void getJobIdTest() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();
		String tmpstr = "JobId: 28082086";

		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(tmpstr);
		bw1.close();

		String tmpid = null;
		// extract job id
		String tokens[] = tmpstr.trim().toLowerCase().split(":");
		if (tokens.length >= 2 && tokens[1].trim().length() > 0)
			tmpid = tokens[1].trim();

		assertEquals(tmpid, Utils.getJobId(path));
		statusfile1.delete();

		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write("JobId:");
		bw2.close();

		assertNull(Utils.getJobId(path));
		statusfile2.delete();

	}

	@Test
	public void addJobIdTest() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();
		String jobid = "28082086";

		// prepare status file for addobId method with JobStatus and JobId
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write("JobStatus:\n");
		bw1.write("JobId:");
		bw1.close();

		Utils.addJobId(path, jobid);

		// test contents of status file
		BufferedReader br1 = new BufferedReader(new InputStreamReader(new FileInputStream(path), "UTF-8"));
		String line;
		while ((line = br1.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals("JobStatus: running", line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals("JobId: 28082086", line);

		}
		br1.close();
		statusfile1.delete();

		// prepare status file for addobId method with only JobId
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write("JobId:");
		bw2.close();

		Utils.addJobId(path, jobid);

		// test contents of status file
		BufferedReader br2 = new BufferedReader(new InputStreamReader(new FileInputStream(path), "UTF-8"));
		while ((line = br2.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals("JobStatus: running", line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals("JobId: 28082086", line);

		}
		br2.close();
		statusfile2.delete();

		// prepare status file for addobId method without anything
		File statusfile3 = new File(path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write("");
		bw3.close();

		Utils.addJobId(path, jobid);

		// test contents of status file
		BufferedReader br3 = new BufferedReader(new InputStreamReader(new FileInputStream(path), "UTF-8"));
		while ((line = br3.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals("JobStatus: running", line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals("JobId: 28082086", line);

		}
		br3.close();
		statusfile3.delete();

	}

	@Test
	public void modifyStatusTest() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();
		String status1 = Status.STATUS_JOB_RUNNING.getName();
		String status2 = Status.STATUS_JOB_COMPLETED.getName();

		// test for modifyStatus completed -> running
		File statusfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_COMPLETED.getName()+"\n");
		bw1.close();

		Utils.modifyStatus(path, status1);

		BufferedReader br1 = new BufferedReader(new InputStreamReader(new FileInputStream(path), "UTF-8"));
		String line;
		while ((line = br1.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName()+" "+Status.STATUS_JOB_RUNNING.getName(), line);

		}
		br1.close();
		statusfile1.delete();

		// test for modifyStatus running -> completed
		File statusfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName()+Status.STATUS_JOB_RUNNING.getName()+"\n");
		bw2.close();

		Utils.modifyStatus(path, status2);

		BufferedReader br2 = new BufferedReader(new InputStreamReader(new FileInputStream(path), "UTF-8"));
		while ((line = br2.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName()+" "+Status.STATUS_JOB_COMPLETED.getName(), line);

		}
		br2.close();
		statusfile2.delete();

	}

	@Test
	public void getStatusFileTest() throws IOException {
		String tmpdir1 = System.getProperty("java.io.tmpdir");
		String tmpdir2 = "";
		String path = System.getProperty("java.io.tmpdir") + Status.STATUS_FILE.getName();
		File statusfile = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile));
		bw1.close();
		assertEquals(path, Utils.getStatusFile(new File(tmpdir1)).getAbsolutePath());
		assertNull(Utils.getStatusFile(new File(tmpdir2)));
	}

	@Test
	public void getLogFilePathOnHPCTest() throws UnknownHostException {
		
		String runningJob = "runningJobhpc";
		String userName = "abc";
		File taskSpace =  new File("taskspace");
		String hpcAddress = "hpc";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		
		assertEquals("/home/".concat(userName).concat("/").concat(taskSpace.getName()).concat("/").concat(runningJob.replace(hpcAddress, MachineAddress)).concat("/")
				.concat(runningJob.replace(hpcAddress, MachineAddress)).concat(Status.EXTENSION_LOG_FILE.getName()), Utils.getLogFilePathOnHPC(runningJob, userName, taskSpace, hpcAddress));
	}

	@Test
	public void getOutputFilePathOnHPCTest() throws UnknownHostException {
		
		String runningJob = "runningJobhpc";
		String userName = "abc";
		File taskSpace =  new File("taskspace");
		String hpcAddress = "hpc";
		String outputFileNameWithExtension = "outputFileNameWithExtension.log";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		
		assertEquals("/home/".concat(userName).concat("/").concat(taskSpace.getName()).concat("/").concat(runningJob.replace(hpcAddress, MachineAddress)).concat("/").
				concat(outputFileNameWithExtension), Utils.getOutputFilePathOnHPC(runningJob, userName, taskSpace, hpcAddress, outputFileNameWithExtension));
	}

	@Test
	public void getJobFolderPathOnHPCTest() throws UnknownHostException {
		String runningJob = "runningJobhpc";
		String userName = "abc";
		File taskSpace =  new File("taskspace");
		String hpcAddress = "hpc";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		assertEquals("/home/".concat(userName).concat("/").concat(taskSpace.getName()).concat("/").concat(runningJob.replace(hpcAddress, MachineAddress)), Utils.getJobFolderPathOnHPC(runningJob, userName, taskSpace, hpcAddress));
		
		
	}

	@Test
	public void getJobOutputFilePathOnAgentPCTest() {
		
		String runningJob = "runningJobhpc";
		File taskSpace =  new File("taskspace");
		String outputFileName = "outputFileName";
		String outputFileExtension = ".log";
		assertEquals(taskSpace.getAbsolutePath().concat(File.separator).concat(runningJob).concat(File.separator)
				.concat(outputFileName).concat(outputFileExtension), Utils.getJobOutputFilePathOnAgentPC(runningJob, taskSpace, outputFileName, outputFileExtension));
	}

	@Test
	public void isErrorTerminationTest() throws IOException {
		String path = System.getProperty("java.io.tmpdir") + "logFileOnAgentPC";

		File logfile1 = new File(path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(logfile1));
		bw1.write(Status.JOB_LOG_MSG_ERROR_TERMINATION.getName().toLowerCase());
		bw1.close();
		assertTrue(Utils.isErrorTermination(path));
		logfile1.delete();

		File logfile2 = new File(path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(logfile2));
		bw2.write(" ");
		bw2.close();
		assertFalse(Utils.isErrorTermination(path));
		logfile2.delete();
	}

	@Test
	public void getMachineAddressTest() throws UnknownHostException {
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		assertEquals(MachineAddress, Utils.getMachineAddress());
	}

	@Test
	public void translateLineEndingIntoUnixTest() throws IOException {
		File src = new File(System.getProperty("java.io.tmpdir") + "src.txt");
		File dest = new File(System.getProperty("java.io.tmpdir") + "dest.txt");
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(src));
		bw.write("Lorem ipsum dolor sit amet\rconsectetur adipisci elit,\rsed eiusmod tempor incidunt\rminim veniam, quis nostrum exercitationem ullam corporis suscipit\r" );
		bw.close();
		
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(dest));
		bw1.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\nminim veniam, quis nostrum exercitationem ullam corporis suscipit\n" );
		bw1.close();
		
		Utils.translateLineEndingIntoUnix(src);
		
		assertTrue(FileUtils.contentEquals(dest, src));
		src.delete();		
		dest.delete();
	}

	@Test
	public void copyModifiedContentForUnixTest() throws IOException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		File src = new File("src.txt");
		File dest = new File("dest.txt");
		File expected = new File("expected.txt");
		
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(src));
		bw1.write("Lorem ipsum dolor sit amet \r consectetur adipisci elit,\r sed eiusmod tempor incidunt \r minim veniam, quis nostrum exercitationem ullam corporis suscipit \r" );
		bw1.close();
		
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(expected));
		bw2.write("Lorem ipsum dolor sit amet \n consectetur adipisci elit,\n sed eiusmod tempor incidunt \n minim veniam, quis nostrum exercitationem ullam corporis suscipit \n" );
		bw2.close();
		
		Utils utils = new Utils();
		Method copyModifiedContentForUnix = utils.getClass().getDeclaredMethod("copyModifiedContentForUnix", File.class, File.class);
		copyModifiedContentForUnix.setAccessible(true);
		copyModifiedContentForUnix.invoke(utils, src, dest);
		
		assertTrue(FileUtils.contentEquals(dest, expected));
		src.delete();		
		dest.delete();
		expected.delete();
		
	}

	@Test
	public void moveToCompletedJobsFolderTest() throws IOException {
		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");
		String tmpdir = System.getProperty("java.io.tmpdir") + "jobFolder\\";
		new File(tmpdir).mkdir();
		String path = tmpdir + Status.STATUS_FILE.getName();
		
		File statusfile = new File(path);
		BufferedWriter bw = new BufferedWriter(new FileWriter(statusfile));
		bw.write("XYZ");
		bw.close();
		String[] src = new File(tmpdir).list();
		
		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File completedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));
		
		Utils.moveToCompletedJobsFolder(new File(tmpdir), slurmJobProperty);
		assertTrue(completedJobsDir.isDirectory());
		for (String file : src) {
			assertTrue(Arrays.asList(completedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace.delete();
	}

	@Test
	public void getCompletedJobsDirectoryTest() throws IOException {
		String tmpdir = System.getProperty("java.io.tmpdir");
		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File completedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));
		
		assertEquals(completedJobsDir, Utils.getCompletedJobsDirectory(new File(tmpdir), slurmJobProperty));
		workspace.delete();
	}

	@Test
	public void moveToFailedJobsFolderTest() throws IOException {
		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");
		String tmpdir = System.getProperty("java.io.tmpdir") + "jobFolder\\";
		new File(tmpdir).mkdir();
		String path = tmpdir + Status.STATUS_FILE.getName();
		
		File statusfile = new File(path);
		BufferedWriter bw = new BufferedWriter(new FileWriter(statusfile));
		bw.write("XYZ");
		bw.close();
		String[] src = new File(tmpdir).list();
		
		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File FailedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));
		
		Utils.moveToFailedJobsFolder(new File(tmpdir), slurmJobProperty);
		assertTrue(FailedJobsDir.isDirectory());
		for (String file : src) {
			assertTrue(Arrays.asList(FailedJobsDir.list()).contains(file));
		}
		assertFalse(new File(tmpdir).exists());
		workspace.delete();
	}

	@Test
	public void getFailedJobsDirectoryTest() throws IOException {
		String tmpdir = System.getProperty("java.io.tmpdir");
		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File FailedJobsDir = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName())
				.concat(File.separator).concat(new File(tmpdir).getName()));
		
		assertEquals(FailedJobsDir, Utils.getFailedJobsDirectory(new File(tmpdir), slurmJobProperty));
		workspace.delete();
	}
}
