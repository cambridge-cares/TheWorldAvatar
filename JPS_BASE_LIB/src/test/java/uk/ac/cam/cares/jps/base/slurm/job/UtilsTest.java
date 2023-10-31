package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

class UtilsTest {

	private static final String TEST_TEXT = "Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n";
	private Path TEMP_DIR;

	@BeforeEach
	void createFolder() throws IOException {
		TEMP_DIR = Files.createTempDirectory("UtilsTest");
	}

	@AfterEach
	void cleanUp() throws IOException {
		try (Stream<Path> pathTree = Files.walk(TEMP_DIR)) {
			pathTree.sorted(Comparator.reverseOrder())
					.map(Path::toFile)
					.forEach(File::delete);
		}
	}

	@Test
	void getTimeStampTest() {
		assertNotEquals(Utils.getTimeStamp(), Utils.getTimeStamp());
	}

	@Test
	void openBufferedWriterTest() throws IOException {

		File tempfile = File.createTempFile("test_file", ".txt", TEMP_DIR.toFile());

		try (BufferedWriter bw = Utils.openBufferedWriter(tempfile.getAbsolutePath())) {
			assertNotNull(bw);
		} finally {
			Files.deleteIfExists(tempfile.toPath());
		}
	}

	@Test
	void openSourceFileTest() throws IOException {
		File tempfile = File.createTempFile("test_file", ".txt", TEMP_DIR.toFile());

		try (BufferedReader br = Utils.openSourceFile(tempfile.getAbsolutePath())) {
			assertNotNull(br);
		} finally {
			Files.deleteIfExists(tempfile.toPath());
		}

	}

	@Test
	void isJobCompletedOneTest() throws IOException {

		Path jobFolder = TEMP_DIR.resolve("login-skylake.hpc.cam.ac.uk_110761971919363");
		Files.createDirectories(jobFolder);
		Path statusFile = jobFolder.resolve(Status.STATUS_FILE.getName());

		// prepare status file for JobStatus:completed
		Files.write(statusFile,
				(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName()).getBytes());
		// test
		assertTrue(Utils.isJobCompleted(jobFolder.toFile()));
		Files.delete(statusFile);

		// prepare status file for JobStatus:error termination
		Files.write(statusFile,
				(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName()).getBytes());
		// test
		assertTrue(Utils.isJobCompleted(jobFolder.toFile()));
		Files.delete(statusFile);

		// prepare status file for fail
		Files.write(statusFile, " ".getBytes());
		// test
		assertFalse(Utils.isJobCompleted(jobFolder.toFile()));
		Files.delete(statusFile);
	}

	@Test
	public void isJobCompletedTwoTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		Path jobFolder_path = taskspace.toPath().resolve("login-skylake.hpc.cam.ac.uk_110761971919363");
		Path status_path = jobFolder_path.resolve(Status.STATUS_FILE.getName());

		// JobStatus:completed and JobOutput:processed
		Files.createDirectories(jobFolder_path);

		Files.write(status_path, (Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName() + "\n"
				+ Status.ATTRIBUTE_JOB_OUTPUT.getName() + Status.OUTPUT_PROCESSED.getName()).getBytes());
		File jobFolder = jobFolder_path.toFile();
		String[] src1 = jobFolder.list();
		File completedJobsDir = Paths.get(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentCompletedJobsSpacePrefix() + taskspace.getName(),
				jobFolder.getName())
				.toFile();

		assertTrue(Utils.isJobCompleted(jobFolder, slurmJobProperty));
		assertTrue(completedJobsDir.isDirectory());
		List<String> completedFiles = Arrays.asList(completedJobsDir.list());
		for (String file : src1) {
			assertTrue(completedFiles.contains(file));
		}
		assertFalse(jobFolder.exists());

		// JobStatus:completed
		Files.createDirectories(jobFolder_path);
		Files.write(status_path,
				(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName() + "\n").getBytes());

		assertTrue(Utils.isJobCompleted(jobFolder, slurmJobProperty));
		Files.delete(status_path);

		// JobStatus: error termination
		Files.write(status_path,
				(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName()).getBytes());
		String[] src2 = jobFolder.list();

		File FailedJobsDir = Paths.get(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentFailedJobsSpacePrefix() + taskspace.getName(),
				jobFolder.getName())
				.toFile();

		assertTrue(Utils.isJobCompleted(jobFolder, slurmJobProperty));
		assertTrue(FailedJobsDir.isDirectory());
		List<String> failedFiles = Arrays.asList(FailedJobsDir.list());
		for (String file : src2) {
			assertTrue(failedFiles.contains(file));
		}
		assertFalse(jobFolder.exists());

		// false scenario
		Files.createDirectories(jobFolder_path);
		Files.write(status_path, " ".getBytes());
		assertFalse(Utils.isJobCompleted(jobFolder, slurmJobProperty));

		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(completedJobsDir.getParentFile());
		FileUtils.forceDelete(FailedJobsDir.getParentFile());
	}

	@Test
	void isJobErroneouslyCompletedOneTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobErroneouslyCompleted(jobFolder));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobErroneouslyCompleted(jobFolder));

		FileUtils.forceDelete(taskspace);

	}

	@Test
	void isJobErroneouslyCompletedTwoTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobErroneouslyCompleted(status_path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobErroneouslyCompleted(status_path));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobRunningOneTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:running
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_RUNNING.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobRunning(jobFolder));
		statusfile1.delete();

		// prepare status file for JobStatus:completing
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETING.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobRunning(jobFolder));
		statusfile2.delete();

		// prepare status file for JobStatus:pending
		File statusfile3 = new File(status_path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_PENDING.getName());
		bw3.close();
		// test
		assertTrue(Utils.isJobRunning(jobFolder));
		statusfile3.delete();

		// prepare status file for fail
		File statusfile4 = new File(status_path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		// test
		assertFalse(Utils.isJobRunning(jobFolder));

		FileUtils.forceDelete(taskspace);

	}

	@Test
	void isJobRunningTwoTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:running
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_RUNNING.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobRunning(status_path));
		statusfile1.delete();

		// prepare status file for JobStatus:completing
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETING.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobRunning(status_path));
		statusfile2.delete();

		// prepare status file for JobStatus:pending
		File statusfile3 = new File(status_path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_PENDING.getName());
		bw3.close();
		// test
		assertTrue(Utils.isJobRunning(status_path));
		statusfile3.delete();

		// prepare status file for fail
		File statusfile4 = new File(status_path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		// test
		assertFalse(Utils.isJobRunning(status_path));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobNotStartedOneTest() throws IOException, NoSuchFieldException, IllegalAccessException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:not started
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_NOT_STARTED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobNotStarted(jobFolder));
		statusfile1.delete();

		// False scenario - 1
		Utils utils = new Utils();
		Field isStatusFileOpen = utils.getClass().getDeclaredField("isStatusFileOpen");
		isStatusFileOpen.setAccessible(true);
		isStatusFileOpen.setBoolean(isStatusFileOpen, true);
		assertFalse(Utils.isJobNotStarted(jobFolder));
		isStatusFileOpen.setBoolean(isStatusFileOpen, false);

		// False scenario - 2
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobNotStarted(jobFolder));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobNotStartedTwoTest() throws IOException, NoSuchFieldException, IllegalAccessException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:not started
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_NOT_STARTED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobNotStarted(status_path));
		statusfile1.delete();

		// False scenario 1
		Utils utils = new Utils();
		Field isStatusFileOpen = utils.getClass().getDeclaredField("isStatusFileOpen");
		isStatusFileOpen.setAccessible(true);
		isStatusFileOpen.setBoolean(isStatusFileOpen, true);
		assertFalse(Utils.isJobNotStarted(status_path));
		isStatusFileOpen.setBoolean(isStatusFileOpen, false);

		// False scenario 2
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobNotStarted(status_path));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobFinishedOneTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobStatus:completed
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobFinished(jobFolder, status_path));
		statusfile1.delete();

		// prepare status file for JobStatus:error termination
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw2.close();
		// test
		assertTrue(Utils.isJobFinished(jobFolder, status_path));
		statusfile2.delete();

		// prepare status file for fail
		File statusfile3 = new File(status_path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(" ");
		bw3.close();
		// test
		assertFalse(Utils.isJobFinished(jobFolder, status_path));
		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobFinishedTwoTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		;
		String jobFolder_path = taskspace.getAbsolutePath() + File.separator
				+ "login-skylake.hpc.cam.ac.uk_110761971919363";
		String status_path = jobFolder_path + File.separator + Status.STATUS_FILE.getName();

		// JobStatus:completed and JobOutput:processed
		File jobFolder1 = new File(jobFolder_path);
		jobFolder1.mkdirs();
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName() + "\n"
				+ Status.ATTRIBUTE_JOB_OUTPUT.getName() + Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		String[] src1 = jobFolder1.list();

		File completedJobsDir_parent = new File(
				Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
						.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(taskspace.getName()));
		File completedJobsDir = new File(completedJobsDir_parent + File.separator + jobFolder1.getName());

		assertTrue(Utils.isJobFinished(jobFolder1, status_path, slurmJobProperty));
		assertTrue(completedJobsDir.isDirectory());
		for (String file : src1) {
			assertTrue(Arrays.asList(completedJobsDir.list()).contains(file));
		}
		assertFalse(jobFolder1.exists());

		// JobStatus:completed
		File jobFolder2 = new File(jobFolder_path);
		jobFolder2.mkdirs();
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_COMPLETED.getName() + "\n");
		bw2.close();

		assertTrue(Utils.isJobFinished(jobFolder2, status_path, slurmJobProperty));
		statusfile2.delete();

		// JobStatus: error termination
		File statusfile3 = new File(status_path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.write(Status.ATTRIBUTE_JOB_STATUS.getName() + Status.STATUS_JOB_ERROR_TERMINATED.getName());
		bw3.close();
		String[] src2 = jobFolder2.list();

		File FailedJobsDir_parent = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(taskspace.getName()));
		File FailedJobsDir = new File(FailedJobsDir_parent + File.separator + jobFolder2.getName());

		assertTrue(Utils.isJobFinished(jobFolder2, status_path, slurmJobProperty));
		assertTrue(FailedJobsDir.isDirectory());
		for (String file : src2) {
			assertTrue(Arrays.asList(FailedJobsDir.list()).contains(file));
		}
		assertFalse(jobFolder2.exists());

		// false scenario
		File jobFolder3 = new File(jobFolder_path);
		jobFolder3.mkdirs();
		File statusfile4 = new File(status_path);
		BufferedWriter bw4 = new BufferedWriter(new FileWriter(statusfile4));
		bw4.write(" ");
		bw4.close();
		assertFalse(Utils.isJobFinished(jobFolder3, status_path, slurmJobProperty));

		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(completedJobsDir_parent);
		FileUtils.forceDelete(FailedJobsDir_parent);

	}

	@Test
	void isJobPostProcessedTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for Jobstatus:error termination
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName() + Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobPostProcessed(jobFolder, status_path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobPostProcessed(jobFolder, status_path));

		FileUtils.forceDelete(taskspace);

	}

	@Test
	void isJobOutputProcessedOneTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobOutput:processed
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName() + Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobOutputProcessed(jobFolder));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobOutputProcessed(jobFolder));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void isJobOutputProcessedTwoTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		// prepare status file for JobOutput:processed
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_OUTPUT.getName() + Status.OUTPUT_PROCESSED.getName());
		bw1.close();
		// test
		assertTrue(Utils.isJobOutputProcessed(status_path));
		statusfile1.delete();

		// prepare status file for fail
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(" ");
		bw2.close();
		// test
		assertFalse(Utils.isJobOutputProcessed(status_path));
		FileUtils.forceDelete(taskspace);
	}

	@Test
	void getJobIdTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();
		String tmpstr = Status.ATTRIBUTE_JOB_ID.getName() + " 28082086";

		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(tmpstr);
		bw1.close();

		String tmpid = null;
		// extract job id
		String tokens[] = tmpstr.trim().toLowerCase().split(":");
		if (tokens.length >= 2 && tokens[1].trim().length() > 0)
			tmpid = tokens[1].trim();

		assertEquals(tmpid, Utils.getJobId(status_path));
		statusfile1.delete();

		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_ID.getName());
		bw2.close();

		assertNull(Utils.getJobId(status_path));

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void addJobIdTest() throws IOException {

		Path taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609");
		File jobFolder = taskspace.resolve("login-skylake.hpc.cam.ac.uk_110761971919363").toFile();
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();
		String jobid = "28082086";

		// prepare status file for addobId method with JobStatus and JobId
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + "\n" + Status.ATTRIBUTE_JOB_ID.getName());
		bw1.close();

		Utils.addJobId(status_path, jobid);

		// test contents of status file
		BufferedReader br1 = new BufferedReader(new InputStreamReader(new FileInputStream(status_path), "UTF-8"));
		String line;
		while ((line = br1.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName() + " " + Status.STATUS_JOB_RUNNING.getName(), line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_ID.getName() + " " + jobid, line);

		}
		br1.close();
		statusfile1.delete();

		// prepare status file for addobId method with only JobId
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_ID.getName());
		bw2.close();

		Utils.addJobId(status_path, jobid);

		// test contents of status file
		BufferedReader br2 = new BufferedReader(new InputStreamReader(new FileInputStream(status_path), "UTF-8"));
		while ((line = br2.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName() + " " + Status.STATUS_JOB_RUNNING.getName(), line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_ID.getName() + " " + jobid, line);
		}
		br2.close();
		statusfile2.delete();

		// prepare status file for addobId method without anything
		File statusfile3 = new File(status_path);
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(statusfile3));
		bw3.close();

		Utils.addJobId(status_path, jobid);

		// test contents of status file
		BufferedReader br3 = new BufferedReader(new InputStreamReader(new FileInputStream(status_path), "UTF-8"));
		while ((line = br3.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName() + " " + Status.STATUS_JOB_RUNNING.getName(), line);
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_ID.getName() + " " + jobid, line);
		}
		br3.close();
	}

	@Test
	void modifyStatusTest() throws IOException {

		Path taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609");
		File jobFolder = taskspace.resolve("login-skylake.hpc.cam.ac.uk_110761971919363").toFile();
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();
		String status1 = Status.STATUS_JOB_RUNNING.getName();
		String status2 = Status.STATUS_JOB_COMPLETED.getName();

		// test for modifyStatus completed -> running
		File statusfile1 = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile1));
		bw1.write(Status.ATTRIBUTE_JOB_STATUS.getName() + status2 + "\n");
		bw1.close();

		Utils.modifyStatus(status_path, status1);

		BufferedReader br1 = new BufferedReader(new InputStreamReader(new FileInputStream(status_path), "UTF-8"));
		String line;
		while ((line = br1.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName() + " " + status1, line);

		}
		br1.close();
		statusfile1.delete();

		// test for modifyStatus running -> completed
		File statusfile2 = new File(status_path);
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(statusfile2));
		bw2.write(Status.ATTRIBUTE_JOB_STATUS.getName() + status1 + "\n");
		bw2.close();

		Utils.modifyStatus(status_path, status2);

		BufferedReader br2 = new BufferedReader(new InputStreamReader(new FileInputStream(status_path), "UTF-8"));
		while ((line = br2.readLine()) != null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName()))
				assertEquals(Status.ATTRIBUTE_JOB_STATUS.getName() + " " + status2, line);

		}
		br2.close();
	}

	@Test
	void getStatusFileTest() throws IOException {

		Path taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609");
		File jobFolder = taskspace.resolve("login-skylake.hpc.cam.ac.uk_110761971919363").toFile();
		jobFolder.mkdirs();
		File jobFolder_null = new File("");
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		File statusfile = new File(status_path);
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(statusfile));
		bw1.close();
		assertEquals(status_path, Utils.getStatusFile(jobFolder).getAbsolutePath());
		assertNull(Utils.getStatusFile(jobFolder_null));
	}

	@Test
	void getLogFilePathOnHPCTest() throws UnknownHostException {

		String runningJob = "login-skylake.hpc.cam.ac.uk_110761971919363";
		String userName = "hpcServerUserName";
		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");

		assertEquals("/home/".concat(userName).concat("/").concat(taskspace.getName()).concat("/")
				.concat(runningJob.replace(hpcAddress, MachineAddress)).concat("/")
				.concat(runningJob.replace(hpcAddress, MachineAddress)).concat(Status.EXTENSION_LOG_FILE.getName()),
				Utils.getLogFilePathOnHPC(runningJob, userName, taskspace, hpcAddress));
	}

	@Test
	void getOutputFilePathOnHPCTest() throws UnknownHostException {

		String runningJob = "login-skylake.hpc.cam.ac.uk_110761971919363";
		String userName = "hpcServerUserName";
		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		String outputFileNameWithExtension = "output.log";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");

		assertEquals("/home/".concat(userName).concat("/").concat(taskspace.getName()).concat("/")
				.concat(runningJob.replace(hpcAddress, MachineAddress)).concat("/").concat(outputFileNameWithExtension),
				Utils.getOutputFilePathOnHPC(runningJob, userName, taskspace, hpcAddress,
						outputFileNameWithExtension));
	}

	@Test
	void getJobFolderPathOnHPCTest() throws UnknownHostException {
		String runningJob = "login-skylake.hpc.cam.ac.uk_110761971919363";
		String userName = "hpcServerUserName";
		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		assertEquals("/home/".concat(userName).concat("/").concat(taskspace.getName()).concat("/")
				.concat(runningJob.replace(hpcAddress, MachineAddress)),
				Utils.getJobFolderPathOnHPC(runningJob, userName, taskspace, hpcAddress));

	}

	@Test
	void getJobOutputFilePathOnAgentPCTest() {

		String runningJob = "login-skylake.hpc.cam.ac.uk_110761971919363";
		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		String outputFileName = "output";
		String outputFileExtension = ".log";
		assertEquals(taskspace.getAbsolutePath().concat(File.separator).concat(runningJob).concat(File.separator)
				.concat(outputFileName).concat(outputFileExtension),
				Utils.getJobOutputFilePathOnAgentPC(runningJob, taskspace, outputFileName, outputFileExtension));
	}

	@Test
	void isErrorTerminationTest() throws IOException {

		File taskspace = TEMP_DIR.resolve("UnitTestAgent_435827288195609").toFile();
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String path = jobFolder.getAbsolutePath() + File.separator + "logFileOnAgentPC";

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

		FileUtils.forceDelete(taskspace);
	}

	@Test
	void getMachineAddressTest() throws UnknownHostException {
		InetAddress address;
		address = InetAddress.getLocalHost();
		String MachineAddress = address.toString().replace("/", "_");
		assertEquals(MachineAddress, Utils.getMachineAddress());
	}

	@Test
	void translateLineEndingIntoUnixTest() throws IOException {
		Path src = TEMP_DIR.resolve("src.txt");
		Path dest = TEMP_DIR.resolve("dest.txt");

		Files.write(src, TEST_TEXT.replace("\n", "\r\n").getBytes());
		Files.write(dest, TEST_TEXT.getBytes());

		Utils.translateLineEndingIntoUnix(src.toFile());

		assertTrue(FileUtils.contentEquals(dest.toFile(), src.toFile()));
	}

	@Test
	void moveToCompletedJobsFolderTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		File statusfile = new File(status_path);
		BufferedWriter bw = new BufferedWriter(new FileWriter(statusfile));
		bw.write("XYZ");
		bw.close();
		String[] src = jobFolder.list();

		File completedJobsDir_parent = new File(
				Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
						.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(taskspace.getName()));
		File completedJobsDir = new File(completedJobsDir_parent + File.separator + jobFolder.getName());

		Utils.moveToCompletedJobsFolder(jobFolder, slurmJobProperty);
		assertTrue(completedJobsDir.isDirectory());
		for (String file : src) {
			assertTrue(Arrays.asList(completedJobsDir.list()).contains(file));
		}
		assertFalse(jobFolder.exists());

		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(completedJobsDir_parent);
	}

	@Test
	void getCompletedJobsDirectoryTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		File completedJobsDir_parent = new File(
				Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
						.concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(taskspace.getName()));
		File completedJobsDir = new File(completedJobsDir_parent + File.separator + jobFolder.getName());

		assertEquals(completedJobsDir, Utils.getCompletedJobsDirectory(jobFolder, slurmJobProperty));
		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(completedJobsDir_parent);
	}

	@Test
	void moveToFailedJobsFolderTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String status_path = jobFolder.getAbsolutePath() + File.separator + Status.STATUS_FILE.getName();

		File statusfile = new File(status_path);
		BufferedWriter bw = new BufferedWriter(new FileWriter(statusfile));
		bw.write("XYZ");
		bw.close();
		String[] src = jobFolder.list();

		File FailedJobsDir_parent = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(taskspace.getName()));
		File FailedJobsDir = new File(FailedJobsDir_parent + File.separator + jobFolder.getName());

		Utils.moveToFailedJobsFolder(jobFolder, slurmJobProperty);
		assertTrue(FailedJobsDir.isDirectory());
		for (String file : src) {
			assertTrue(Arrays.asList(FailedJobsDir.list()).contains(file));
		}
		assertFalse(jobFolder.exists());

		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(FailedJobsDir_parent);

	}

	@Test
	void getFailedJobsDirectoryTest() throws IOException {

		SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
		slurmJobProperty.setAgentClass("UnitTestAgent");
		slurmJobProperty.setAgentCompletedJobsSpacePrefix("CompletedJobs");
		slurmJobProperty.setAgentFailedJobsSpacePrefix("FailedJobs");

		File taskspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(),
				slurmJobProperty.getAgentClass());
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		File FailedJobsDir_parent = new File(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator)
				.concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(taskspace.getName()));
		File FailedJobsDir = new File(FailedJobsDir_parent + File.separator + jobFolder.getName());

		assertEquals(FailedJobsDir, Utils.getFailedJobsDirectory(jobFolder, slurmJobProperty));
		FileUtils.forceDelete(taskspace);
		FileUtils.forceDelete(FailedJobsDir_parent);

	}
}
