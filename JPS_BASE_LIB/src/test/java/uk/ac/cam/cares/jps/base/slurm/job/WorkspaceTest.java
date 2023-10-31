package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class WorkspaceTest {

	private static final String TEST_TEXT = "Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n";
	private Path TEMP_DIR;

	@BeforeEach
	void createFolder() throws IOException {
		TEMP_DIR = Files.createTempDirectory("WorkspaceTest");
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
	void getExistingWorkspaceTest() throws IOException {
		String agentClass = "UnitTestAgent";
		Path workspace = TEMP_DIR.resolve(agentClass + "_" + System.nanoTime());
		Files.createDirectories(workspace);

		assertEquals(workspace.toAbsolutePath(),
				Workspace.getWorkspace(TEMP_DIR.toString(), agentClass).toPath().toAbsolutePath());

		Files.delete(workspace);
	}

	@Test
	void getNewWorkspaceTest() throws IOException {
		String agentClass = "UnitTestAgent";
		assertEquals(TEMP_DIR.resolve(agentClass).toString(),
				Workspace.getWorkspace(TEMP_DIR.toString(), agentClass).getAbsolutePath().split("_")[0]);
	}

	@Test
	void isWorkspaceAvailableTestTwo()
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException, IOException {

		String agentClass = "UnitTestAgent";
		Path ws = TEMP_DIR.resolve(agentClass + "_" + System.nanoTime());
		Files.createDirectories(ws);

		Workspace workspace = new Workspace();

		assertTrue(workspace.isWorkspaceAvailable(TEMP_DIR.toFile(), agentClass));
		Files.delete(ws);
		assertFalse(workspace.isWorkspaceAvailable(TEMP_DIR.toFile(), agentClass));
	}

	@Test
	void createJSONInputFileTest() throws IOException {

		Path workspaceFolder = TEMP_DIR;
		Path jsonInputFilePath = workspaceFolder.resolve("input.json").toAbsolutePath();
		Path jsonInputFilePath_temp = workspaceFolder.resolve("input_temp.json").toAbsolutePath();
		Workspace workspace = new Workspace();

		Files.write(jsonInputFilePath_temp, TEST_TEXT.getBytes());

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.createJSONInputFile(workspaceFolder.toFile(), jsonInputFilePath.toString(), TEST_TEXT));
		assertTrue(FileUtils.contentEquals(jsonInputFilePath.toFile(), jsonInputFilePath_temp.toFile()));
	}

	@Test
	void getInputFilePathTest() throws IOException {

		Path taskSpace = TEMP_DIR.resolve("UnitTestAgent_435827288195609");
		Path jobFolder = taskSpace.resolve("login-skylake.hpc.cam.ac.uk_110761971919363");
		Files.createDirectories(jobFolder);
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		String inputFileExtension = ".com";
		String[] tokens = jobFolder.getFileName().toString().split("_");
		String timeStampPart = null;
		if (tokens.length == 2 && tokens[1] != null && StringUtils.isNumeric(tokens[1]))
			timeStampPart = tokens[1];
		Workspace workspace = new Workspace();
		assertEquals(
				jobFolder.resolve(hpcAddress + "_" + timeStampPart + inputFileExtension).toAbsolutePath().toString(),
				workspace.getInputFilePath(jobFolder.toFile(), hpcAddress, inputFileExtension));
	}

	@Test
	void getInputFileExtension() throws SlurmJobException, IOException {

		Path input = TEMP_DIR.resolve("input.csv");

		Files.createFile(input);

		Workspace workspace = new Workspace();

		assertEquals(input.toString().substring(input.toString().lastIndexOf('.')),
				workspace.getInputFileExtension(input.toFile()));
	}

	@Test
	void createJobFolderTest() throws IOException {
		Path workspacePath = TEMP_DIR;
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		long timeStamp = System.nanoTime();
		Workspace workspace = new Workspace();
		Path jobFolder = workspacePath.resolve(hpcAddress + "_" + timeStamp);

		assertEquals(jobFolder.toAbsolutePath(),
				workspace.createJobFolder(workspacePath.toString(), hpcAddress, timeStamp).toPath().toAbsolutePath());
		assertNull(workspace.createJobFolder(workspacePath.toString(), hpcAddress, timeStamp));
	}

	@Test
	void createInputFileTest() throws IOException {

		Path inputFileDestinationPath = TEMP_DIR.resolve("input_dest");
		Path input = TEMP_DIR.resolve("input_src");
		Workspace workspace = new Workspace();

		Files.write(input, TEST_TEXT.getBytes());

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.createInputFile(inputFileDestinationPath.toString(), input.toFile()));
		assertTrue(FileUtils.contentEquals(input.toFile(), inputFileDestinationPath.toFile()));
	}

	@Test
	void createStatusFileTest() throws IOException {

		Path workspaceFolder = TEMP_DIR;
		Path statusFilePath = workspaceFolder.resolve(Status.STATUS_FILE.getName());
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		Path tempFile = workspaceFolder.resolve("temp.txt");
		Workspace workspace = new Workspace();

		try (BufferedWriter bw = new BufferedWriter(new FileWriter(tempFile.toFile()))) {
			bw.write(Status.ATTRIBUTE_JOB_STATUS.getName() + " ");
			bw.write(Status.STATUS_JOB_NOT_STARTED.getName() + "\n");
			bw.write(Status.ATTRIBUTE_JOB_ID.getName() + "\n");
			bw.write(Status.ATTRIBUTE_AGENT_ID.getName() + " ");
			bw.write(workspaceFolder.getFileName() + "\n");
			bw.write(Status.ATTRIBUTE_HPC_ADDRESS.getName() + " ");
			bw.write(hpcAddress + "\n");
			bw.write(Status.ATTRIBUTE_JOB_OUTPUT.getName() + "\n");
		}

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.createStatusFile(workspaceFolder.toFile(), statusFilePath.toString(), hpcAddress));
		assertTrue(FileUtils.contentEquals(statusFilePath.toFile(), tempFile.toFile()));
	}

	@Test
	void copyFileTestOne() throws IOException {

		Path destination = TEMP_DIR.resolve("dest");
		Path source = TEMP_DIR.resolve("src");
		Path temp = TEMP_DIR.resolve("temp");
		Workspace workspace = new Workspace();

		Files.write(source, TEST_TEXT.getBytes());

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.copyFile(source.toString(), destination.toString()));
		assertTrue(FileUtils.contentEquals(source.toFile(), destination.toFile()));

		Files.delete(destination);

		Path destination_sh = TEMP_DIR.resolve("dest.sh");
		Files.write(temp, TEST_TEXT.getBytes());

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.copyFile(source.toString(), destination_sh.toString()));
		assertTrue(FileUtils.contentEquals(temp.toFile(), destination_sh.toFile()));
	}

	@Test
	void getTimeStampPartTest() {

		String folder = "login-skylake.hpc.cam.ac.uk_428109593378500";
		String folder_null = "";
		Workspace workspace = new Workspace();
		String[] tokens = folder.split("_");
		assertEquals(tokens[1], workspace.getTimeStampPart(folder));
		assertNull(workspace.getTimeStampPart(folder_null));
	}

	@Test
	void getStatusFilePathTest() {

		Path jobFolder = TEMP_DIR;
		Workspace workspace = new Workspace();
		String expected = jobFolder.resolve(Property.STATUS_FILE_NAME.getPropertyName()).toAbsolutePath().toString();
		assertEquals(expected, workspace.getStatusFilePath(jobFolder.toFile()));
	}

	@Test
	void getJSONInputFilePathTest() {

		Path jobFolder = TEMP_DIR;
		Workspace workspace = new Workspace();
		String expected = jobFolder.resolve(Property.JSON_INPUT_FILE_NAME.getPropertyName()).toAbsolutePath()
				.toString();
		assertEquals(expected, workspace.getJSONInputFilePath(jobFolder.toFile()));
	}

	@Test
	void copyScriptFileTest() throws IOException {

		Path source = TEMP_DIR.resolve("source");
		Path dest = TEMP_DIR.resolve("dest");
		Files.createDirectories(dest);

		String slurmScriptFileName = "slurmscript.sh";
		Path temp = TEMP_DIR.resolve("temp");
		Workspace workspace = new Workspace();

		Files.write(source, TEST_TEXT.getBytes());
		Files.write(temp, TEST_TEXT.getBytes());

		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(),
				workspace.copyScriptFile(source.toString(), dest.toString(), slurmScriptFileName));
		Path slurmScriptPath = dest.resolve(slurmScriptFileName);
		assertTrue(FileUtils.contentEquals(temp.toFile(), slurmScriptPath.toFile()));
	}
}
