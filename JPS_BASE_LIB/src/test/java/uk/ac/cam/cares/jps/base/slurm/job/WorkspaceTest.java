package uk.ac.cam.cares.jps.base.slurm.job;


import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

class WorkspaceTest {
	
	@Test
	public void getWorkspaceTest() throws IOException {
		
		String workspaceParentPath = System.getProperty("java.io.tmpdir").toString();
		String agentClass = "UnitTestAgent";
		File workspace = new File(workspaceParentPath.concat(File.separator).concat(agentClass).concat("_").concat(""+System.nanoTime()));
		workspace.mkdir();
		
		assertEquals(workspace.getAbsolutePath(), Workspace.getWorkspace(workspaceParentPath, agentClass).getAbsolutePath());
		
		workspace.delete();
		
		assertEquals(workspaceParentPath.concat(agentClass), Workspace.getWorkspace(workspaceParentPath, agentClass).getAbsolutePath().split("_")[0]);
		Workspace.getWorkspace(workspaceParentPath, agentClass).delete();
		assertNotNull(Workspace.getWorkspace(workspaceParentPath, agentClass));
		
		
		Workspace.getWorkspace(workspaceParentPath, agentClass).delete();
	}
	
	@Test
	public void createWorkspaceNameTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		
		String workspaceParentPath = System.getProperty("java.io.tmpdir").toString();
		String agentClass = "UnitTestAgent";
		Workspace workspace = new Workspace();
		Method createWorkspaceName = workspace.getClass().getDeclaredMethod("createWorkspaceName", String.class, String.class);
		createWorkspaceName.setAccessible(true);
		File wspace = (File) createWorkspaceName.invoke(workspace, workspaceParentPath, agentClass);
		assertEquals(workspaceParentPath.concat(agentClass), wspace.getAbsolutePath().split("_")[0]);
		assertNotNull(createWorkspaceName.invoke(workspace, workspaceParentPath, agentClass));
		Workspace.getWorkspace(workspaceParentPath, agentClass).delete();
		Workspace.getWorkspace(workspaceParentPath, agentClass).delete();
		
	}
	
	@Test
	public void isWorkspaceAvailableTestOne() throws IllegalAccessException, InvocationTargetException, NoSuchMethodException{
		
		String workspaceParentPath = System.getProperty("java.io.tmpdir").toString();
		String agentClass = "UnitTestAgent";
		File ws = new File(workspaceParentPath.concat(File.separator).concat(agentClass).concat("_").concat(""+System.nanoTime()));
		ws.mkdir();
		Workspace workspace = new Workspace();
		Method isWorkspaceAvailable = workspace.getClass().getDeclaredMethod("isWorkspaceAvailable", String.class, String.class);
		isWorkspaceAvailable.setAccessible(true);
		
		assertTrue((boolean)isWorkspaceAvailable.invoke(workspace, workspaceParentPath, agentClass));
		ws.delete();
		assertFalse((boolean)isWorkspaceAvailable.invoke(workspace, workspaceParentPath, agentClass));
		
	}
	
	@Test
	public void isWorkspaceAvailableTestTwo() throws IllegalAccessException, InvocationTargetException, NoSuchMethodException{
		
		String workspaceParentPath = System.getProperty("java.io.tmpdir").toString();
		String agentClass = "UnitTestAgent";
		File ws = new File(workspaceParentPath.concat(File.separator).concat(agentClass).concat("_").concat(""+System.nanoTime()));
		ws.mkdir();
		
		Workspace workspace = new Workspace();
		Method isWorkspaceAvailable = workspace.getClass().getDeclaredMethod("isWorkspaceAvailable", File.class, String.class);
		isWorkspaceAvailable.setAccessible(true);
		
		assertTrue((boolean)isWorkspaceAvailable.invoke(workspace, new File(workspaceParentPath), agentClass));
		ws.delete();
		assertFalse((boolean)isWorkspaceAvailable.invoke(workspace, new File(workspaceParentPath), agentClass));
	}
	
	@Test
	public void createJSONInputFileTest() throws IOException {
		
		File workspaceFolder = new File(System.getProperty("java.io.tmpdir"));
		String jsonInputFilePath = workspaceFolder.getAbsolutePath()+File.separator+"input.json";
		String jsonInputFilePath_temp = workspaceFolder.getAbsolutePath()+File.separator+"input_temp.json";
		String jsonString = "Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n";
		Workspace workspace = new Workspace();
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(new File(jsonInputFilePath_temp)));
		bw.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n");
		bw.close();
		
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.createJSONInputFile(workspaceFolder, jsonInputFilePath, jsonString));
		assertTrue(FileUtils.contentEquals(new File(jsonInputFilePath), new File(jsonInputFilePath_temp)));
		
		new File(jsonInputFilePath).delete();
		new File(jsonInputFilePath_temp).delete();
	}
	
	@Test
	public void getInputFilePathTest() throws IOException {
		
		File taskspace = new File(System.getProperty("java.io.tmpdir") + "UnitTestAgent_435827288195609");
		File jobFolder = new File(taskspace + File.separator + "login-skylake.hpc.cam.ac.uk_110761971919363");
		jobFolder.mkdirs();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		String inputFileExtension = ".com";
		String[] tokens = jobFolder.getName().split("_");
		String timeStampPart = null;
		if(tokens.length==2 && tokens[1]!=null && StringUtils.isNumeric(tokens[1]))
			 timeStampPart = tokens[1];
		Workspace workspace = new Workspace();
		assertEquals(jobFolder.getAbsolutePath()+File.separator+hpcAddress+"_"+timeStampPart+inputFileExtension, workspace.getInputFilePath(jobFolder, hpcAddress, inputFileExtension));
		
		FileUtils.forceDelete(taskspace);
	}
	
	@Test
	public void getInputFileExtension() throws SlurmJobException, IOException {
		
		File input = new File(System.getProperty("java.io.tmpdir")+File.separator+"input.csv");
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(input));
		bw.close();
		Workspace workspace = new Workspace();
		
		assertEquals(input.getAbsolutePath().substring(input.getAbsolutePath().lastIndexOf('.')), workspace.getInputFileExtension(input));
	}
	
	@Test
	public void createJobFolderTest() {
		String workspacePath = System.getProperty("java.io.tmpdir").toString();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		long timeStamp = System.nanoTime();
		Workspace workspace = new Workspace();
		File jobfolder = new File(workspacePath.concat(File.separator).concat(hpcAddress).concat("_").concat(""+timeStamp));
		
		assertEquals(jobfolder.getAbsolutePath(), workspace.createJobFolder(workspacePath, hpcAddress, timeStamp).getAbsolutePath());
		assertNull(workspace.createJobFolder(workspacePath, hpcAddress, timeStamp));
		
		jobfolder.delete();
	}
	
	@Test
	public void createInputFileTest() throws IOException {
		
		String inputFileDestinationPath = System.getProperty("java.io.tmpdir") + "input_dest";
		File input = new File(System.getProperty("java.io.tmpdir")+"input_src");
		Workspace workspace = new Workspace();
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(input));
		bw.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n");
		bw.close();
		
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.createInputFile(inputFileDestinationPath, input));
		assertTrue(FileUtils.contentEquals(input, new File(inputFileDestinationPath)));
		input.delete();
		new File(inputFileDestinationPath).delete();
	}
	
	@Test
	public void createStatusFileTest() throws IOException {
		
		File workspaceFolder = new File(System.getProperty("java.io.tmpdir"));
		String statusFilePath = workspaceFolder.toString()+File.separator+Status.STATUS_FILE.getName();
		String hpcAddress = "login-skylake.hpc.cam.ac.uk";
		File tempFile = new File(workspaceFolder+File.separator+"temp.txt");
		Workspace workspace = new Workspace();
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(tempFile));
		bw.write(Status.ATTRIBUTE_JOB_STATUS.getName().concat(" "));
		bw.write(Status.STATUS_JOB_NOT_STARTED.getName().concat("\n"));
		bw.write(Status.ATTRIBUTE_JOB_ID.getName().concat("\n"));
		bw.write(Status.ATTRIBUTE_AGENT_ID.getName().concat(" "));
		bw.write(workspaceFolder.getName().concat("\n"));
		bw.write(Status.ATTRIBUTE_HPC_ADDRESS.getName().concat(" "));
		bw.write(hpcAddress.concat("\n"));
		bw.write(Status.ATTRIBUTE_JOB_OUTPUT.getName().concat("\n"));
		bw.close();
		
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.createStatusFile(workspaceFolder, statusFilePath, hpcAddress));
		assertTrue(FileUtils.contentEquals(new File(statusFilePath), tempFile));
		tempFile.delete();
		new File(statusFilePath).delete();
		
	}
	
	@Test
	public void copyFileTestOne() throws IOException {
		
		String destination = System.getProperty("java.io.tmpdir") + "dest";
		String source = System.getProperty("java.io.tmpdir")+"src";
		File temp = new File(System.getProperty("java.io.tmpdir")+"temp");
		Workspace workspace = new Workspace();
		
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(source));
		bw1.write("Lorem ipsum dolor sit amet\rconsectetur adipisci elit,\rsed eiusmod tempor incidunt\r minim veniam, quis nostrum exercitationem ullam corporis suscipit\r");
		bw1.close();
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.copyFile(source, destination));
		assertTrue(FileUtils.contentEquals(new File(source), new File(destination)));
		
		new File(destination).delete();
		
		String destination_sh = System.getProperty("java.io.tmpdir")+"dest.sh";
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(temp));
		bw2.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n");
		bw2.close();
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.copyFile(source, destination_sh));
		assertTrue(FileUtils.contentEquals(temp, new File(destination_sh)));
		
		temp.delete();
		new File(destination_sh).delete();
		new File(source).delete();
		
	}
	
	@Test
	public void getTimeStampPartTest() {
		
		String folder = "login-skylake.hpc.cam.ac.uk_428109593378500";
		String folder_null = "";
		Workspace workspace = new Workspace();
		String[] tokens = folder.split("_");
		assertEquals(tokens[1], workspace.getTimeStampPart(folder));
		assertNull(workspace.getTimeStampPart(folder_null));
		
	}
	
	@Test
	public void getStatusFilePathTest() {
		
		File jobFolder = new File(System.getProperty("java.io.tmpdir"));
		Workspace workspace = new Workspace();
		String expected = jobFolder.getAbsolutePath().concat(File.separator).concat(Property.STATUS_FILE_NAME.getPropertyName());
		assertEquals(expected, workspace.getStatusFilePath(jobFolder));
	}
	
	@Test
	public void getJSONInputFilePathTest() {
		
		File jobFolder = new File(System.getProperty("java.io.tmpdir"));
		Workspace workspace = new Workspace();
		String expected = jobFolder.getAbsolutePath().concat(File.separator).concat(Property.JSON_INPUT_FILE_NAME.getPropertyName());
		assertEquals(expected, workspace.getJSONInputFilePath(jobFolder));
		
	}
	
	@Test
	public void copyScriptFileTest() throws IOException {
		
		String source = System.getProperty("java.io.tmpdir")+"source";
		String dest = System.getProperty("java.io.tmpdir") + "dest\\";
		new File(dest).mkdir();
		
		String slurmScriptFileName = "slurmscript.sh";
		File temp = new File(System.getProperty("java.io.tmpdir")+"temp");
		Workspace workspace = new Workspace();
		
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(source));
		bw1.write("Lorem ipsum dolor sit amet\rconsectetur adipisci elit,\rsed eiusmod tempor incidunt\r minim veniam, quis nostrum exercitationem ullam corporis suscipit\r");
		bw1.close();
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(temp));
		bw2.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\n minim veniam, quis nostrum exercitationem ullam corporis suscipit\n");
		bw2.close();
		
		assertEquals(Status.JOB_SETUP_SUCCESS_MSG.getName(), workspace.copyScriptFile(source, dest, slurmScriptFileName));
		assertTrue(FileUtils.contentEquals(temp, new File(dest.concat(File.separator).concat(slurmScriptFileName))));
		
		new File(dest.concat(File.separator).concat(slurmScriptFileName)).delete();
		new File(dest).delete();
		new File(source).delete();
		temp.delete();
	}
	
	@Test
	public void copyFileTestTwo() throws IOException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
		
		File source = new File(System.getProperty("java.io.tmpdir")+"src");
		File destination = new File(System.getProperty("java.io.tmpdir") + "dest");
		Workspace workspace = new Workspace();
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(source));
		bw.write("Lorem ipsum dolor sit amet\nconsectetur adipisci elit,\nsed eiusmod tempor incidunt\nminim veniam, quis nostrum exercitationem ullam corporis suscipit\n");
		bw.close();
		Method copyFile = workspace.getClass().getDeclaredMethod("copyFile", File.class, File.class);
		copyFile.setAccessible(true);
		copyFile.invoke(workspace, source, destination);
		assertTrue(FileUtils.contentEquals(source, destination));
		source.delete();
		destination.delete();
	}
}
