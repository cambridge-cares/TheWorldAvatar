package uk.ac.cam.cares.jps.agent.mechanism.coordination;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.slurm.job.Property;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Workspace;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

public class Utils {
	private static Logger logger = LoggerFactory.getLogger(Utils.class);
	public static long previousTimeStamp;
	
	/**
	 * Generate unique time stamp to be used in naming mechanism calibration jobs and</br>
	 * the instance of MoDS Agent.
	 * 
	 * @return
	 */
	public static long getTimeStamp() {
		long currentTimeStamp = System.nanoTime();
		while(!(currentTimeStamp > previousTimeStamp)) {
			currentTimeStamp = System.nanoTime();
		}
		previousTimeStamp = currentTimeStamp;
		return currentTimeStamp;
	}
	
	/**
	 * Creates an instance of the BUfferedWriter class.
	 * 
	 * @param filePathPlusName the path plus name of the file being written
	 * @return
	 * @throws IOException
	 */
	public static BufferedWriter openBufferedWriter(String filePathPlusName) throws IOException {
		return new BufferedWriter(new FileWriter(filePathPlusName));
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openSourceFile(String filePathPlusName) throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(filePathPlusName), "UTF-8"));
	}
	
	public static boolean isJobCompleted(File jobFolder) throws IOException {
		return isJobFinished(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	public static boolean isJobOutputProcessed(File jobFolder) throws IOException {
		return isJobOutputProcessed(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Check the status if a job finished.
	 * 
	 * @param statusFilePath
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobFinished(String statusFilePath) throws IOException {
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())) {
				if (line.contains(Status.STATUS_JOB_COMPLETED.getName())) {
					statusFile.close();
					return true;
				}
				if (line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())) {
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check the status if a job is currently running. 
	 * 
	 * @param statusFilePath the absolute path to the status file
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobOutputProcessed(String statusFilePath) throws IOException {
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())) {
				if (line.contains(Status.OUTPUT_PROCESSED.getName())) {
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Modifies the status of job in the status file.
	 * 
	 * @param filePath the path to the status file
	 * @param status can be "running" or "completed"
	 * @throws IOException
	 */
	public static void modifyStatus(String filePath, String status) throws IOException {
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())) {
				line = Status.ATTRIBUTE_JOB_STATUS.getName().concat(" ").concat(status);
			}
			fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for (String lineContent:fileContent) {
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}
	
	/**
	 * Modifies the output to processed in the status file. 
	 * 
	 * @param filePath the path to the status file
	 * @param status can be empty ("") or "processed".
	 * @throws IOException
	 */
	public static void modifyOutputStatus(String filePath, String status) throws IOException {
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())) {
				line = Status.ATTRIBUTE_JOB_OUTPUT.getName().concat(" ").concat(status);
			}
			fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for (String lineContent:fileContent) {
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}
	
	/**
	 * Finds the status file from a list of files related to a job. 
	 * 
	 * @param filePaths paths to all files in a job folder
	 * @return
	 */
	public static File getStatusFile(List<String> filePaths) {
		for (String filePath:filePaths) {
			if (filePath.toLowerCase().endsWith(Status.STATUS_FILE.getName().toLowerCase())) {
				return new File(filePath);
			}
		}
		return null;
	}
	
	/**
	 * Returns the status file if the job folder is provided. 
	 * 
	 * @param jobFolder path to the job folder
	 * @return
	 */
	public static File getStatusFile(File jobFolder) {
		if (jobFolder.isDirectory()) {
			if ((new File(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName())).isFile())) {
				return new File(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
			}
		}
		return null;
	}
	
	/**
	 * Returns the zip of a given folder and content inside that folder. 
	 * 
	 * @param folderName the folder name. 
	 * @return zip folder and content inside that folder. 
	 * @throws IOException
	 */
	public static File getZipFile(String folderName) throws IOException {
		Path sourceDirectory = Paths.get(folderName);
		String zipFileName = folderName.concat(".zip");
		
		try {
			ZipOutputStream zipOutputStream = new ZipOutputStream(new FileOutputStream(zipFileName));
			
			Files.walkFileTree(sourceDirectory, new SimpleFileVisitor<Path>() {
				
				@Override
				public FileVisitResult visitFile(Path arg0, BasicFileAttributes arg1) throws IOException {
					
					try {
						Path destinationFile = sourceDirectory.relativize(arg0);
						zipOutputStream.putNextEntry(new ZipEntry(destinationFile.toString()));
						byte[] bytes = Files.readAllBytes(arg0);
						zipOutputStream.write(bytes, 0, bytes.length);
						zipOutputStream.closeEntry();
					} catch (IOException e) {
						e.printStackTrace();
					}
					return FileVisitResult.CONTINUE;
				}
				
			});
			
			zipOutputStream.flush();
			zipOutputStream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		File zipFile = new File(zipFileName);
		
		return zipFile;
	}
	
	/**
	 * Unzip a given file to the specified destination. 
	 * 
	 * @param zipFilePath path of zip file
	 * @param destDir destination of unzip file
	 */
	public static void unzipFile(String zipFilePath, String destDir) {
		File dir = new File(destDir);
		// create output directory if it doesn't exist
		if (!dir.exists()) {
			dir.mkdirs();
		}
		FileInputStream fis;
		// buffer for read and write data to file
		byte[] buffer = new byte[1024];
		try {
			fis = new FileInputStream(zipFilePath);
			ZipInputStream zis = new ZipInputStream(fis);
			ZipEntry ze = zis.getNextEntry();
			while (ze != null) {
				try {
					String fileName = ze.getName();
					File newFile = new File(destDir + File.separator + fileName);
//					System.out.println("Unzipping to " + newFile.getAbsolutePath());
					// create directories for sub directories in zip
					new File(newFile.getParent()).mkdirs();
					if (ze.isDirectory()) // check if this is a diectory or file
					{
						newFile.mkdirs();
					} else {
						FileOutputStream fos = new FileOutputStream(newFile);
						int len;
						while ((len = zis.read(buffer)) > 0) {
							fos.write(buffer, 0, len);
						}
						fos.close();
					}
					// close this ZipEntry
					zis.closeEntry();
				} catch (Exception e) {
					System.err.println(e.getMessage());
				}
				ze = zis.getNextEntry();
			}
			// close last ZipEntry
			zis.closeEntry();
			zis.close();
			fis.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Copy the whole folder and its subfolders from source to destination. 
	 * @param source
	 * @param destination
	 * @throws IOException
	 */
	public static void copyFolder(String source, String destination) throws IOException {
		File srcDir = new File(source);
		File destDir = new File(destination);
		FileUtils.copyDirectory(srcDir, destDir);
	}
	
	/**
	 * Copy any file from the source path to the destination path.
	 * 
	 * @param from the absolute path to the file which will be copied.
	 * @param to the absolute path to the folder where the file will be<br>
	 * copied. If user provides absolute path including the file name,<br>
	 * probably renamed or the same as the one in the from path, the method<br>
	 * will keep the same name. 
	 * 
	 * @throws IOException
	 */
	public static void copyFile(File from, File to) throws IOException {
		com.google.common.io.Files.copy(from, to);
	}
	
	/**
	 * Reads the job id from the status file.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static String getJobId(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().toLowerCase().startsWith(Status.ATTRIBUTE_JOB_ID.getName().toLowerCase())){
				String tokens[] = line.trim().split(":");
				if(tokens.length>=2 && tokens[1].trim().length()>0){
					statusFile.close();
					return tokens[1].trim();
				}
			}
		}
		statusFile.close();
		return null;
	}
	/**
	 * Adds the job id to the status file. 
	 * 
	 * @param filePath the path to the status file.
	 * @param jobId the job id generated following the sbatch submission. 
	 * @throws IOException
	 */
	public static void addJobId(String filePath, String jobId) throws IOException{
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null){
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())) {
		        line = Status.ATTRIBUTE_JOB_STATUS.getName().concat(" ").concat(Status.STATUS_JOB_RUNNING.getName());
		    }
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName())) {
		        line = Status.ATTRIBUTE_JOB_ID.getName().concat(" ").concat(jobId);
		    }
		    fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for(String lineContent:fileContent){
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}
	
	/**
	 * Reads the agent id from the status file.
	 * 
	 * @param statusFilePath
	 * @return
	 * @throws IOException
	 */
	public static String getAgentId(String statusFilePath) throws IOException {
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().toLowerCase().startsWith(Status.ATTRIBUTE_AGENT_ID.getName().toLowerCase())){
				String tokens[] = line.trim().split(":");
				if(tokens.length>=2 && tokens[1].trim().length()>0){
					statusFile.close();
					return tokens[1].trim();
				}
			}
		}
		statusFile.close();
		return null;
	}
	
	public static void modifyAgentId(String filePath, String agentId) throws IOException {
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null) {
			if (line.trim().startsWith(Status.ATTRIBUTE_AGENT_ID.getName())) {
				line = Status.ATTRIBUTE_AGENT_ID.getName().concat(" ").concat(agentId);
			}
			fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for (String lineContent:fileContent) {
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}
	
	/**
	 * Checks if a job is still running.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobRunning(File jobFolder) throws IOException{
		return isJobRunning(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Check the status if a job is currently running.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobRunning(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_RUNNING.getName()) 
						|| line.contains(Status.STATUS_JOB_COMPLETING.getName()) 
						|| line.contains(Status.STATUS_JOB_PENDING.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check if a job is completed.
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobCompleted(File jobFolder, SlurmJobProperty slurmJobProperty) throws IOException{
		return isJobFinished(jobFolder, jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()), slurmJobProperty);
	}
	
	/**
	 * Check the status if a job finished.
	 * 
	 * @param jobFolder the folder that contains the job.
	 * @param statusFilePath the file that contains the status of the job.
	 * @param slurmJobProperty the variable that allows to read different<br>
	 * properties including agentClass, completed jobs folder prefix, etc.
	 * 
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobFinished(File jobFolder, String statusFilePath, SlurmJobProperty slurmJobProperty) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_COMPLETED.getName())){
					statusFile.close();
					if(isJobPostProcessed(jobFolder, statusFilePath)){
						moveToComplete(jobFolder, slurmJobProperty);
					}
					return true;
				}
				if(line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())){
					statusFile.close();
					if(isJobPostProcessed(jobFolder, statusFilePath)){
						moveToComplete(jobFolder, slurmJobProperty);
					}
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check the status if a job post-processed.
	 * 
	 * @param jobFolder the folder that contains the job.
	 * @param statusFilePath the file that contains the status of the job.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobPostProcessed(File jobFolder, String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())){
				if(line.contains(Status.OUTPUT_PROCESSED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Moves the provided job folder to the folder that contains completed<br>
	 * and post-processed jobs of the agent. 
	 * 
	 * @param jobFolder the folder that contains a job
	 * @param slurmJobProperty
	 */
	public static void moveToComplete(File jobFolder, SlurmJobProperty slurmJobProperty) {
		try {
			File destDir = getCompletedJobsDirectory(jobFolder, slurmJobProperty);
			if(destDir!=null){
				FileUtils.copyDirectory(jobFolder, destDir);
				FileUtils.deleteDirectory(jobFolder);
			}
		} catch (IOException e) {
		    e.printStackTrace();
		}
	}
	
	/**
	 * Returns the folder where completed jobs are saved.
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static File getCompletedJobsDirectory(File jobFolder, SlurmJobProperty slurmJobProperty) throws IOException{
		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), slurmJobProperty.getAgentClass());
		String completedJobsDirectory = Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator).concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName()).concat(File.separator).concat(jobFolder.getName());
		workspace = new File(completedJobsDirectory);
		if(workspace.mkdirs()){
			return workspace;
		}
		return null;
	}
}