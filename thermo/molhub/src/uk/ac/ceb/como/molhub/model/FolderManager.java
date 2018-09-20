package uk.ac.ceb.como.molhub.model;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;

public class FolderManager {

	private List<GaussianUploadReport> uploadReportList = new ArrayList<GaussianUploadReport>();
	
	GaussianUploadReport gaussianUploadReport = new GaussianUploadReport();
	
	/**
	 * @author nk510
	 * @param fileName Name of Gaussian file
	 * @param catalinaFolderPath A path where folder will be created.
	 * @return A unique folder name.
	 * @throws UnsupportedEncodingException
	 */
	public static String generateUniqueFolderName(String fileName, String catalinaFolderPath)
			throws UnsupportedEncodingException {
		
		String folderName = "";

		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 Generates source for universally unique identifier (uuid) based
		 *         on file name, date, time, and milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);

		folderName = catalinaFolderPath + "/webapps/ROOT/" + uuid.toString();

		return folderName;

	}

	/**
	 * @author nk510
	 * @param folderName a folder's name to be created.
	 */
	public static void createFolder(String folderName) {

		Path folderPath = Paths.get(folderName);

		if (!Files.exists(folderPath)) {

			try {
				Files.createDirectories(folderPath);
			} catch (IOException e) {

				e.printStackTrace();
			}
		}

	}

	/**
	 * @author nk510
	 * @param inputFile Input file to be saved in created folder.
	 * @param absoluteFilePath a path to a folder where input file will be saved.
	 * @throws IOException
	 */
	public static void saveFileInFolder(File inputFile, String absoluteFilePath) throws IOException {

		Path path = Paths.get(absoluteFilePath);

		byte[] data = Files.readAllBytes(path);

		BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(inputFile));

		stream.write(data);

		stream.close();

	}
	
	public List<GaussianUploadReport> getUploadReportList() {
		return uploadReportList;
	}

	public void setUploadReportList(List<GaussianUploadReport> uploadReportList) {
		this.uploadReportList = uploadReportList;
	}
	
	/**
	 * @author nk510
	 * @param  uuid unique folder name
	 * @param  catalinaFolderPath Apache Tomcat server's Catalina folder path.
	 * @param  format format of a file stored in given folder name.
	 * @return file name saved in given folder name (uuid).
	 */
	public String getFileName(String uuid,  String catalinaFolderPath, String format) {
		
		String fileName=null;
		
		String folderName = catalinaFolderPath + "/webapps/ROOT/" + uuid.toString();
		
		File file = new File(folderName);

		for(File f : file.listFiles()) {
			
			if(f.getName().endsWith(format)){
				
				fileName = f.getName();
				
				break;
			}
		}
		
        return fileName;
	
	}	
}