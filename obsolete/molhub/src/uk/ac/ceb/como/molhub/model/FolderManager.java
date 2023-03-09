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

import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.ceb.como.molhub.bean.GaussianUploadReport;

/**
 * 
 * The Class FolderManager.
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *  
 */
public class FolderManager {

	/** The upload report list. */
	private List<GaussianUploadReport> uploadReportList = new ArrayList<GaussianUploadReport>();
	
	/** The gaussian upload report. */
	GaussianUploadReport gaussianUploadReport = new GaussianUploadReport();
	
	/**
	 * Generate unique folder name.
	 *
	 * @author nk510
	 * @param fileName Name of Gaussian file
	 * @throws UnsupportedEncodingException the unsupported encoding exception
	 * @return uuid the unique identifier (UUID) we use to name folder and files.
	 * 
	 */
	public static String generateUniqueFolderName(String fileName)
			throws UnsupportedEncodingException {
		
		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 Generates source for universally unique identifier (UUID) based
		 *         on file name, date, time, and  CPU milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);
		
		return uuid.toString();

	}

	/**
	 * Creates the folder.
	 *
	 * @author nk510
	 * @param folderName <p>A folder's name to be created based on using UUID.</p>
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
	 * Save file in folder.
	 *
	 * @author nk510
	 * @param inputFile Input file to be saved in created folder.
	 * @param absoluteFilePath a path to a folder where input file will be saved.
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static void saveFileInFolder(File inputFile, String absoluteFilePath) throws IOException {

		Path path = Paths.get(absoluteFilePath);

		byte[] data = Files.readAllBytes(path);

		BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(inputFile));

		stream.write(data);

		stream.close();

	}
	
	/**
	 * Gets the upload report list.
	 *
	 * @return the upload report list that includes UUID, Gaussian file name, OWL file name, OWL consistency (true/false), Comment 
	 */
	public List<GaussianUploadReport> getUploadReportList() {
		return uploadReportList;
	}

	/**
	 * Sets the upload report list.
	 *
	 * @param uploadReportList the new upload report list
	 */
	public void setUploadReportList(List<GaussianUploadReport> uploadReportList) {
		this.uploadReportList = uploadReportList;
	}
	
	/**
	 * Gets the file name.
	 * 
	 * @author nk510
	 * @param uuid the unique identifier (UUID)
	 * @param kbFolderPath folder where owl files are stored.
	 * @param dataFolderPath folder where Gaussian, JSON, OWL, and PNG files are stored.
	 * @param format the format of file.
	 * @return the file name
	 *  
	 */

	public String getFileName(String uuid,  String kbFolderPath, String dataFolderPath, String format) {
		
		String fileName=null;
		
		String folderName = null;
		
		if(format.endsWith(".owl")) {
			
		folderName =kbFolderPath + uuid.toString();
		
		}
		else {
			
		folderName= dataFolderPath + uuid.toString();
			
		}
		
		File file = new File(folderName);

		for(File f : file.listFiles()) {
			
			if(f.getName().endsWith(format)){
				
				fileName = f.getName();
				
				break;
			}
		}
		
        return fileName;
	
	}

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * @param uuidFile the unique identifies (parameter) for OWL file name.
 * @return JSON file name that contains data extracted from Log file.
 */
public String getGaussianJsonFileName(String uuidFile) {
	
	return uuidFile.replaceAll(".owl", ".json");
}

	public String getOwlFileName(String uuid, String uuidFile, String kbFolderPath, String format) {
		
		String folderName = null;
		
		if(format.endsWith(".owl")) {
		folderName =kbFolderPath + uuid.toString();
		}
		
		File file = new File(folderName + "/" + uuidFile);
		
        return file.getName()  ;
}

	
	/**
	 * 
	 * @param uuid the unique identifier that is used to create unique folder inside Tomcat server 
	 * @param uuidFile the uuid file 
	 * @param dataFolderPath data folder path
	 * @return file Gaussian file name that is uploaded 
	 */
	public String getGaussianFileName(String uuid, String uuidFile, String dataFolderPath) {
		
		String fileName = "";
		
		
		File[] fileList = new FileUtility().getFileList(dataFolderPath + "/" + uuid +"/",".log", ".g09", ".g16");
		

		for(File f: fileList) {
			
			fileName = f.getName();
		}
		
		return fileName;

}
	



	/**
	 * @param uuid unique identifier (UUID) that we use to create folder, name uploaded Log file.
	 * @param uuidFile unique identifier for generated OWL file.
	 * @param dataFolderPath data folder path
	 * @param format format of a file.
	 * @return the JSON file name that contains information about results of NASA polynomial calculations.
	 */
	public String getJsonNasaFileName(String uuid, String uuidFile, String dataFolderPath, String format) {
		
		String folderName = null;
		
		if(format.endsWith("q_nasa.json")) {

		folderName =dataFolderPath + uuid.toString() + "/" + uuidFile.replaceAll(".owl", "") + format;
		
		} 
		
		File file = new File(folderName);
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Checks whether NASA polynomial JSON file is generated. 
		 */
		if(file.exists()) {
			
        return file.getName()  ;
        
		}else {
			
			return null;
		}
	
	}
	
}