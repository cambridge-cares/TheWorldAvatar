package uk.ac.cam.cares.ebr.manager;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;

import java.util.UUID;


/**
 * @author nk510
 * The Class FolderManager.
 */
public class FolderManager {

	
	/**
	 * Generate unique folder name.
	 *
	 * @author nk510
	 * @param fileName Name of Gaussian file
	 * @throws UnsupportedEncodingException the unsupported encoding exception
	 * @return uuid the unique identifier used to name folder and files.
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
		 * @author nk510 Generates source for universally unique identifier (uuid) based
		 *         on file name, date, time, and cpu milliseconds.
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
	 * @param folderName <p>A folder's name to be created based on using uuid.</p>
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
	 * Gets the file name.
	 * 
	 * @author nk510
	 * @param uuid the unique identifier
	 * @param kbFolderPath folder where owl files are stored.
	 * @param dataFolderPath folder where Gaussian, xml, and png files are stored.
	 * @param format the format of file.
	 * @return the file name
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
}