package uk.ac.cam.cares.jps.thermo.manager;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.UUID;

public class FolderManager {

	
	 /**
		 * Generate unique folder name.
		 *
		 * @author nk510
		 * @param fileName Name of Gaussian file
		 * @param catalinaFolderPath A path where folder will be created.
		 * @return A unique folder name.
		 * @throws UnsupportedEncodingException the unsupported encoding exception
		 */
		synchronized public static String generateUniqueFolderName(String fileName, String catalinaFolderPath)
				throws UnsupportedEncodingException {
			
			String folderName = "";

			long milliseconds = System.currentTimeMillis();

			String datetime = new Date().toString();

			datetime = datetime.replace(" ", "");
			datetime = datetime.replace(":", "");

			/**
			 * 
			 * @author nk510 <p>Generates source for universally unique identifier (uuid) based
			 *         on file name, date, time, and cpu milliseconds.</p>
			 * 
			 */

			String source = fileName + datetime + milliseconds;

			byte[] bytes = source.getBytes("UTF-8");

			UUID uuid = UUID.nameUUIDFromBytes(bytes);

			folderName =  uuid.toString();

			return folderName;

		}
		
		/**
		 * Creates the folder.
		 *
		 * @author nk510
		 * @param folderName <p>A folder's name to be created based on using uuid.</p>
		 */
		synchronized public static void createFolder(String folderName) {

			Path folderPath = Paths.get(folderName);

			if (!Files.exists(folderPath)) {

				try {
					Files.createDirectories(folderPath);
				} catch (IOException e) {

					e.printStackTrace();
				}
			}

		}
		
}
