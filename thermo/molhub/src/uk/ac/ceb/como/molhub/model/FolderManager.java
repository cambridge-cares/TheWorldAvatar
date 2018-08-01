package uk.ac.ceb.como.molhub.model;

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

public class FolderManager {

	public static String generateUniqueFolderName(String fileName, String catalinaFolderPath)
			throws UnsupportedEncodingException {

		// To create UUID by using encoded content of given file.
		// byte[] encoded = Files.readAllBytes(Paths.get(path));
		// String fileContent = new String(encoded, encoding);

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
		
		folderName = catalinaFolderPath + "/conf/Catalina/" + uuid.toString();

		return folderName;
		
	}

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

	public static void saveFileInFolder(File inputFile, String absoluteFilePath) throws IOException {

		Path path = Paths.get(absoluteFilePath);
		
		byte[] data = Files.readAllBytes(path);
		
		BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(inputFile));
		
		stream.write(data);
		
        stream.close();
        
	}
	
	
}