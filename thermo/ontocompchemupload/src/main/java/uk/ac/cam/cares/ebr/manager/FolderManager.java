package uk.ac.cam.cares.ebr.manager;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;

import java.util.Map.Entry;
import java.util.UUID;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author nk510 The Class FolderManager.
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
	public static String generateUniqueFolderName(String fileName) throws UnsupportedEncodingException {

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
	 * @param folderName
	 *                   <p>
	 *                   A folder's name to be created based on using uuid.
	 *                   </p>
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
	 * @param inputFile        Input file to be saved in created folder.
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
	 * @param uuid           the unique identifier
	 * @param kbFolderPath   folder where owl files are stored.
	 * @param dataFolderPath folder where Gaussian, xml, and png files are stored.
	 * @param format         the format of file.
	 * @return the file name
	 */

	public String getFileName(String uuid, String kbFolderPath, String dataFolderPath, String format) {

		String fileName = null;

		String folderName = null;

		if (format.endsWith(".owl")) {

			folderName = kbFolderPath + uuid.toString();

		} else {

			folderName = dataFolderPath + uuid.toString();

		}

		File file = new File(folderName);

		for (File f : file.listFiles()) {

			if (f.getName().endsWith(format)) {

				fileName = f.getName();

				break;
			}
		}

		return fileName;

	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param folderPath the folder path for 923 hco species
	 * @return the linkedlist of 923 cas registry ids of hco species. Gaussian file
	 *         names should be given by using their cas registry id. 
	 * 
	 */
	public static LinkedList<String> getHCOSpeciesCasRegID(String folderPath) {

		LinkedList<String> hcoSpeciesList = new LinkedList<String>();

		File speciesFile = new File(folderPath);

		File[] hcoSpeciesFolder = speciesFile.listFiles();

		for (int i = 0; i < hcoSpeciesFolder.length; i++) {

			if (hcoSpeciesFolder[i].isFile()) {
	
			/**
			 * Adds in LinkedList species names that are given as CAS registry IDs.
			 */
			hcoSpeciesList.add(hcoSpeciesFolder[i].getName().split("\\.")[0]);

			}
			
		}

	return hcoSpeciesList;

	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param csvFilePath the csv file path. The csv file contains 923 cas registry IDs and related ontospecies IRIs.
	 * @return the linked hash map. Key in this map is cas regisry id.
	 * @throws IOException the IO exception
	 * 
	 */
	public static LinkedHashMap<String, String> getOntoSpeciesCasRegIDAndIRI(String csvFilePath) throws IOException{
		
		LinkedHashMap<String, String> ontoCompChemSpecieMap = new LinkedHashMap<String, String>();
		
		CSVReader reader = new CSVReader(new FileReader(csvFilePath));
		
		String[] line;
		
		while((line=reader.readNext())!=null) {
		
			ontoCompChemSpecieMap.put(line[0], line[1]);
		}
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * 
		 * Prints the LinkedHasMap content.
		 */
		for(Entry<String, String> map: ontoCompChemSpecieMap.entrySet()) {
			
//			System.out.println(map.getKey() + " " + map.getValue());
		}
		
		System.out.println("ontoCompChemSpecieMap.size(): " +ontoCompChemSpecieMap.size());
		
		reader.close();
		
		return ontoCompChemSpecieMap;
	}
	

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param sourceFile the source Gaussian file.
	 * @param destinationFile destination Gaussian file.
	 * @throws IOException the IO exception.
	 */
	public static void copyFileToDestinationFolder(File sourceFile, File destinationFile) throws IOException {
		
        InputStream is = null;
        OutputStream os = null;
        
        try {
            is = new FileInputStream(sourceFile);
            os = new FileOutputStream(destinationFile);

            // buffer size 2K
            byte[] buf = new byte[2048];

            int bytesRead;
            while ((bytesRead = is.read(buf)) > 0) {
                os.write(buf, 0, bytesRead);
            }
        } finally {
            is.close();
            os.close();
        }
    }
	
}