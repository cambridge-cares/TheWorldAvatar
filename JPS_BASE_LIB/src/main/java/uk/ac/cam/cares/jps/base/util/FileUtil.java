package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.commons.io.IOUtils;

import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvValidationException;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class FileUtil {

	/**
	 * Declared to return the list of files produced by the getDirectoryFiles()<p>
	 * method.
	 */
	private List<File> files = new ArrayList<>();
	
	public static String readFileLocally(String path) {
		
		try {
			return new String(Files.readAllBytes(Paths.get(path)));
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public static void writeFileLocally(String path, String content) {
		
		File file = new File(path);
		file.getParentFile().mkdirs();
		
	    try(FileWriter fileWriter= new FileWriter(file)) {
			fileWriter.write(content);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public static String inputStreamToString(InputStream inputStream) {
		
	   	// the following code will not preserve line endings
//		InputStreamReader isReader = new InputStreamReader(inputStream);
//		BufferedReader reader = new BufferedReader(isReader);
//		StringBuffer sb = new StringBuffer();
//		String line;
//		try {
//			try {
//				while((line = reader.readLine())!= null){
//					sb.append(line);
//				}
//			} finally {
//				inputStream.close();
//			}
//		} catch (IOException e) {
//			throw new JPSRuntimeException(e.getMessage(), e);
//		}
//		return sb.toString();

		try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()){
			IOUtils.copy(inputStream, outputStream);
			return new String(outputStream.toByteArray());
    	} catch (IOException e) {
    		throw new JPSRuntimeException(e.getMessage(), e);
    	}
	}
	
	public static InputStream stringToInputStream(String s) {
		return new ByteArrayInputStream(s.getBytes(Charset.forName("UTF-8")));
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF-8"));
	}
	
	/**
	 * Reads a CSV file and returns the content as a list of lines. Each line</br>
	 * is also codified as a list of elements.
	 * 
	 * @param fileName
	 * @return
	 * @throws IOException
	 * @throws CsvValidationException
	 */
	public static List<List<String>> openCSVSourceFile(String fileName) throws IOException, CsvValidationException {
		List<List<String>> records = new ArrayList<List<String>>();
		try (CSVReader csvReader = new CSVReader(new FileReader(fileName));) {
			String[] values = null;
			while ((values = csvReader.readNext()) != null) {
				records.add(Arrays.asList(values));
			}
		}
		return records;
	}
	
	/**
	 * Creates an instance of the BufferedWriter class.
	 * 
	 * @param filePathPlusName the path plus name of the file being written
	 * @return
	 * @throws IOException
	 */
	public static BufferedWriter openBufferedWriter(String filePathPlusName) throws IOException{
		return new BufferedWriter(new FileWriter(filePathPlusName));
	}
	
	/**
	 * Decompresses a zip file.
	 * 
	 * @param zipFilePath
	 * @param destDir
	 */
    public static void unzip(String zipFilePath, String destDir) {
    	 ZipFile zipFile = new ZipFile(zipFilePath);
 	    try {
 			zipFile.extractAll(destDir);
 		} catch (ZipException e) {
 			// TODO Auto-generated catch block
 			e.printStackTrace();
 		}
    }
	
	/**
	 * Traverses the hierarchy rooted at the folder set by a calling method<p>
	 * and looks for the list of files of the given types.<p>
	 * <p>
	 * To get all types of files, supply an empty string in the list of file<p>
	 * extensions. Read more details in the fileExtensions parameter below.
	 * 
	 * @param folder folder or directory provided by the calling method
	 * @param fileExtensions one or more file extensions provided as a list<p>
	 * by the calling method, e.g. ("owl", "rdf"). If the calling method supplies<p>
	 * empty (""), the method will return all types of files.
	 * @return
	 */
	public List<File> getDirectoryFiles(File folder, List<String> fileExtensions){
			if(folder!=null && folder.isFile()){
				for(String fileExtension:fileExtensions){
					if(folder.getName().endsWith(fileExtension)){
						files.add(new File(folder.getAbsolutePath()));						
					}
				}
			}
			if (folder.isDirectory()) {
				String[] subFolders = folder.list();
				for (String subFolder : subFolders) {
					getDirectoryFiles(new File(folder.getAbsolutePath().concat(File.separator).concat(subFolder)), fileExtensions);
				}
			}
			return files;
	}
} 
