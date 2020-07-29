package uk.ac.cam.cares.jps.base.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.commons.io.IOUtils;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class FileUtil {

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
		
	    FileWriter fileWriter = null;
	    
	    try {
			fileWriter = new FileWriter(file);
			fileWriter.write(content);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
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
		
		
    	try {
    		ByteArrayOutputStream outputStream = null;
			try {
				outputStream = new ByteArrayOutputStream();
				IOUtils.copy(inputStream, outputStream);
				return new String(outputStream.toByteArray());
			} finally {
				inputStream.close();
				outputStream.close();
			}
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
	 * Creates an instance of the BufferedWriter class.
	 * 
	 * @param filePathPlusName the path plus name of the file being written
	 * @return
	 * @throws IOException
	 */
	public static BufferedWriter openBufferedWriter(String filePathPlusName) throws IOException{
		return new BufferedWriter(new FileWriter(filePathPlusName));
	}
} 
