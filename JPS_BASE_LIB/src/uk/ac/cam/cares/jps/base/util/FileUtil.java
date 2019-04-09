package uk.ac.cam.cares.jps.base.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

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
			fileWriter.close();
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}   
	}
	
	public static void writeFileLocally2(String path, String content) {
		FileOutputStream outputStream = null;
	    try {
		    outputStream = new FileOutputStream(path);
		    byte[] strToBytes = content.getBytes();
			outputStream.write(strToBytes);
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} finally {
			try {
				outputStream.close();
			} catch (IOException e) {
				throw new JPSRuntimeException(e.getMessage(), e);
			}
		}
	}
}
