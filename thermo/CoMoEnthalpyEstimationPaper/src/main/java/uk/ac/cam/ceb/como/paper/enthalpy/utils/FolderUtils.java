package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.UUID;

import org.apache.commons.io.FileUtils;

public class FolderUtils {

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param fileName a String that represents file name.
	 * @return String as a unique folder name.
	 * @throws UnsupportedEncodingException the exception.
	 * 
	 */
	public String generateUniqueFolderName(String fileName) throws UnsupportedEncodingException {

		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * 
		 *         Generates source for universally unique identifier (uuid) based on
		 *         file name, date, time, and cpu milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);

		return uuid.toString();

	}
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param sourceFiles Files that are stored in "valid-test-restults" folder
	 * @param targetFolder Files that are stored in folder automatically generated on each run of pre-processing step.
	 * @param depth the level (depth) of folder structure (hierarchy).
	 * 
	 */
	public static void compareFiles(File[] sourceFiles, String targetFolder, String validTestResults, int depth){
       
        for (File sourceFile : sourceFiles){
        	
            if(sourceFile.isFile()){
            	
            File targetFile = new File(sourceFile.getAbsolutePath().replaceFirst(validTestResults, targetFolder));
                
            try {
                	
        	boolean areTwoFilesEqual = FileUtils.contentEqualsIgnoreEOL(sourceFile, targetFile,"utf-8");
        	
            System.out.println("Files: " + sourceFile.getCanonicalPath() + " and "  + targetFile.getCanonicalPath() + " are equal: " + areTwoFilesEqual);
            
            /**
             * 
             * @author NK510 (caresssd@hermes.cam.ac.uk)
             * Junit test of equality the content of two files.
             * 
             */
            assertEquals("Difference between files: ", FileUtils.readFileToString(sourceFile, "utf-8"), FileUtils.readFileToString(targetFile, "utf-8"));
            
			}catch (IOException e) {
					
			e.printStackTrace();
			
			}                
            
            }else { 
            	
            if(sourceFile.isDirectory()){
            		
            compareFiles(sourceFile.listFiles(), targetFolder,validTestResults,depth + 1);
            
           } 
        } 
      }
   } 
}