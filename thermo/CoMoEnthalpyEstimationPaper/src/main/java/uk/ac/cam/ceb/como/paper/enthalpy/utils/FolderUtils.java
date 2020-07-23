package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import static org.junit.Assert.assertEquals;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

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
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param folderPath Folder path that contains folders and files that will be zipped.
	 * @throws IOException
	 */
	public static void getZipFile(String folderPath) throws IOException {
		
		
		Path sourceDirectory = Paths.get(folderPath);		
		
		String zipFileName = folderPath.concat(".zip");

		try {
			
		ZipOutputStream zipOutputStream = new ZipOutputStream (new FileOutputStream(zipFileName));
		
		Files.walkFileTree(sourceDirectory, new SimpleFileVisitor<Path>() {

			@Override
			public FileVisitResult visitFile(Path arg0, BasicFileAttributes arg1) throws IOException {
				
				try {
					
				Path destinationFile = sourceDirectory.relativize(arg0);
				
				zipOutputStream.putNextEntry(new ZipEntry(destinationFile.toString()));
				
				byte[] bytes = Files.readAllBytes(arg0);
				
				zipOutputStream.write(bytes, 0, bytes.length);
				
				zipOutputStream.closeEntry();
				
				}catch(IOException e) {
					
					e.printStackTrace();
				}
				
				// TODO Auto-generated method stub
				//return super.visitFile(arg0, arg1);
				
				return FileVisitResult.CONTINUE;
				
			}
		});
		
		zipOutputStream.flush();
		
		zipOutputStream.close();
		
		}catch(IOException e) {
			
			e.printStackTrace();
		}
		
	}
	
}