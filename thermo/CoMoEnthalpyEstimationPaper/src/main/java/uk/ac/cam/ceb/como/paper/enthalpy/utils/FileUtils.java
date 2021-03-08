package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * 
 * @author NK510
 * 
 *
 */

public class FileUtils {
	
	
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param referenceFolderPath The folder path where approved files are stored.
	 * @param targetFolderPath    The folder path where generated files are stored
	 *                            by using initial analysis (data pre-processing) in
	 *                            Philipp's code.
	 *                            
	 */
	
	public void compareFiles(String referenceFolderPath, String targetFolderPath) {

		File referenceFolder = new File(referenceFolderPath);
		
		File targetFolder = new File(targetFolderPath);

		if (referenceFolder.isDirectory() && referenceFolder.exists() && targetFolder.isDirectory() && targetFolder.exists()) {

			File[] referenceFilesList = referenceFolder.listFiles();
			
			File[] targetFileList = targetFolder.listFiles();

		}
	}

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param referenceFiles      List of files that are stored in reference folder
	 *                            or its subfolders.
	 * @param targetFiles         List of files that are stored in target folder or
	 *                            its subfolders.
	 *                            	
	 * @param folderLevel         the level of folder in folder tree.
	 * @param index               the index.
	 * 
	 */
   
   public void searchFiles(File[] referenceFiles, File[] targetFiles, int folderLevel, int index) {

		if((index==referenceFiles.length) && (index==targetFiles.length)) {
			
		return;
			
		}
		
		if(referenceFiles[index].isFile()) {
			
			System.out.println("referenceFiles["+index+"].getName()" + referenceFiles[index].getName() + "  " + "targetFiles["+index+"].getName()" + targetFiles[index].getName() );
			
		} else if(referenceFiles[index].isDirectory()) {
			
			
		}
	}
   /**
    * 
    * @author NK510 (caresssd@hermes.cam.ac.uk)
    * @param zipFilePath the file path of zip file
    * 
    */
   public static void getUnzipFolder(String zipFilePath) {
	   
	   try {
		   
		   ZipFile zipFile = new ZipFile(zipFilePath);
		   
		   Enumeration<?> enumeration = zipFile.entries();
		   
		   while(enumeration.hasMoreElements()) {
			   
			   ZipEntry zipEntry = (ZipEntry)enumeration.nextElement();
			   
			   
			   String zipEntryName = zipEntry.getName();
			   
			//   System.out.println("zipEntry.getName(): " + zipEntry.getName());
			   
			   File fileName = new File(zipEntryName);
			   
			   System.out.println(fileName);
			   
			   if(zipEntryName.endsWith("/")) {
				   
				   fileName.mkdirs();
				   continue;
			   }
			   
			   File parentFile = fileName.getParentFile();
			   if(parentFile!=null) {
				   parentFile.mkdirs();
			   }
			   
			   InputStream inputStream = zipFile.getInputStream(zipEntry);
			   FileOutputStream fileOutputStream = new FileOutputStream(fileName);
			   
			   
			   byte[] bytes = new byte[2048];
               int length;
               
               while ((length = inputStream.read(bytes)) >= 0) {
                   fileOutputStream.write(bytes, 0, length);
               }
               inputStream.close();
               fileOutputStream.close();
		   }
		   
           zipFile.close();
           
       } catch (IOException e) {
           e.printStackTrace();
       }
   
   }
   
}