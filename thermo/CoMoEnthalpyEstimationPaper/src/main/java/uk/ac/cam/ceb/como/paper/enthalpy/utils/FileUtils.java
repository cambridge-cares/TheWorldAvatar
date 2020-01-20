package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.File;

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
}