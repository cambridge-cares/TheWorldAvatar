package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;


import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.io.FileUtils;

import com.opencsv.CSVReader;
import com.sun.istack.logging.Logger;

import junit.framework.Assert;

public class Utils {
	
	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk) Number of runs.
	 * 
	 */

	static int[] ctrRuns = new int[] { 1 };
	static int[] ctrRuns_1 = new int[] { 1 };

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk) Number of reactions that will be
	 *         generated for each species from target list.
	 * 
	 */

	static int[] ctrRes = new int[] { 1 }; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10 //5
	static int[] ctrRes_1 = new int[] { 1 };

	/**
	 * 
	 * @author nk510 (caresssd@hermes.cam.ac.uk) Number of radicals.
	 * 
	 */

	static int[] ctrRadicals_0 = new int[] { 0 }; // 0, 1, 2, 3, 4, 5 //100
	static int[] ctrRadicals_1 = new int[] { 1 }; // 0, 1, 2, 3, 4, 5 //100
	static int[] ctrRadicals_5 = new int[] { 5 }; // 0, 1, 2, 3, 4, 5 //100
	
	HashMap<String, String> pairedFileList = new HashMap<>();
	/**
	 * Compares validated test results with the thermo-code generated results. 
	 * 
	 * @param folderGenTestResults folder containing generated test results
	 * @param destRList list of reactions
	 * @param folderValidTestResults folder containing validated test results
	 */
	public void compareFiles(String folderGenTestResults, String destRList, String folderValidTestResults){
	
	File sourceDirectory = new File(destRList +"\\" + folderValidTestResults);
       
    if(sourceDirectory.exists() && sourceDirectory.isDirectory()){
    	   
    	File sourceFolderList[] = sourceDirectory.listFiles();
        
    	compareFiles(sourceFolderList, folderGenTestResults, folderValidTestResults,0);
    }
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
	public void compareFiles(File[] sourceFiles, String targetFolder, String validTestResults, int depth) {

		for (File sourceFile : sourceFiles) {

			if (sourceFile.isFile()) {

				File targetFile = new File(sourceFile.getAbsolutePath().replaceFirst(validTestResults, targetFolder));

				try {

					boolean areTwoFilesEqual = FileUtils.contentEqualsIgnoreEOL(sourceFile, targetFile, "utf-8");

					System.out.println("Files: " + sourceFile.getCanonicalPath() + " and "
							+ targetFile.getCanonicalPath() + " are equal: " + areTwoFilesEqual);

					/**
					 * 
					 * @author NK510 (caresssd@hermes.cam.ac.uk) Junit test of
					 *         equality the content of two files.
					 * 
					 */
					assertEquals("Difference between files: ", FileUtils.readFileToString(sourceFile, "utf-8"),
							FileUtils.readFileToString(targetFile, "utf-8"));

				} catch (IOException e) {

					e.printStackTrace();

				}

			} else {

				if (sourceFile.isDirectory()) {

					compareFiles(sourceFile.listFiles(), targetFolder, validTestResults, depth + 1);

				}
			}
		}
	}
	
	/**
	 * To generate a paired list of files, it starts with the first folder given by the calling method.  
	 * 
	 * @param folderGenTestResults
	 * @param destRList
	 * @param folderValidTestResults
	 * @return
	 */
	public HashMap<String, String> generatePairedFileList(String folderGenTestResults, String destRList,
			String folderValidTestResults) {

		File sourceDirectory = new File(destRList + "\\" + folderValidTestResults);

		if (sourceDirectory.exists() && sourceDirectory.isDirectory()) {

			File sourceFolderList[] = sourceDirectory.listFiles();

			return generatePairedFileList(sourceFolderList, folderGenTestResults, folderValidTestResults, 0);
		}
		
		return null;
	}
	
	/**
	 * Generates the paired list of validated and code produced test result files. 
	 * 
	 * @param sourceFiles
	 * @param targetFolder
	 * @param validTestResults
	 * @param depth
	 * @return
	 */
	public HashMap<String, String> generatePairedFileList(File[] sourceFiles, String targetFolder, String validTestResults, int depth) {
		for (File sourceFile : sourceFiles) {

			if (sourceFile.isFile()) {

				File targetFile = new File(sourceFile.getAbsolutePath().replaceFirst(validTestResults, targetFolder));
				pairedFileList.put(sourceFile.getAbsolutePath(), targetFile.getAbsolutePath());
			} else {
				if (sourceFile.isDirectory()) {
					generatePairedFileList(sourceFile.listFiles(), targetFolder, validTestResults, depth + 1);
				}
			}
		}
		return pairedFileList;
	}
	
	/**
	 * Compare the values of the first three columns in the given validated and generated test CSV files.
	 * 
	 * @param srcFile
	 * @param sourceFile
	 * @param targetFile
	 * @param resultFileMap
	 */
	public void compareFiles(String srcFilePath, File sourceFile, File targetFile, HashMap<String, String> resultFileMap){
		if(srcFilePath.endsWith(".csv")){
			compareCSVFiles(srcFilePath, resultFileMap);
			return;
		}
		if(srcFilePath.endsWith(".txt")){
			compareTextFiles(srcFilePath, resultFileMap);
			return;
		}
		try{
			System.out.println(srcFilePath+" = "+resultFileMap.get(srcFilePath)+": "+ FileUtils.contentEqualsIgnoreEOL(sourceFile, targetFile, "utf-8"));
			assertEquals(FileUtils.readFileToString(sourceFile, "utf-8"),
					FileUtils.readFileToString(targetFile, "utf-8"));
		}catch(IOException e){
			e.printStackTrace();
		}
	}

	/**
	 * Compares to report if the first three columns of two CSV files have the same content.
	 * 
	 * @param srcFilePath
	 * @param resultFileMap
	 */
	public void compareCSVFiles(String srcFilePath, HashMap<String, String> resultFileMap){
		try{
			List<List<String>> sourceFile = openCSVSourceFile(srcFilePath);
			List<List<String>> targetFile = openCSVSourceFile(resultFileMap.get(srcFilePath));
			
			List<String> sourceListPart = new ArrayList<String>();
			List<String> targetListPart = new ArrayList<String>();

			List<String> sourceListFull = new ArrayList<String>();
			List<String> targetListFull = new ArrayList<String>();

			int count;
			for(List<String> line: sourceFile){
				count = 0;
				for(String column:line){
					count++;
					if(count<=3){
						sourceListPart.add(column);
					}
					sourceListFull.add(column);
				}
			}
			for(List<String> line: targetFile){
				count = 0;
				for(String column:line){
					count++;
					if(count<=3){
						targetListPart.add(column);
					}
					targetListFull.add(column);
				}
			}
			Collections.sort(sourceListPart);
			Collections.sort(targetListPart);
			System.out.println(srcFilePath+" = "+resultFileMap.get(srcFilePath)+": "+ sourceListPart.equals(targetListPart));
			Assert.assertEquals(sourceListPart, targetListPart);
			if(!sourceListFull.equals(targetListFull)){
				System.out.println("Only the first 3 columns of the following two files are identical and a Git Diff like comparator has found some differences between the files:\n   "+srcFilePath+" = "+resultFileMap.get(srcFilePath));
			}
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Compares to report if two text files have the same content.
	 * 
	 * @param srcFilePath
	 * @param resultFileMap
	 */
	public void compareTextFiles(String srcFilePath, HashMap<String, String> resultFileMap){
		try{
			BufferedReader sourceFile = openSourceFile(srcFilePath);
			BufferedReader targetFile = openSourceFile(resultFileMap.get(srcFilePath));
			
			List<String> sourceList = new ArrayList<String>();
			List<String> targetList = new ArrayList<String>();

			String line;
			while((line=sourceFile.readLine())!=null){
				sourceList.add(line);
			}
			while((line=targetFile.readLine())!=null){
				targetList.add(line);
			}

			if(!sourceList.equals(targetList)){
				System.out.println("Only the sorted lists of the reactions found in the following two files are identifical and a Git Diff like comparator has identified some differences between the files:\n   "+srcFilePath+" = "+resultFileMap.get(srcFilePath));
			}
			
			Collections.sort(sourceList);
			Collections.sort(targetList);
			System.out.println(srcFilePath+" = "+resultFileMap.get(srcFilePath)+": "+ sourceList.equals(targetList));
			Assert.assertEquals(sourceList, targetList);
		}catch(IOException e){
			e.printStackTrace();
		}
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
	 */
	public static List<List<String>> openCSVSourceFile(String fileName) throws IOException {
		List<List<String>> records = new ArrayList<List<String>>();
		try (CSVReader csvReader = new CSVReader(new FileReader(fileName));) {
			String[] values = null;
			while ((values = csvReader.readNext()) != null) {
				records.add(Arrays.asList(values));
			}
		}
		return records;
	}
}
