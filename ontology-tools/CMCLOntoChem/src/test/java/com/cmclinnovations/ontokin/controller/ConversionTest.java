package com.cmclinnovations.ontokin.controller;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import javax.swing.filechooser.FileFilter;

import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontokin.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontokin.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;
import com.cmclinnovations.ontokin.model.utils.CtmlConverterUtils;
import com.cmclinnovations.ontokin.model.utils.Dialogs;

/**
 * Tests the similarity between a set of source CTMLs and a generated ones.
 * 
 * @author msff2
 *
 */
public class ConversionTest {
	private static Logger logger = LoggerFactory.getLogger(ConversionTest.class);
	private ApplicationContext applicationContext;
	private AppConfigOperationControl opCtrl;
	private OntoKin ontoKin;
	int ctmlMechanismSerialNo = 0;
	int owlMechanismSerialNo = 0;
	int noOfDiffInXmlComment = 0;
	File folder;
	HashMap<String, String> folderVsSourceCtmls = new HashMap<String, String>();
	HashMap<String, String> folderVsGeneratedCtmls = new HashMap<String, String>();
	
	@Before
	public void readAllMechanisms(){
		folder = openMechanismFolder();
	}
	
	@Test
	public void converstionTest() {
		if(folder!=null){
			traverseFolderHierarchy(folder);
			logger.info("Finished the conversion of ".concat(Integer.toString(ctmlMechanismSerialNo))
					.concat(" mechanism(s)"));
		}
		List<String> sourceCtmlFolders = new ArrayList<String>(folderVsSourceCtmls.keySet());
		Collections.sort(sourceCtmlFolders);
		ctmlMechanismSerialNo = 0;
		int numberOfMismatchedFile = 0;
		logger.info("Started comparing files to identify the diference.");
		int difference = 0;
		for(String srcCtmlFolder: sourceCtmlFolders){
			logger.info("The serial number of the comparison is "+ ++ctmlMechanismSerialNo);
			BufferedWriter bw = getBufferedWriter(folderVsGeneratedCtmls.get(srcCtmlFolder).replace(".xml", ".log"));
			difference = diffBetnSrcAndGeneratedCtml(bw, folderVsSourceCtmls.get(srcCtmlFolder), folderVsGeneratedCtmls.get(srcCtmlFolder));
			if(difference!=0){
				numberOfMismatchedFile++;
			}
			//			assertEquals(0, diffBetnSrcAndGeneratedCtml(bw, folderVsSourceCtmls.get(srcCtmlFolder), folderVsGeneratedCtmls.get(srcCtmlFolder)));
		}
		if(numberOfMismatchedFile>0){
			System.out.println("Out of "+sourceCtmlFolders.size()+ " CTML file(s), "+ numberOfMismatchedFile + " generated file(s) did not match with source file(s).");
		}else{
			System.out.println("All "+sourceCtmlFolders.size()+ " source and generated CTML file(s) are identical.");
		}
	}
	
	/**
	 * Analyses and determines the difference between a source CTML and the 
	 * generated one.  
	 * 
	 * @param bw
	 * @param srcCtml
	 * @param generatedCtml
	 * @return
	 */
	private int diffBetnSrcAndGeneratedCtml(BufferedWriter bw, String srcCtml, String generatedCtml){
		int difference = 0;
		logger.info("Checking the difference between the following two CTML files:");
		logger.info("Source    CTML file:"+srcCtml);
		logger.info("Generated CTML file:"+generatedCtml);
		try {
			bw.write("Checking the difference between the following two CTML files:\n");
			bw.write("Source    CTML file:"+srcCtml);
			bw.write("\nGenerated CTML file:"+generatedCtml);
			BufferedReader brSourceCtml = openSourceFile(srcCtml);
			BufferedReader brGeneratedCtml = openSourceFile(generatedCtml);
			difference = analyseDifference(bw, brSourceCtml, brGeneratedCtml);
			brSourceCtml.close();
			brGeneratedCtml.close();
			if(difference==0){
				bw.write("\nThere is no difference between data elements.");
				if(noOfDiffInXmlComment>0){
					bw.write("\nHowever, "+noOfDiffInXmlComment+" XML comments of the source CTML was/were not found in the generated CTML.");
				}
				logger.info("There is no difference between data elements.");
				if(noOfDiffInXmlComment>0){
					logger.info("However, "+noOfDiffInXmlComment+" XML comments of the source CTML was/were not found in the generated CTML.");
				}
				bw.close();
				noOfDiffInXmlComment=0;
			} else{
				bw.write("The source CTML file and the generated CTML file are not identical.");
			}
		} catch (IOException e) {
			logger.error("CTML File read failed.");
			e.printStackTrace();
		}
		return difference;
	}
	
	/**
	 * Checks the simple equality check and if it does not find any difference
	 * it returns 0, otherwise it forwards the call to isThereDiffInDetailedAnalysis
	 * for detailed analysis.
	 * 
	 * @param bw
	 * @param brSourceCtml
	 * @param brGeneratedCtml
	 * @return
	 */
	private int analyseDifference(BufferedWriter bw, BufferedReader brSourceCtml, BufferedReader brGeneratedCtml){
		int difference = 0;
		try{
		String lineSrc;
		String lineGen;
		while ((lineSrc = brSourceCtml.readLine()) != null) {
			if((lineGen = brGeneratedCtml.readLine()) != null) {
				if(lineSrc.trim().equals(lineGen.trim())){
				} else{
					if(isThereDiffInDetailedAnalysis(bw, lineSrc, lineGen, brSourceCtml)){
						difference++;
						bw.write("\nThe following line of the source CTMLdidn't match:\n"+lineSrc);
						System.out.println("\nThe following line of the source CTMLdidn't match:\n"+lineSrc);
					}
				}
			}
		}
		}catch(IOException e){
			logger.error("File read/write failed.");
		}
		return difference;
	}
	
	/**
	 * Performs detailed matching between each line of the source and 
	 * generated CTML files.
	 * 
	 * @param bw
	 * @param lineSrc
	 * @param lineGen
	 * @param brSourceCtml
	 * @return
	 */
	private boolean isThereDiffInDetailedAnalysis(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader brSourceCtml) throws IOException{
		boolean difference = true;
		String line;
		if(lineSrc.trim().startsWith("<?xml ") && lineGen.trim().startsWith("<?xml ")){
			return false;
		} else if(lineGen.trim().contains(lineSrc.trim())){
			while ((line = brSourceCtml.readLine()) != null) {
				if(line.trim().isEmpty()){
					continue;
				}
				if (lineGen.trim().contains(line.trim())) {
					return false;
				}
			}
		} else{
			difference = isThereDiffDueToXmlComment(bw, lineSrc, lineGen, brSourceCtml);
		}
		return difference;
	}
	
	/**
	 * Checks if the difference is due to xml comment.
	 * 
	 * @param bw
	 * @param line
	 * @param lineGen
	 * @param brSourceCtml
	 * @return
	 * @throws IOException
	 */
	private boolean isThereDiffDueToXmlComment(BufferedWriter bw, String line, String lineGen, BufferedReader brSourceCtml) throws IOException{
		boolean difference = true;
		if(line.trim().startsWith("<!--")){
			noOfDiffInXmlComment++;
			bw.write("\nThe following xml comment line of the source CTML file didn't match:\n"+line);
			System.out.println("\nThe following xml comment line of the source CTML file didn't match:\n"+line);
			while ((line = brSourceCtml.readLine()) != null){
				if(!line.trim().startsWith("<!--")){
					break;
				}
				bw.write("\nThe following xml comment line of the source CTML file didn't match:\n"+line);
				System.out.println("\nThe following xml comment line of the source CTML file didn't match:\n"+line);
			}
			if (lineGen.trim().contains(line.trim())) {
				return false;
			}
		}
		return difference;
	}
	
	/**
	 * Requests user to choose a folder which may contain a set of mechanisms 
	 * in CTML format.</br> 
	 * Following the selection of a valid path, it converts a list of CTML</br>
	 * files into OWL files representing chemical mechanism ontologies.</br>
	 * 
     * @return File the folder chosen by user
     */
	private File openMechanismFolder() {
		File folder = Dialogs.selectDirectoryDialog(new File(System.getProperty("user.home")), new FileFilter[] {},
				false);
		if (folder == null) {
		} else if (!folder.exists()) {
			Dialogs.showErrorDialog("Selected folder does not exist.", "Read");
		}

		if (folder != null) {
			if (applicationContext == null) {
				applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
			}
			if (opCtrl == null) {
				opCtrl = applicationContext.getBean(AppConfigOperationControl.class);
			}
		}
		return folder;
    }

	/**
	 * Traverses the folder hierarchy rooted at the folder chosen by user. 
	 * 
	 * @param folder the root folder of a hierarchy
	 */
	private void traverseFolderHierarchy(File folder) {
		if(folder!=null && folder.getAbsolutePath().endsWith("\\mechanism.xml")){
			String folderOnly = folder.getAbsolutePath();
			folderOnly = folderOnly.substring(0, folderOnly.lastIndexOf("\\mechanism.xml"));
			logger.info("Processing mechanism "+ ++ctmlMechanismSerialNo);
			logger.info("Currently processing the following mechanism:"+folder.getAbsolutePath());
			folderVsSourceCtmls.put(folderOnly, folder.getAbsolutePath());			
			convertCtmlToOwl(folder.getAbsolutePath(), folderOnly);
		}
		if (folder.isDirectory()) {
			String[] subNote = folder.list();
			for (String fileName : subNote) {
				traverseFolderHierarchy(new File(folder, fileName));
			}
		}
	}
	
	/**
	 * Converts a CTML file into OWL.
	 * 
	 * @param ctmlFile the path to the CTML file.  
	 * @param absoluteFilePath the absolute path to the file being processed
	 */
	private void convertCtmlToOwl(String ctmlFile, String owlFilesPath){
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(ctmlFile);
		ontoKin = OntoKinFactory.getOntoKin(opCtrl.getOpReadCtml(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoKin.convert();
			String owlFileName = generateOwlFileName(owlFilesPath);
			folderVsGeneratedCtmls.put(owlFilesPath, owlFilesPath.concat("\\").concat(owlFileName).concat("_with_comment.xml"));
			convertOwlToCtml(owlFilesPath.concat("\\").concat(owlFileName).concat(".owl"), owlFilesPath);
		} catch (OWLOntologyCreationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch(OntoException e){
			e.printStackTrace();
		}

	}
	/**
	 * Produces the name of the current OWL file from its path.
	 * 
	 * @param owlFilesPath
	 * @return
	 */
	private String generateOwlFileName(String owlFilesPath){
		if(owlFilesPath.contains("\\")){
			return owlFilesPath.substring(owlFilesPath.lastIndexOf("\\")+1);
		}
		return owlFilesPath;
	}
	
	/**
	 * Converts an OWL file into CTML.
	 * 
	 * @param owlFilePath the absolute path (= path + filename) of the OWL
	 * file currently being converted
	 * @param ctmlFilePath only the path to the CTML file currently being 
	 * generated
	 */
	private void convertOwlToCtml(String owlFilePath, String ctmlFilePath){
		
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(owlFilePath);
		ontoKin = OntoKinFactory.getOntoKin(opCtrl.getOpReadOwl(), opCtrl.getOpWriteCtml(), sourceFiles, ctmlFilePath);
		try {
			ontoKin.convert();
		} catch (OWLOntologyCreationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch(OntoException e){
			e.printStackTrace();
		}

	}
	
	/**
	 * Produces an instance of BufferedWriter to write a file.
	 * 
	 * @param filePathPlusName
	 */
	private BufferedWriter getBufferedWriter(String filePathPlusName) {
		BufferedWriter fileCommented = null;
		try {
			fileCommented = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(filePathPlusName), "UTF8"));
		} catch (IOException e) {
			logger.error("A BufferedWriter object for the following file could not be created.");
			e.printStackTrace();
		}
		return fileCommented;
	}
	
	/**
	 * Creates a new file by adding source comments to the OntoKin generated 
	 * CTML file which does not contain any comment.
	 * 
	 * @param filePathPlusName
	 */
	private void addSourceCommentToCtmlObjects(String filePathPlusName) {
		try {
			BufferedWriter fileCommented = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(filePathPlusName.replace(".xml", "_with_comment.xml")), "UTF8"));
			BufferedReader br = openSourceFile(filePathPlusName);
			String line;
			while ((line = br.readLine()) != null) {
				
			}
			fileCommented.close();
			br.close();
		} catch (IOException e) {
			logger.error("OntoKin generated CTML file read error.");
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @param path
	 *            the path name of the file
	 * @param name
	 *            a file name
	 * @return
	 * @throws IOException
	 */
	private static BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF8"));
	}
	
}
