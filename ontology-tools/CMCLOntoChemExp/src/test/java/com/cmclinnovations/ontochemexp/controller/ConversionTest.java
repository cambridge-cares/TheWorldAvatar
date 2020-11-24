package com.cmclinnovations.ontochemexp.controller;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import javax.swing.filechooser.FileFilter;

import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochemexp.model.configuration.OperationControlConfig;
import com.cmclinnovations.ontochemexp.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.utils.Dialogs;

public class ConversionTest {
	private static Logger logger = LoggerFactory.getLogger(ConversionTest.class);
	private ApplicationContext applicationContext;
	private OperationControlConfig opCtrl;
	private OntoChemExp ontoChemExp;
	int primeSerialNo = 0;
	int owlDerialNo = 0;
	int noOfDiffInXmlComment = 0;
	long instanceSerialID = System.nanoTime();
	File folder;
	HashMap<String, String> folderVsSourcePrime = new HashMap<String, String>();
	HashMap<String, String> folderVsGeneratedPrime = new HashMap<String, String>();
	HashMap<String, String> folderVsOwlOfSrcPrime = new HashMap<String, String>();
	HashMap<String, String> folderVsOwlOfGeneratedPrime = new HashMap<String, String>();
	
	@Before
	public void readAllPrime() {
		folder = openPrimeFolder();
	}
	
	@Test
	public void conversionTest() {
		if (folder != null) {
			traverseFolderHierarchy(folder);
			logger.info("Finished the conversion of ".concat(Integer.toString(primeSerialNo)).concat(" PrIMe file(s)"));
		}
		List<String> owlOfSourcePrimeFolders = new ArrayList<String>(folderVsOwlOfSrcPrime.keySet());
		Collections.sort(owlOfSourcePrimeFolders);
		primeSerialNo = 0;
		int numberOfMismatchedFile = 0;
		logger.info("Started comparing files to identify the difference.");
		int difference = 0;
		for (String owlOfSrcPrimeFolder : owlOfSourcePrimeFolders) {
			logger.info("The serial number of the comparison is "+ ++primeSerialNo);
			BufferedWriter bw = getBufferedWriter(folderVsOwlOfGeneratedPrime.get(owlOfSrcPrimeFolder).replace(".owl", ".log"));
			difference = diffBetnOwlOfSrcAndGeneratedPrime(bw, folderVsOwlOfSrcPrime.get(owlOfSrcPrimeFolder), folderVsOwlOfGeneratedPrime.get(owlOfSrcPrimeFolder));
			if (difference != 0) {
				numberOfMismatchedFile++;
				System.out.println(owlOfSrcPrimeFolder);
			}
		}
		if (numberOfMismatchedFile > 0) {
			System.out.println("Out of "+owlOfSourcePrimeFolders.size()+" PrIMe file(s), "+numberOfMismatchedFile+" generated file(s) did not match with source file(s).");
		} else {
			System.out.println("All "+owlOfSourcePrimeFolders.size()+" source and generated PrIMe file(s) are identical.");
		}
	}
	
	/**
	 * Analyses and determines the difference between a source CTML and the 
	 * generated one.  
	 * 
	 * @param bw
	 * @param srcPrime
	 * @param generatedPrime
	 * @return
	 */
	private int diffBetnOwlOfSrcAndGeneratedPrime(BufferedWriter bw, String srcPrime, String generatedPrime) {
		int difference = 0;
		logger.info("Checking the difference between the following two PrIMe files:");
		logger.info("Source    PrIMe file:"+srcPrime);
		logger.info("Generated PrIMe file:"+generatedPrime);
		try {
			bw.write("Checking the difference between the following two PrIMe files:\n");
			bw.write("Source    PrIMe file:"+srcPrime);
			bw.write("\\nGenerated PrIMe file:"+generatedPrime);
			BufferedReader brSourcePrime = openSourceFile(srcPrime);
			BufferedReader brGeneratedPrime = openSourceFile(generatedPrime);
			difference = analyseDifference(bw, brSourcePrime, brGeneratedPrime);
			brSourcePrime.close();
			brGeneratedPrime.close();
			if (difference == 0) {
				bw.write("\nThere is no difference between data elements.");
				if (noOfDiffInXmlComment > 0) {
					bw.write("\nHowever, "+noOfDiffInXmlComment+" XML comments of the source PrIMe was/were not found in the generated PrIMe.");
				}
				logger.info("There is no difference between data elements.");
				if (noOfDiffInXmlComment > 0) {
					logger.info("However, "+noOfDiffInXmlComment+" XML comments of the source PrIMe was/were not found in the generated PrIMe.");
				}
				bw.close();
				noOfDiffInXmlComment = 0;
			} else {
				bw.write("The source PrIMe file and the generated PrIMe file are not identical.");
			}
		} catch (IOException e) {
			logger.error("PrIMe File read failed.");
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
	 * @param brSourcePrime
	 * @param brGeneratedPrime
	 * @return
	 */
	private int analyseDifference(BufferedWriter bw, BufferedReader brSourcePrime, BufferedReader brGeneratedPrime) {
		int difference = 0;
		try {
			String lineSrc;
			String lineGen;
			while ((lineSrc = brSourcePrime.readLine()) != null) {
				if ((lineGen = brGeneratedPrime.readLine()) != null) {
					if (lineSrc.trim().equals(lineGen.trim())) {
					} else {
						if (isThereDiffInDetailedAnalysis(bw, lineSrc, lineGen, brSourcePrime)) {
							difference++;
							bw.write("\nThe following line of the source PrIMe didn't match:\n"+lineSrc);
							System.out.println("\nThe following line of the source PrIMe didn't match:\n"+lineSrc);
							System.out.println("\n"+lineGen);
						}
					}
				}
			}
		} catch(IOException e) {
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
	 * @param brSourcePrime
	 * @return
	 */
	private boolean isThereDiffInDetailedAnalysis(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader brSourcePrime) throws IOException {
		boolean difference = true;

		if (isThereDiffDueToName(bw, lineSrc, lineGen, brSourcePrime)) {
			difference = false;
		}
		if (isThereDiffDueToQuot(bw, lineSrc, lineGen, brSourcePrime)) {
			difference = false;
		}
		
		return difference;
	}
	
	private boolean isThereDiffDueToName(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader beSourcePrime) throws IOException {
		boolean difference = false;
		
		String line = lineGen.replace("_generated", "");
		if (line.trim().equals(lineSrc.trim())) {			
			difference = true;
		}
		
		return difference;
	}
	
	private boolean isThereDiffDueToQuot(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader beSourcePrime) throws IOException {
		boolean difference = false;
		
		String line = lineSrc.replace("&quot;", "\\&quot;").replace("mono&quot;", "mono\\&quot;");
		if (line.trim().equals(lineGen.trim())) {			
			difference = true;
		}
		
		return difference;
	}
	
	
	/**
	 * Requests user to choose a folder which may contain a set of mechanisms 
	 * in PrIMe format.</br> 
	 * Following the selection of a valid path, it converts a list of PrIMe</br>
	 * files into OWL files representing chemical mechanism ontologies.</br>
	 * 
     * @return File the folder chosen by user
     */
	private File openPrimeFolder() {
		File folder = Dialogs.selectDirectoryDialog(new File(System.getProperty("user.home")), new FileFilter[] {}, false);
		if (folder == null) {
		} else if (!folder.exists()) {
			Dialogs.showErrorDialog("Selected folder does not exist.", "Read");
		}
		
		if (folder != null) {
			if (applicationContext == null) {
				applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
			}
			if (opCtrl == null) {
				opCtrl = applicationContext.getBean(OperationControlConfig.class);
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
		System.out.println(folder);
		if (folder != null && folder.getAbsolutePath().endsWith(".xml")) {
			String folderOnly = folder.getAbsolutePath();
			folderOnly = folderOnly.substring(0, folderOnly.lastIndexOf(".xml"));
			logger.info("Processing PrIMe file "+ ++primeSerialNo);
			logger.info("Currently processing the following PrIMe file:"+folder.getAbsolutePath());
			folderVsSourcePrime.put(folderOnly, folder.getAbsolutePath());
			convertPrimeToOwl(folder.getAbsolutePath(), folderOnly);
		}
		if (folder.isDirectory()) {
			String[] subNote = folder.list();
			for (String fileName : subNote) {
				traverseFolderHierarchy(new File(folder, fileName));
			}
		}
	}
	
	/**
	 * Converts a PrIMe file into OWL.
	 * 
	 * @param primeFile the path to the PrIMe file.  
	 * @param absoluteFilePath the absolute path to the file being processed
	 */
	private void convertPrimeToOwl(String primeFile, String owlFilesPath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(primeFile);
		ontoChemExp = OntoChemExpFactory.getOntoChemExp(opCtrl.getOpReadPrime(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoChemExp.convert(instanceSerialID);
			String owlFileName = generateOwlFileName(owlFilesPath);
			folderVsGeneratedPrime.put(owlFilesPath, owlFilesPath.concat("\\".concat(owlFileName).concat("_generated.xml")));
			folderVsOwlOfSrcPrime.put(owlFilesPath.concat("\\").concat("kb"), owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat(".owl"));
			convertOwlToPrime(owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat(".owl"), owlFilesPath);
			convertGeneratedPrimeToOwl(owlFilesPath.concat("\\".concat(owlFileName).concat("_generated.xml")), owlFilesPath);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoChemExpException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Produces the name of the current OWL file from its path.
	 * 
	 * @param owlFilesPath
	 * @return
	 */
	private String generateOwlFileName(String owlFilesPath) {
		if (owlFilesPath.contains("\\")) {
			return owlFilesPath.substring(owlFilesPath.lastIndexOf("\\")+1);
		}
		return owlFilesPath;
	}
	
	/**
	 * Converts an OWL file into PrIMe.
	 * 
	 * @param owlFilePath the absolute path (= path + filename) of the OWL
	 * file currently being converted
	 * @param primeFilePath only the path to the PrIMe file currently being 
	 * generated
	 */
	private void convertOwlToPrime(String owlFilePath, String primeFilePath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(owlFilePath);
		ontoChemExp = OntoChemExpFactory.getOntoChemExp(opCtrl.getOpReadOwl(), opCtrl.getOpWritePrime(), sourceFiles, primeFilePath);
		try {
			ontoChemExp.convert(instanceSerialID);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoChemExpException e) {
			e.printStackTrace();
		}
	}
	
	private void convertGeneratedPrimeToOwl(String primeFile, String owlFilesPath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(primeFile);
		ontoChemExp = OntoChemExpFactory.getOntoChemExp(opCtrl.getOpReadPrime(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoChemExp.convert(instanceSerialID);
			String owlFileName = generateOwlFileName(owlFilesPath);
			folderVsOwlOfGeneratedPrime.put(owlFilesPath.concat("\\").concat("kb"), owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat("_generated.owl"));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoChemExpException e) {
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
			fileCommented = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filePathPlusName), "UTF8"));
		} catch (IOException e) {
			logger.error("A BufferedWriter object for the following file could not be created.");
			e.printStackTrace();
		}
		return fileCommented;
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
	private static BufferedReader openSourceFile(String filePathPlusName) throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(filePathPlusName), "UTF8"));
	}
}
