package com.cmclinnovations.prime.species.controller;

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

import com.cmclinnovations.prime.species.model.configuration.OperationControlConfig;
import com.cmclinnovations.prime.species.model.configuration.SpringConfiguration;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;
import com.cmclinnovations.prime.species.model.utils.Dialogs;

public class ConversionTest {
	private static Logger logger = LoggerFactory.getLogger(ConversionTest.class);
	private ApplicationContext applicationContext;
	private OperationControlConfig opCtrl;
	private OntoPrimeSpecies ontoPrimeSpecies;
	int speciesSerialNo = 0;
	int owlSerialNo = 0;
	int noOfDiffInXmlComment = 0;
	long instanceSerialID = System.nanoTime();
//	long instanceSerialID = 0;
	File folder;
	HashMap<String, String> folderVsSourceSpecies = new HashMap<String, String>();
	HashMap<String, String> folderVsGeneratedSpecies = new HashMap<String, String>();
	HashMap<String, String> folderVsOwlOfSourceSpecies = new HashMap<String, String>();
	HashMap<String, String> folderVsOwlOfGeneratedSpecies = new HashMap<String, String>();
	
	@Before
	public void readAllSpecies() {
		folder = openSpeciesFolder();
	}
	
	@Test
	public void conversionTest() {
		if (folder != null) {
			traverseFolderHierarchy(folder);
			logger.info("Finished the conversion of ".concat(Integer.toString(speciesSerialNo)).concat(" PrIMeSpecies file(s)"));
		}
		List<String> owlOfSourceSpeciesFolders = new ArrayList<String>(folderVsOwlOfSourceSpecies.keySet());
		Collections.sort(owlOfSourceSpeciesFolders);
		speciesSerialNo = 0;
		int numberOfMismatchedFile = 0;
		logger.error("Started comparing files to identify the difference.");
		int difference = 0;
		for (String owlOfSourceSpeciesFolder : owlOfSourceSpeciesFolders) {
//			logger.info("The serial number of the comparison is "+ ++speciesSerialNo);
			BufferedWriter bw = getBufferedWriter(folderVsOwlOfGeneratedSpecies.get(owlOfSourceSpeciesFolder).replace(".owl", ".log"));
			difference = diffBetnOwlOfSrcAndGeneratedSpecies(bw, folderVsOwlOfSourceSpecies.get(owlOfSourceSpeciesFolder), folderVsOwlOfGeneratedSpecies.get(owlOfSourceSpeciesFolder));
			if (difference != 0) {
				numberOfMismatchedFile++;
				System.out.println(owlOfSourceSpeciesFolder);
			}
		}
		if (numberOfMismatchedFile > 0) {
			System.out.println("Out of "+owlOfSourceSpeciesFolders.size()+" PrIMeSpecies file(s), "+numberOfMismatchedFile+" generated file(s) did not match with source file(s).");
		} else {
			System.out.println("All "+owlOfSourceSpeciesFolders.size()+" source and generated PrIMeSpecies file(s) are identical.");
		}
	}
	
	/**
	 * Analyses and determines the difference between a source CTML and the 
	 * generated one.  
	 * 
	 * @param bw
	 * @param srcSpecies
	 * @param generatedSpecies
	 * @return
	 */
	private int diffBetnOwlOfSrcAndGeneratedSpecies(BufferedWriter bw, String srcSpecies, String generatedSpecies) {
		int difference = 0;
//		logger.info("Checking the difference between the following two PrIMeSpecies files:");
//		logger.info("Source    PrIMeSpecies file:"+srcSpecies);
//		logger.info("Generated PrIMeSpecies file:"+generatedSpecies);
		try {
			bw.write("Checking the difference between the following two PrIMeSpecies files:\n");
			bw.write("Source    PrIMeSpecies file:"+srcSpecies);
			bw.write("\\nGenerated PrIMeSpecies file:"+generatedSpecies);
			BufferedReader brSourceSpecies = openSourceFile(srcSpecies);
			BufferedReader brGeneratedSpecies = openSourceFile(generatedSpecies);
			difference = analyseDifference(bw, brSourceSpecies, brGeneratedSpecies);
			brSourceSpecies.close();
			brGeneratedSpecies.close();
			if (difference == 0) {
				bw.write("\nThere is no difference between data elements.");
				if (noOfDiffInXmlComment > 0) {
					bw.write("\nHowever, "+noOfDiffInXmlComment+" XML comments of the source PrIMeSpecies was/were not found in the generated PrIMeSpecies.");
				}
//				logger.info("There is no difference between data elements.");
				if (noOfDiffInXmlComment > 0) {
					logger.info("However, "+noOfDiffInXmlComment+" XML comments of the source PrIMeSpecies was/were not found in the generated PrIMeSpecies.");
				}
				bw.close();
				noOfDiffInXmlComment = 0;
			} else {
				bw.write("The source PrIMeSpecies file and the generated PrIMeSpecies file are not identical.");
			}
		} catch (IOException e) {
			logger.error("PrIMeSpecies File read failed.");
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
	 * @param brSourceSpecies
	 * @param brGeneratedSpecies
	 * @return
	 */
	private int analyseDifference(BufferedWriter bw, BufferedReader brSourceSpecies, BufferedReader brGeneratedSpecies) {
		int difference = 0;
		try {
			String lineSrc;
			String lineGen;
			while ((lineSrc = brSourceSpecies.readLine()) != null) {
				if ((lineGen = brGeneratedSpecies.readLine()) != null) {
					if (lineSrc.trim().equals(lineGen.trim())) {
					} else {
						if (isThereDiffInDetailedAnalysis(bw, lineSrc, lineGen, brSourceSpecies)) {
							difference++;
							bw.write("\nThe following line of the source PrIMeSpecies didn't match:\n"+lineSrc);
//							System.out.println("\nThe following line of the source PrIMeSpecies didn't match:\n"+lineSrc);
//							System.out.println("\n"+lineGen);
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
	 * @param brSourceSpecies
	 * @return
	 */
	private boolean isThereDiffInDetailedAnalysis(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader brSourceSpecies) throws IOException {
		boolean difference = true;
		
		if (isThereDiffDueToName(bw, lineSrc, lineGen, brSourceSpecies)) {
			difference = false;
		}
		
		if (isThereDiffDueToQuot(bw, lineSrc, lineGen, brSourceSpecies)) {
			difference = false;
		}
		
		return difference;
	}
	
	private boolean isThereDiffDueToName(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader beSourceSpecies) throws IOException {
		boolean difference = false;
		
		String line = lineGen.replace("_generated", "");
		if (line.trim().equals(lineSrc.trim())) {			
			difference = true;
		}
		
		return difference;
	}
	
	private boolean isThereDiffDueToQuot(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader beSourceSpecies) throws IOException {
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
	private File openSpeciesFolder() {
		File folder = Dialogs.selectDirectoryDialog(new File(System.getProperty("user.name")), new FileFilter[] {}, false);
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
			logger.info("Processing PrIMeSpecies file "+ ++speciesSerialNo);
			logger.info("Currently processing the following PrIMeSpecies file:"+folder.getAbsolutePath());
			folderVsSourceSpecies.put(folderOnly, folder.getAbsolutePath());
			convertSpeciesToOwl(folder.getAbsolutePath(), folderOnly);
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
	 * @param speciesFile the path to the PrIMe file.  
	 * @param absoluteFilePath the absolute path to the file being processed
	 */
	private void convertSpeciesToOwl(String speciesFile, String owlFilesPath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(speciesFile);
		ontoPrimeSpecies = OntoPrimeSpeciesFactory.getOntoPrimeSpecies(opCtrl.getOpReadSpecies(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoPrimeSpecies.convert(instanceSerialID);
			String owlFileName = generateOwlFileName(owlFilesPath);
			folderVsGeneratedSpecies.put(owlFilesPath, owlFilesPath.concat("\\".concat(owlFileName).concat("_generated.xml")));
			folderVsOwlOfSourceSpecies.put(owlFilesPath.concat("\\").concat("kb"), owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat(".owl"));
			convertOwlToSpecies(owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat(".owl"), owlFilesPath);
			convertGeneratedSpeciesToOwl(owlFilesPath.concat("\\".concat(owlFileName).concat("_generated.xml")), owlFilesPath);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoPrimeSpeciesException e) {
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
	 * @param speciesFilePath only the path to the PrIMe file currently being 
	 * generated
	 */
	private void convertOwlToSpecies(String owlFilePath, String speciesFilePath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(owlFilePath);
		ontoPrimeSpecies = OntoPrimeSpeciesFactory.getOntoPrimeSpecies(opCtrl.getOpReadOwl(), opCtrl.getOpWriteSpecies(), sourceFiles, speciesFilePath);
		try {
			ontoPrimeSpecies.convert(instanceSerialID);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoPrimeSpeciesException e) {
			e.printStackTrace();
		}
	}
	
	private void convertGeneratedSpeciesToOwl(String speciesFile, String owlFilesPath) {
		ArrayList<String> sourceFiles = new ArrayList<String>();
		sourceFiles.add(speciesFile);
		ontoPrimeSpecies = OntoPrimeSpeciesFactory.getOntoPrimeSpecies(opCtrl.getOpReadSpecies(), opCtrl.getOpWriteOwl(), sourceFiles, owlFilesPath);
		try {
			ontoPrimeSpecies.convert(instanceSerialID);
			String owlFileName = generateOwlFileName(owlFilesPath);
			folderVsOwlOfGeneratedSpecies.put(owlFilesPath.concat("\\").concat("kb"), owlFilesPath.concat("\\").concat("kb").concat("\\").concat(owlFileName).concat("_generated.owl"));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OntoPrimeSpeciesException e) {
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
