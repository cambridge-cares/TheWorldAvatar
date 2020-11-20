package com.cmclinnovations.conversion;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.conversion.utils.CompletenessUtils;

/**
 * This class contains methods that verifies the completeness of a conversion 
 * from a source to a target.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class Completeness implements ICompleteness{
	private Logger logger = LoggerFactory.getLogger(Completeness.class);
	int difference = 0;
	int noOfDiffInXmlComment = 0;
	
	/**
	 * Analyses and determines the difference between a source file and the 
	 * one generated from it.
	 * 
	 * @param bw
	 * @param sourceFile
	 * @param generatedFile
	 * @return
	 */
	public void reportDifference(String reportFile, String sourceFile, String generatedFile){
		difference = 0;
		logger.info("Checking differences between the following two files:");
		logger.info("Source file:"+sourceFile);
		logger.info("Generated file:"+generatedFile);
		try {
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(reportFile), "UTF-8"));
			bw.write("                            Conversion Report");
			bw.write("\n----------------------------------------------------------------------------");
			bw.write("\nThis report describes the difference between the following two files:\n");
			bw.write("Source file:"+sourceFile);
			bw.write("\nGenerated file:"+generatedFile);
			bw.write("\n------------------------------------------------------------------------------");
			bw.write("\n The following content of the source file was not found in the generated one.");
			bw.write("\n------------------------------------------------------------------------------");
			BufferedReader brSourceFile = CompletenessUtils.openSourceFile(sourceFile);
			BufferedReader brGeneratedFile = CompletenessUtils.openSourceFile(generatedFile);
			difference = analyseDifference(bw, brSourceFile, brGeneratedFile);
			brSourceFile.close();
			brGeneratedFile.close();
			if(difference==0){
				bw.write("\n----------------------------------------------------------------------------");
				bw.write("\nFinal comment:");
				bw.write("\n----------------------------------------------------------------------------");
				bw.write("\nThe source and generated file are not identical.");
				bw.close();
			}
			bw.close();
		} catch (IOException e) {
			logger.error("IOException occurred. Please see below for details:");
			e.printStackTrace();
		}
	}

	/**
	 * Checks the simple equality check and if it does not find any difference
	 * it returns 0, otherwise it forwards the call to isThereDiffInDetailedAnalysis
	 * for detailed analysis.
	 * 
	 * @param bw
	 * @param brSourceFile
	 * @param brGeneratedFile
	 * @return
	 */
	private int analyseDifference(BufferedWriter bw, BufferedReader brSourceFile, BufferedReader brGeneratedFile){
		try{
		String lineSrc;
		String lineGen;
		while ((lineSrc = brSourceFile.readLine()) != null) {
			if((lineGen = brGeneratedFile.readLine()) != null) {
				if(lineSrc.trim().equals(lineGen.trim())){
				} else{
					if(isThereDiffInDetailedAnalysis(bw, lineSrc, lineGen, brSourceFile)){
						difference++;
						bw.write("\n"+lineSrc);
					}
				}
			}
		}
		}catch(IOException e){
			logger.error("IOException occurred. Please see below for details:");
			e.printStackTrace();
		}
		return difference;
	}

	/**
	 * Performs detailed matching between each line of the source file and 
	 * the generated one.
	 * 
	 * @param bw
	 * @param lineSrc
	 * @param lineGen
	 * @param brSourceFile
	 * @return
	 */
	private boolean isThereDiffInDetailedAnalysis(BufferedWriter bw, String lineSrc, String lineGen, BufferedReader brSourceFile) throws IOException{
		boolean differenceFlag = true;
		String line;
		if(lineSrc.trim().startsWith("<?xml ") && lineGen.trim().startsWith("<?xml ")){
			return false;
		} else if(lineSrc.trim().contains("<?xml ") && lineGen.trim().contains("<?xml ")){
			return false;
		} else if(lineGen.trim().length()>1 && lineSrc.trim().contains(lineGen.trim().substring(0, lineGen.trim().length()-1))){
			return false;
		} else if(!lineSrc.trim().startsWith("<!--")){ 
			bw.write("\n"+lineSrc);
			while ((line = brSourceFile.readLine()) != null) {
				if (lineGen.trim().contains(line.trim())) {
					return false;
				} else {
					bw.write("\n"+line);
					difference++;
				}
			}
		} else if(lineGen.trim().contains(lineSrc.trim())){
			while ((line = brSourceFile.readLine()) != null) {
				if(line.trim().isEmpty()){
					continue;
				}
				if (lineGen.trim().contains(line.trim())) {
					return false;
				}
			}
		} else{
			differenceFlag = isThereDiffDueToXmlComment(bw, lineSrc, lineGen, brSourceFile);
		}
		return differenceFlag;
	}

	/**
	 * Checks if the difference is due to an xml comment.
	 * 
	 * @param bw
	 * @param line
	 * @param lineGen
	 * @param brSourceFile
	 * @return
	 * @throws IOException
	 */
	private boolean isThereDiffDueToXmlComment(BufferedWriter bw, String line, String lineGen, BufferedReader brSourceFile) throws IOException{
		boolean difference = true;
		if(line.trim().startsWith("<!--")){
			noOfDiffInXmlComment++;
			bw.write("\nThe following xml comment line of the source file didn't match:\n"+line);
			System.out.println("\nThe following xml comment line of the source file didn't match:\n"+line);
			while ((line = brSourceFile.readLine()) != null){
				if(!line.trim().startsWith("<!--")){
					break;
				}
				bw.write("\nThe following xml comment line of the source file didn't match:\n"+line);
				System.out.println("\nThe following xml comment line of the source file didn't match:\n"+line);
			}
			if (lineGen.trim().contains(line.trim())) {
				return false;
			}
		}
		return difference;
	}
}
