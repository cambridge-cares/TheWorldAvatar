package com.cmclinnovations.ontochem.model.utils;

import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.persistence.internal.jpa.metadata.structures.ArrayAccessor;
import org.slf4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.model.configuration.AppConfigOperationControl;
import com.cmclinnovations.ontochem.model.configuration.OntoChemKB;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.kb.server.IRepositoryManager;
import com.cmclinnovations.ontochem.model.kb.server.RepositoryManager;

public class OwlConverterUtils extends OwlConverter{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(OwlConverterUtils.class);
    static IRepositoryManager iRepositoryManger = new RepositoryManager();
	static String serverURL = "http://172.24.155.69:8080/rdf4j-server/";
	private static final String repositoryID = "ontochem";
	/**
	 * Forms the absolute path including the name of the CTML file being</br> 
	 * generated using:</br>
	 * (i) the absolute path including the name of the OWL file being</br> 
	 * converted, and</br>
	 * (ii) the path user indicates to save the CTML file following the</br> 
	 * conversion.
	 * 
	 * @param owlFileAbsolutePath
	 * @param ctmlFilePath
	 *   
	 * @return the absolute path including the name of the CTML file being 
	 * generated
	 * @throws OntoException
	 */
	public static String formCtmlAbsoultePath(String owlFileAbsolutePath, String ctmlFilePath) throws OntoException {
		if (owlFileAbsolutePath == null) {
			logger.error("Provided owlFileAbsolutePath path is null.");
			throw new OntoException("Provided owlFileAbsolutePath path is null.");
		}
		if (ctmlFilePath == null) {
			logger.error("Provided ctmlFilePath is null.");
			throw new OntoException("Provided ctmlFilePath is null.");
		}
		String owlMechanismName = extractOwlMechanismName(owlFileAbsolutePath);
		owlMechanismName = owlMechanismName.concat(".xml");
		return ctmlFilePath.concat("\\").concat(owlMechanismName);
	}

	/**
	 * Extracts the name of the mechanism being processed from an OWL file path. 
	 * 
	 * @param file The file path
	 * @return String returns the name of the file 
	 * @throws OntoException
	 */
	public static String extractOwlMechanismName(String file) throws OntoException {
		String tokens[] = {};
		if (file.contains("\\")) {
			tokens = file.split("\\".concat("\\"));
		}
		if (file.contains("/")) {
			tokens = file.split("/");
		}	
		if(tokens!=null){
		if(tokens.length<2){
			logger.error("The file path is unexpectedly short.");
			throw new OntoException("The file path is unexpectedly short.");
		}
		if(!tokens[tokens.length-1].contains(".owl")){
			logger.error("Provided owlMechanismName does not contain the owl extension.");
			throw new OntoException("Provided owlMechanismName does not contain the owl extension.");
		}
		return tokens[tokens.length-1].replace(".owl", "");
		}
		throw new OntoException("The name of the mechanism from the OWL path:"+file+", could not be extracted.");
	}

	/**
	 * Converts an OWL lass name into a CTML compatible class name.
	 * 
	 * @param classOwlName the name of the reaction class found in OWL. 
	 * @return String CTML compatible class name of the OWL class. 
	 * @throws OntoException
	 */
	public static String getReactionClass(String classOwlName) throws OntoException{
		if(classOwlName==null){
			logger.error("CTML class name requested to be converted to an OWL class name is null.");
			throw new OntoException("CTML class name requested to be converted to an OWL class name is null.");
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassPLOGReaction())){
			return appConfigCtml.getReactionTypeFallOffPLOGValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassLindemannReaction())){
			return appConfigCtml.getReactionTypeFallOffLindemannValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassSRIReaction())){
			return appConfigCtml.getReactionTypeFallOffSRIValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassTroeReaction())){
			return appConfigCtml.getReactionTypeFallOffTroeValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassCHEBReaction())){
			return appConfigCtml.getReactionTypeFallOffCHEBValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassFallOffReaction())){
			return appConfigCtml.getReactionTypeFallOffValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassSurfaceReaction())){
			return appConfigCtml.getReactionTypeSurfaceValue();
		}
		if(classOwlName.equalsIgnoreCase(appConfigOntokin.getClassThreeBodyReaction())){
			return appConfigCtml.getReactionTypeThreeBodyValue();
		}
		return null;
	}
    /**
     * Downloads an ontology from a URL.
     * 
     * @param filename it represents both the path to the file and its name</br>
     * including extension
     * @param fileURL
     * @throws MalformedURLException
     * @throws IOException
     */
    public static void downloadFileFromUrl(final String filename, final String fileURL)
			throws MalformedURLException, IOException {
		BufferedInputStream in = null;
		FileOutputStream fout = null;
		try {
			String mechanismContextIRI = fileURL;
			String[] tokens = mechanismContextIRI.split("#");
			if (mechanismContextIRI.contains("#")) {
				tokens = mechanismContextIRI.split("#");
			}
			if (tokens!=null && tokens[0].endsWith(".owl")) {
				if (applicationContext == null) {
					applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
				}
				if (ontoChemKB == null) {
					ontoChemKB = applicationContext.getBean(OntoChemKB.class);
				}
				try {
					iRepositoryManger.downloadOntology(ontoChemKB.getOntoChemKBRDF4JServerUrl(), mechanismContextIRI,
							tokens[0], ontoChemKB.getOntoChemKBRDF4JRepositoryId(), filename);
				} catch (OntoException e) {
					e.printStackTrace();
					logger.error("RepositoryManger failed to download the following OWL file:" + fileURL);
					throw new IOException();
				}
			}
		} finally {
			if (in != null) {
				in.close();
			}
			if (fout != null) {
				fout.close();
			}
		}
	}
    
	/**
	 * Each CTML comment starts with either \- or \+ to mean line break or</br>
	 * continuation, respectively. When a comment is extracted from OWL, these</br>
	 * special characters are put in place to make it CTML compatible.</br>
	 * 
	 * If a line is longer that 80 characters, it is broken into a new line</br>
	 * at the end of the 80th character. When a line break operation is</br>
	 * carried out, it is marked with \+. Usual line separation is marked</br>
	 * with \-.  
	 * 
	 * @param comment any comment read from an OWL file.
	 * @return the comment with additional line break (\-) and 
	 * continuation (\+) characters. 
	 */
	public static String cTMLifyComment(String comment){
		if(comment==null || comment.trim().isEmpty()){
			logger.info("The comment is null or emptry so no CTMLification is needed.");
			return comment;
		}
		String[] lines = comment.split("\n");
		String commentLines = "";
		for(String line:lines){
			if(line.length()>80){
				commentLines = getTokens(commentLines, line);
			} else{
				commentLines = commentLines.concat("\\-").concat(line).concat("\n");
			}
		}
		return commentLines;
	}

	
	/**
	 * For comment lines which are longer than 80 characters, this method</br>
	 * breaks them into multiple 80 line characters. 
	 * 
	 * @param commentLines
	 * @param line
	 * @return
	 */
	public static String getTokens(String commentLines, String line){
		int length = line.length();
		int ctmlLineLength = 80;
		int multiplier = 0;
		for(int i=0;i<length;i=ctmlLineLength+i){
			multiplier++;
			if(i==0){
				commentLines = commentLines.concat("\\-");
			}
			if((multiplier*ctmlLineLength)<length){
				if(i==0){
					commentLines = commentLines.concat(line.substring(i, multiplier*ctmlLineLength)).concat("\n");
				}else{
					commentLines = commentLines.concat("\\+").concat(line.substring(i, multiplier*ctmlLineLength)).concat("\n");
				}
			}else{
				commentLines = commentLines.concat("\\+").concat(line.substring(i, length)).concat("\n");
			}
		}
		return commentLines;
	}
}
