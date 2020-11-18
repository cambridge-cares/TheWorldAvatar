package com.cmclinnovations.ontokin.model.utils;

import org.slf4j.Logger;

import com.cmclinnovations.ontokin.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.exception.OntoException;

public class OwlConverterUtils extends OwlConverter{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(OwlConverterUtils.class);
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
		if (!file.contains("\\")) {
			logger.error("Unexpected file path.");
			throw new OntoException("Unexpected file path.");
		}
		String tokens[] = file.split("\\".concat("\\"));
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

}
