package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.io.IOException;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.ParameterList;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingBasisSet;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingEnvironment;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingLevelOfTheory;

/**
 * The Class GenerateParameter.
 *
 * @author nk510
 */
public class GenerateParameter {

/**
 * Gets the parameter list environment module.
 *
 * @param file the Gaussian file.
 * @return the parameter list in environment module.
 * @throws IOException Signals that an I/O exception has occurred.
 */
public static ParameterList getParameterListEnvironmentModule(File file) throws IOException {
		
		ParameterList pList = new ParameterList();
		
		pList.getParameterOrParameterList().add(ParsingEnvironment.getProgramName(file));
		pList.getParameterOrParameterList().add(ParsingEnvironment.getProgramVersion(file));
        pList.getParameterOrParameterList().add(ParsingEnvironment.getRunDateStamp(file));
		
		return pList;
	}

/**
 * Gets the parameter list initial module.
 *
 * @param file the Gaussian file.
 * @param sumOfAtoms the sum of atoms.
 * @return the parameter list in initial module.
 * @throws Exception the exception.
 */
public static ParameterList getParameterListInitialModule(File file, int sumOfAtoms ) throws Exception {
	
	ParameterList pList = new ParameterList();
	
	pList.getParameterOrParameterList().add(ParsingLevelOfTheory.getLevelOfTheryParameter(file, sumOfAtoms));
	pList.getParameterOrParameterList().add(ParsingBasisSet.getBasisSetParameter(file, sumOfAtoms));
	
	return pList;
}

}
