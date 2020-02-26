package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.PropertyList;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingElectronicEnergy;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingFrequencies;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometryType;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingRotationalConstants;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingRotationalSymmetry;

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * The Class GenerateProperty in final module.
 */
public class GenerateProperty {

/**
 * Gets the property list final module.
 *
 * @param file the Gaussian file (g09). 
 * @return the property list final module.
 * @throws Exception the exception.
 * 
 */
public static PropertyList getPropertyListFinalModule(File file) throws Exception {
		
		ParsingRotationalSymmetry prs = new ParsingRotationalSymmetry();
		ParsingRotationalConstants rcp = new ParsingRotationalConstants();
		
		ParsingFrequencies pf = new ParsingFrequencies();
		ParsingGeometryType pgt = new ParsingGeometryType();
		
		ParsingElectronicEnergy electronicEnergy = new ParsingElectronicEnergy();
		
		PropertyList pList = new PropertyList();
		
		/**
		 * @author nk510 
		 * <p>Generates 'frequencies'.</p>
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation()
				.add(pf.generateFrequenciesFromG09(file.getAbsoluteFile()));

		/**
		 * 
		 * @author nk510 
		 * <p>Generates 'rotational symmetry'.</p>
		 * 
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation()
		.add(prs.generateRotationalSymmetryFromG09(file.getAbsoluteFile()));

		/**
		 * 
		 * @author nk510 
		 * <p>Generates 'rotational constants'.</p>
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation()
		.add(rcp.generateRotationalConstantsFromG09(file.getAbsoluteFile()));

		/**
		 * 
		 * @author nk510
		 * <p>Generates 'geometry type'.</p>
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation()
		.add(pgt.getGeometryTypeFromG09(file.getAbsoluteFile()));		
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * <p>Generates 'zero point correction'</p>
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation().add(electronicEnergy.getZeroPointCorrectionProperty(file.getAbsoluteFile()));
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * <p>Generates 'scf electronic energy'</p>
		 * 
		 */
		pList.getPropertyOrPropertyListOrObservation().add(electronicEnergy.getSCFElectronicEnergyProperty(file));
		
		return pList;
	}
}