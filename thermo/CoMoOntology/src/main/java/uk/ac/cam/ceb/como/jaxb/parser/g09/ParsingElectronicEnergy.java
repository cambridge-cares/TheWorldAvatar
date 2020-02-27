package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;

import java.io.FileReader;
import java.io.IOException;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * Parses total energy in Gaussian file.
 *
 */

public class ParsingElectronicEnergy {
	
/**
 * 
 * @param file Input Gaussian file
 * @return a String that represents is total energy
 * @throws IOException
 */
public static String getZeroPointCorrection(File file) throws IOException {
		
		String zeroPointCorrection= "";
		
		BufferedReader br = new BufferedReader(new FileReader(file));
		
 		for (String line; (line = br.readLine()) != null;) {

			if (line.contains("Zero-point correction=")) {
		
				line = line.substring(line.indexOf("=") + 1);
				line =line.substring(0, line.indexOf("("));
				line = line.replaceAll(" ", "");
				
				System.out.println(line);
				
				zeroPointCorrection = line;
			}
		}
 		
		br.close();
		
		System.out.println("Zero point correction: " + zeroPointCorrection);
		
		
		return zeroPointCorrection;
	}

/**
 * 
 * @param file the input file
 * @return the string that represents scf electronic energy.
 * @throws IOException
 */
public static String getSCFElectronicEnergy(File file) throws IOException {
	
	BufferedReader scfEneergyBr = new BufferedReader(new FileReader(file));
		
	String scfElectronicEnergy = "";
		
	for (String line; (line = scfEneergyBr.readLine()) != null;) {

		if (line.contains("SCF Done:")) {
			
			line = line.substring(line.indexOf("=") + 1);
			line=line.substring(line.indexOf(" ")+ 2);
			line =line.substring(0, line.indexOf(" ")); 
			
			scfElectronicEnergy = line;
		}
	}
	
	System.out.println("SCF electronic Energy: " + scfElectronicEnergy);
	
	scfEneergyBr.close();

	return scfElectronicEnergy;
}

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param file the input Gaussian file
 * @return the parameter that contains scalar tag with information about total energy.
 * @throws Exception
 * 
 */
public Property getZeroPointCorrectionProperty(File file) throws Exception {
	
	Property property  = new Property();
	
	Scalar scalar = new Scalar();
	
	property.setDictRef("cc:ZeroPointEnergy");
	
	scalar.setValue(getZeroPointCorrection(file));
	scalar.setUnits("nonSi:hartree");
	scalar.setDataType("xsd:double");
	
	property.getScalarOrArrayOrMatrix().add(scalar);

	return property;
}

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param file the input Gaussian file
 * @return the scf energy for given species (Gaussian file)
 * @throws IOException
 */
public Property getSCFElectronicEnergyProperty(File file) throws IOException {
	
	Property property = new Property();
	
	Scalar scalar = new Scalar();
	
	property.setDictRef("cc:scfenergy");
	
	scalar.setValue(getSCFElectronicEnergy(file));
	scalar.setUnits("nonSi:hartree");
	scalar.setDataType("xsd:double");

	property.getScalarOrArrayOrMatrix().add(scalar);
	
	return property;
}
}