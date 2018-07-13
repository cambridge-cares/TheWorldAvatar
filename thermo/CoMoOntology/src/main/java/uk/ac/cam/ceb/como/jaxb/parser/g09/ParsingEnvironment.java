package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.lang3.StringUtils;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Parameter;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;

/**
 * The Class ParsingEnvironment.
 */
public class ParsingEnvironment {

/** The br. */
private static BufferedReader br;

/**
 * Gets the last line in G 09 file.
 *
 * @param file the file
 * @return the last line in G09 file
 * @throws IOException Signals that an I/O exception has occurred.
 */
public static String getLastLineInG09File(File file) throws IOException {
	
	String lastLine="";
			
	br = new BufferedReader(new FileReader(file));
	
	for (String line; (line = br.readLine()) != null;) {
		
		lastLine = line;
		
	}
	
	return lastLine;
}

/**
 * Gets the program version.
 *
 * @param file the file
 * @return the program version
 * @throws IOException Signals that an I/O exception has occurred.
 */
public static Parameter getProgramVersion(File file) throws IOException {
	
	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();
	
	String programVersion="";
	
	String lastLine = getLastLineInG09File(file);
	
	programVersion = StringUtils.substringBetween(lastLine,"Gaussian ", " at");
	
	scalar.setValue(programVersion);
	scalar.setDataType("xsd:string");
	
	parameter.setDictRef("cc:programVersion");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;

}

/**
 * Gets the program name.
 *
 * @param file the file
 * @return the program name
 */
public static Parameter getProgramName(File file) {
	
	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();
	
	scalar.setDataType("xsd:string");
	scalar.setValue("Gaussian");
	
	parameter.setDictRef("cc:program");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;
	
}

/**
 * Gets the run date stamp.
 *
 * @param file the file
 * @return the run date stamp
 * @throws IOException Signals that an I/O exception has occurred.
 */
public static Parameter getRunDateStamp(File file) throws IOException {

	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();

	
	String dateStamp ="";
	
	String lastLine = getLastLineInG09File(file);
	
	dateStamp = StringUtils.substringAfter(lastLine," at ");
	
	scalar.setDataType("xsd:string");
	scalar.setValue(dateStamp);
	
	parameter.setDictRef("cc:runDate");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;

}

}