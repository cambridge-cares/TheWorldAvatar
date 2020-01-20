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

/** The br denotes an instantiation of <a href="java.io.BufferedReader">BufferedReader.</a> */
private static BufferedReader br;

/**
 * Gets the last line in G 09 file.
 *
 * @param file <p>The file is Gaussian file.</p>
 * @return String <p>The last line in Gaussina file.</p>
 * @throws IOException <p>Signals that an I/O exception has occurred.</p>
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
 * @param file <p> It is a Gaussian file.</p>
 * @return the program version <p>It is version of program used for calculations.</p>
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
 * @param file <p> It is a Gaussian file.</p>.
 * @return the program name <p> It is a name of program used for thermo calculations.</p>
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
 * @param file <p> It is Gaussian input file name</p>.
 * @return the run date stamp <p> It denotes data of running Gaussian calculation.</p>
 * @throws IOException Signals <p> It is an I/O exception has occurred.</p>
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