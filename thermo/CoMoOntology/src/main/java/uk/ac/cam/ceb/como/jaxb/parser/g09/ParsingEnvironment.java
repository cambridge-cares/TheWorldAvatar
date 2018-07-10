package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.lang3.StringUtils;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Parameter;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;


public class ParsingEnvironment {

private static BufferedReader br;

public static void main(String[] args) throws IOException {
	
	File f = new File("src/test/resources/g09/Cl.g09");
	

}

public static String getLastLineInG09File(File f) throws IOException {
	
	String lastLine="";
			
	br = new BufferedReader(new FileReader(f));
	
	for (String line; (line = br.readLine()) != null;) {
		
		lastLine = line;
		
	}
	
	return lastLine;
}

public static Parameter getProgramVersion(File f) throws IOException {
	
	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();
	
	String programVersion="";
	
	String lastLine = getLastLineInG09File(f);
	
	programVersion = StringUtils.substringBetween(lastLine,"Gaussian ", " at");
	
	scalar.setValue(programVersion);
	scalar.setDataType("xsd:string");
	
	parameter.setDictRef("cc:programVersion");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;

}

public static Parameter getProgramName(File f) {
	
	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();
	
	scalar.setDataType("xsd:string");
	scalar.setValue("Gaussian");
	
	parameter.setDictRef("cc:program");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;
	
}

public static Parameter getRunDateStamp(File f) throws IOException {

	Parameter parameter = new Parameter();
	Scalar scalar = new Scalar();

	
	String dateStamp ="";
	
	String lastLine = getLastLineInG09File(f);
	
	dateStamp = StringUtils.substringAfter(lastLine," at ");
	
	scalar.setDataType("xsd:string");
	scalar.setValue(dateStamp);
	
	parameter.setDictRef("cc:runDate");
	parameter.getScalarOrArrayOrMatrix().add(scalar);
	
	return parameter;

}

}