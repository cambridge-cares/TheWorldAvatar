package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import java.util.List;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLProperty;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;

public class GenerateElectronicEnergy {

	public static void main(String[] args) throws Exception {

		File file = new File("src/test/resources/g09/TiCl4.g09");
		
		FrequencyParser parser = new FrequencyParser();
		parser.set(file);		
		parser.parse();
		CompChem cc = (CompChem) parser.get();
		IRCompChemWrapper irccw = new IRCompChemWrapper(cc);
		
		System.out.println("Parameters: ");
		
		List<CMLParameter> parameter = irccw.getParameters();
		
		for(CMLParameter cml_p : parameter) {
			
		System.out.println(cml_p.toXML());
		
		}
		
		System.out.println("Property: ");
		
		List<CMLProperty> cml_property = irccw.getProperties();
		
		for(CMLProperty p : cml_property) {
			
			System.out.println(p.toXML());
		}
		
		
		System.out.println("- - - - - - - -  - - - - - - -  -");
		
 		BufferedReader totalEneergyBr = new BufferedReader(new FileReader(file));
 		
		String totalElectronicEnergy = "";
 		
		for (String line; (line = totalEneergyBr.readLine()) != null;) {

			if (line.contains("Zero-point correction=")){
		
				line = line.substring(line.indexOf("=") + 1);
				line =line.substring(0, line.indexOf("("));
				line = line.replaceAll(" ", "");
				
				
				totalElectronicEnergy = line;
			}
		}
		
		System.out.println("Zero-poing correction: " + totalElectronicEnergy);
		
		totalEneergyBr.close();
		
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
		
		System.out.println("SCF Energy: " + scfElectronicEnergy);
		
		scfEneergyBr.close();
		
	}
}