package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

import org.xmlcml.cml.element.CMLParameter;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Parameter;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
/**
 * 
 * @author nk510
 * This class implements method for parsing level of theory in Gaussian file. 
 */
public class ParsingLevelOfTheory {	
	
	/**
	 * 
	 * @param file <p>It denotes Gaussian file.</p>
	 * @return A String <p>It contains information about level of theory for given Gaussian file. The string starts with "#p" and ends with first appearing "/" character.</p>
	 * @throws IOException input output exception.
	 */
	public static String getLevelOfTheoryString(File file) throws IOException {
		
		String levelOfTheory= "";	
		
		/**
		 * 
		 * @author nk510
		 *         <p>
		 *         Try-catch-block works under JavaSE 1.7.
		 *         </p>
		 * 
		 */

		BufferedReader br = new BufferedReader(new FileReader(file));

		for (String line; (line = br.readLine()) != null;) {

			if (line.contains("#p ")) {
				/**
				 * 
				 * @author nk510
				 *         <p>
				 *         Returns substring that starts with "#p " string and ends with first appearing "/" character. 
				 *         </p>
				 * 
				 */

			
				line=  line.substring(line.indexOf("#p ")+3, line.indexOf("/"));

				levelOfTheory = levelOfTheory + line;
				
				System.out.println(levelOfTheory);
				
				break;
			}
		}

		br.close();		
	
	return levelOfTheory;
	
	}
	/**
	 * 
	 * @param file <p> It represents a Gaussian file. </p>
	 * @param numberOfAtoms <p>It represents number of atoms.</p>
	 * @return Jaxb Parametar <p> It contains information about level of theory. Parameter contains one scalar.</p>
	 * @throws Exception the exception. 
	 */
	public static Parameter getLevelOfTheryParameter(File file, int numberOfAtoms) throws Exception {
		
		
		Parameter parameter = new Parameter();
		
		Scalar scalar = new Scalar();
		
		if(numberOfAtoms>1) {
		
			FrequencyParser parser = new FrequencyParser();
			 parser.set(file);
			 parser.parse();
			 
			 CompChem cc = (CompChem) parser.get();

			 CompChemParser ccp = new CompChemParser();
			 
			 ccp.parse(cc);
			 
			 IRCompChemWrapper irccw = new IRCompChemWrapper(cc);
			 
			 List<CMLParameter> cmlP = irccw.getParameters();

			 for(CMLParameter p: cmlP) {
			 	
			    	/**
					 * @author nk510
					 * <p> Finds level of theory when attribute dictRef has value that is equal to 'cc:method'. </p>
					 *
					 */
				 if(p.getDictRef().equals("cc:method")) {
					 
					 	parameter.setDictRef("cc:method");
					 	scalar.setValue(p.getValue());
					 	scalar.setDataType("xsd:string");
					 	
					 	parameter.getScalarOrArrayOrMatrix().add(scalar);
				 }
			}
		}else {
			
		 	parameter.setDictRef("cc:method");
		 	scalar.setValue(getLevelOfTheoryString(file));
		 	scalar.setDataType("xsd:string");
		 	
		 	parameter.getScalarOrArrayOrMatrix().add(scalar);
			
		}
		
		return parameter;
		
	}
	
}