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
 * The Class ParsingBasisSet.
 * 
 */
public class ParsingBasisSet {

	/**
	 * Gets the basis set string.
	 *
	 * @param file
	 *            <p>The Gaussian file</p> 
	 * @return the basis set <p>It is an instance of String extracted from the file (G09)</p>
	 * @throws IOException
	 *             Signals that an I/O exception has occurred
	 */
	public static String getBasisSetString(File file) throws IOException {

		String basisSetString = "";

		/**
		 * 
		 * @author nk510
		 *         <p>
		 *         try block works under JavaSE 1.7
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
				 *         Returns substring that starts with "/" string and ends with first
				 *         appearing space (" ") character. Split string that starts with '#p '
				 *         on two substrings before and after character '/'.
				 *         </p>
				 * 
				 */

				String splitOne[] = line.split("/");
				line = splitOne[1];

				/**
				 * Split second substring into two strings before white space and after white
				 * space (" ").
				 */

				String splitTwo[] = line.split(" ");

				basisSetString = basisSetString + splitTwo[0];

				System.out.println(basisSetString);

				break;
			}
		}

		br.close();

		return basisSetString;

	}

	/**
	 * Gets the basis set parameter.
	 *
	 * @param file
	 *            the file is a Gaussina file.
	 * @param numberOfAtoms
	 *            number of atoms in one species extracted by parsing the file (G09).
	 * @return Jaxb Parameter instance that contains information about basis set
	 *         value.
	 * @throws Exception
	 *             the exception
	 */
	public static Parameter getBasisSetParameter(File file, int numberOfAtoms) throws Exception {

		Parameter parameter = new Parameter();
		Scalar scalar = new Scalar();

		if (numberOfAtoms > 1) {

			FrequencyParser parser = new FrequencyParser();
			parser.set(file);
			parser.parse();

			CompChem cc = (CompChem) parser.get();

			CompChemParser ccp = new CompChemParser();

			ccp.parse(cc);

			IRCompChemWrapper irccw = new IRCompChemWrapper(cc);

			List<CMLParameter> cmlP = irccw.getParameters();

			for (CMLParameter p : cmlP) {

				/**
				 *
				 * Finds basis set when attribute dictRef has value that is equal to 'cc:basis'
				 *
				 */
				if (p.getDictRef().equals("cc:basis")) {

					parameter.setDictRef(p.getDictRef());
					scalar.setValue(p.getValue());
					scalar.setDataType("xsd:string");

					parameter.getScalarOrArrayOrMatrix().add(scalar);
				}
			}

		} else {

			parameter.setDictRef("cc:basis");
			scalar.setValue(getBasisSetString(file));
			scalar.setDataType("xsd:string");

			parameter.getScalarOrArrayOrMatrix().add(scalar);
		}

		return parameter;
	}

}