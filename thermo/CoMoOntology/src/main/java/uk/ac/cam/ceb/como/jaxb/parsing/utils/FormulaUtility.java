package uk.ac.cam.ceb.como.jaxb.parsing.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;

/**
 * The Class FormulaUtility.
 *
 * @author nk510
 *         <p>
 *         This class implements methods for calculating sum of all atoms in
 *         molecule, as well as extract formula name by parsing g09 files.
 *         Implemented methods use EmpiricalFormulaParser class.
 *         </p>
 */
public class FormulaUtility {

	/** The sum atom. */
	int sum_atom = 0;

	/**
	 * Extract formula name. Method reads a Guassian file and extracts formula
	 * name.
	 *
	 * @param file
	 *            the file to be parsed.
	 * @return the string
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public String extractFormulaName(File file) throws IOException {
		
		String formulaName = null;

		/**
		 * @author nk510
		 *         <p>
		 *         Reads Gaussian file, line by line. When detects a line that contains
		 *         keyword 'stoichiometry' then returns formula name appearing in that
		 *         line.
		 * 
		 *         Try-block works under JavaSE 1.7
		 *         </p>
		 * 
		 */

		BufferedReader br = new BufferedReader(new FileReader(file));
		
		for (String line; (line = br.readLine()) != null;) {

			/**
			 * @author nk510
			 *         <p>
			 *         Process the line and tries to find 'Stoichiometry' term.
			 *         </p>
			 */
			if (line.contains("Stoichiometry")) {

				/**
				 * @author nk510
				 *         <p>
				 *         Returns string that contains formula name after "Stoichiometry"
				 *         string and before character '('.
				 *         </p>
				 */

				line = line.substring(line.lastIndexOf(" ") + 1);

				formulaName = line;

				/**
				 * @author nk510 <p>Removes all appearing substrings which have brackets, number,
				 *         including characters such as "-" and "," between brackets . For
				 *         example, formula name: Ti3H2S4(2-,3)->Ti3H2S4.</p>
				 */
				
				formulaName = formulaName.replaceAll("\\(([0-9])+(\\-\\,)([0-9])+\\)", "");
				
				/**
				 * @author nk510 <p>Removes all appearing substrings which have brackets and number
				 *         between brackets. For example, formula name: Ti4Cl2O3(2) -> Ti4Cl2O3</p>
				 */
				
				formulaName = formulaName.replaceAll("\\(([0-9])+\\)", "");

				System.out.println("formula name:  " + formulaName);

				break;
			}
		}

		br.close();

		return formulaName;

	}

	/**
	 * Gets the sum of all atom numbers.
	 *
	 * @author nk510
	 * @param formula
	 *            the formula
	 * @return
	 *         <p>
	 *         Calculates the sum of all atom numbers in one formula.This number
	 *         should be equal to the number of atoms listed in geometry field.
	 *         </p>
	 */
	public int getSumOfAllAtomNumbers(String formula) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

		return empParser.getNumberOfAllAtoms(formula);
	}
}