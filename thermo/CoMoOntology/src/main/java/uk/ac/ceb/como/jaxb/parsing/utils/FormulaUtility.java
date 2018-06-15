package uk.ac.ceb.como.jaxb.parsing.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;

/**
 * 
 * @author nk510 This class implements methods for calculating sum of all atoms
 *         in molecule, as well as extract formula name by parsing g09 files.
 *         Implemented methods use EmpiricalFormulaParser class.
 *
 */
public class FormulaProperty {

	int sum_atom = 0;

	public String extractFormulaName(File file) throws IOException {

		String formulaName = null;

		/**
		 * @author nk510 Reads g09 file, line by line. When detects a line that contains
		 *         keyword "Stoichiometry" then returns formula name appearing in that
		 *         line.
		 * 
		 */

		try (BufferedReader br = new BufferedReader(new FileReader(file))) {

			for (String line; (line = br.readLine()) != null;) {

				/**
				 * @author nk510 Process the line and tries to find 'Stoichiometry' term.
				 */
				if (line.contains("Stoichiometry")) {

					/**
					 * @author nk510 Returns string that contains formula name after "Stoichiometry"
					 *         string and before character '('.
					 */

					line = line.substring(line.lastIndexOf(" ") + 1);

					formulaName = line;

					break;

				}
			}
		}

		return formulaName;

	}

	/**
	 * @author nk510
	 * @param formula
	 * @return Calculates the sum of all atom numbers in one formula.This number
	 *         should be equal to the number of atoms listed in geometry.
	 */
	public int getSumOfAllAtomNumbers(String formula) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

		return empParser.getNumberOfAllAtoms(formula);
	}

}