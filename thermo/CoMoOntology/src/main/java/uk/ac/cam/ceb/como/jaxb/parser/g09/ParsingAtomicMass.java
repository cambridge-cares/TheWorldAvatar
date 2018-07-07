package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.xmlcml.cml.element.CMLMolecule;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

public class ParsingAtomicMass {

	public static StringTokenizer getAtomicWeightString(File file) throws FileNotFoundException, IOException {

		String rc_line = "";

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

			if (line.contains("AtmWgt=")) {
				/**
				 * 
				 * @author nk510
				 *         <p>
				 *         Returns substring that starts after "AtmWgt=" string. It adds
				 *         substring into StringTokenizer.
				 *         </p>
				 * 
				 */

				line = line.substring(line.lastIndexOf("= ") + 1);

				rc_line = rc_line + line;
			}
		}

		br.close();

		StringTokenizer st = new StringTokenizer(rc_line);

		return st;

	}

	
	/**
	 * @param f
	 * @return a number of atoms in geometry optimization module.
	 * @throws Exception
	 */
	public static int getNumberOfAtomsInGeometryOptimization(File f) throws Exception {

		FrequencyParser parser = new FrequencyParser();

		parser.set(f);
		parser.parse();

		CompChem cc = (CompChem) parser.get();

		IRCompChemWrapper irccw = new IRCompChemWrapper(cc);

		CMLMolecule m = irccw.getFinalMolecule();

		return m.getAtomCount();

	}

	/**
	 * @author nk510
	 * @param weightTokenizer
	 * @return list of strings that represent atomic masses of each atom appearing in
	 *         geometry optimization module.
	 */

	public static List<String> getListOfAtomicMass(StringTokenizer weightTokenizer) {

		List<String> weightList = new ArrayList<String>();

		String atomWeight = "";

		while (weightTokenizer.hasMoreTokens()) {

			atomWeight = weightTokenizer.nextToken();

			weightList.add(atomWeight);

		}

		return weightList;

	}
}