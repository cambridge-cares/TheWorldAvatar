package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class ParsingAtomicMass {

	public static StringTokenizer getAtomicWeightString(File file) throws FileNotFoundException, IOException {

		String atomicMassLine = "";

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

				atomicMassLine = atomicMassLine + line;
			}
		}

		br.close();

		StringTokenizer stringTokenizer = new StringTokenizer(atomicMassLine);

		return stringTokenizer;

	}
	
	/**
	 * 
	 * @author nk510
	 * @param weightTokenizer
	 * @return list of strings that represent atomic masses of each atom appearing in
	 *         geometry optimisation module.
	 *         
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