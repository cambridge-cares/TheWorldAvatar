package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * 
 * @author nk510 The Class ParsingCharge.
 * 
 *         <p>The class has implemented method for extracting formal charge from
 *         Gaussian file. We use this method for parsing Gaussian files (g09)
 *         which contain molecules with one atom.</p>
 *         
 */

public class ParsingCharge {

	/** The buffered reader */
	private static BufferedReader br;

	/**
	 * Gets the formal charge.
	 *
	 * @param file
	 *            the file is a Gaussian file (g09)
	 * @return the formal charge
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	
	public static Long getFormalCharge(File file) throws IOException {

		String formalCharge = "";

		try {

			br = new BufferedReader(new FileReader(file));

		} catch (FileNotFoundException e) {

			e.printStackTrace();

		}

		try {

			for (String line; (line = br.readLine()) != null;) {

				/**
				 * @author nk510
				 *         <p>
				 * 		If line contains substring 'Charge =' then we extract substring
				 *         between this string and character 'M' that is starting letter of word
				 *         'Multiplicity'. If there is more than one line containing these
				 *         information, we take last one.
				 *         </p>
				 */
				
				if (line.contains("Charge =")) {

					formalCharge = line.substring(line.indexOf("=") + 1, line.lastIndexOf("M")).replace(" ", "");
				}
			}

		} catch (Exception e) {

			e.printStackTrace();
		}

		br.close();

		Long formarChargeInteger = Long.valueOf(formalCharge);
		
		return formarChargeInteger;
	}
}