package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.lang3.StringUtils;

/**
 * 
 * @author nk510 This class implements method for extracting spin multiplicity
 *         for digital entities (Gaussian files - g09) which have molecule with
 *         one atom.
 */
public class ParsingSpinMultiplicity {

	private static BufferedReader br;

	public static Long getSpinMultiplicity(File file) throws IOException {

		br = new BufferedReader(new FileReader(file));

		String spinMultiplicity = "";

		for (String line; (line = br.readLine()) != null;) {

			if (line.contains("Charge =")) {

				spinMultiplicity = StringUtils.substringAfterLast(line, "=").replaceAll(" ", "");
				
				
			}

		}
		
		Long spinMultiplicityLong = Long.valueOf(spinMultiplicity);

		return spinMultiplicityLong;

	}

}
