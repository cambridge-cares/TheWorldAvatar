package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.lang3.StringUtils;

/**
 * 
 * @author nk510 <p>This class implements method for extracting spin multiplicity
 *         for digital entities (Gaussian files - g09) which have molecule with
 *         one atom.</p>
 *
 */

public class ParsingSpinMultiplicity {

	private static BufferedReader br;

	public static Long getSpinMultiplicity(File file) throws IOException {

		br = new BufferedReader(new FileReader(file));

		String spinMultiplicity = "";

		for (String line; (line = br.readLine()) != null;) {

			/**
			 * @author nk510
			 * <p> If line contains substring 'Charge =' then we extract substring that starts after last appearing character '=' in that line.  
			 * If there is more than one line containing these information, we take last one. </p>
			 */

			
			if (line.contains("Charge =")) {

				spinMultiplicity = StringUtils.substringAfterLast(line, "=").replaceAll(" ", "");
			}
		}
		
		Long spinMultiplicityLong = Long.valueOf(spinMultiplicity);

		return spinMultiplicityLong;

	}
}