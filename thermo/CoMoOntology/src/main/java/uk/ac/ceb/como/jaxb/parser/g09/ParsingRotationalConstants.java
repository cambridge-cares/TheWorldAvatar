package uk.ac.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;

import org.apache.commons.lang.StringUtils;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Array;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;

/**
 * 
 * @author nk510 This class implements methods for generating CompChem XML file
 *         by parsing 'Relational Constants' values from g09 files.
 *
 */
public class ParsingRotationalConstants {

	public Property generateRotationalConstantsFromG09(File file) throws Exception {

		/**
		 * 
		 * @author nk510 The following three lines extract 'Relational Constants' values
		 *         and units from Gaussian files (for example Cl2O6.g09).
		 * 
		 */

		String rc_string = getRotationalConstantsString(file);
		String rc_value = getRotationalContantsValue(rc_string);
		String rc_units = getRotationalContantsUnits(rc_string);

		/**
		 * @author nk510 'rc_property' is object variable of Property jxb class.
		 *         Property class is generated from CompChem XML schema.
		 */
		Property rc_property = new Property();

		rc_property.setDictRef("cc:rotational_constants");

		Array array_jxb = new Array();

		int repl_rc_value = getRotationalConstantsSize(rc_value);

		array_jxb.setSize(BigInteger.valueOf(repl_rc_value));

		array_jxb.setValue(rc_value);
		array_jxb.setUnits(rc_units);

		rc_property.getScalarOrArrayOrMatrix().add(array_jxb);

		return rc_property;

	}

	/**
	 * @author nk510 Code in the method below is used to extract last string that
	 *         starts with 'Rotational constants' in G09 files.
	 */

	public static String getRotationalConstantsString(File file) throws FileNotFoundException, IOException {

		String rc_line = "";

		try (BufferedReader br = new BufferedReader(new FileReader(file))) {

			for (String line; (line = br.readLine()) != null;) {

				if (line.contains("Rotational constants")) {

					rc_line = line;

					/**
					 * @author nk510 Returns string that starts with "Rotational constants" string.
					 *         If there are more than one string it returns, last one.
					 */

					line = line.substring(line.lastIndexOf(" ") + 1);

				}
			}

			return rc_line;
		}
	}

	/**
	 * @author nk510
	 * @param line
	 * @return This method extracts numbers from given string that starts with
	 *         'Rotational constants' as a String object variable. These numbers
	 *         represent 'Rotational constants' values.
	 * 
	 */

	public static String getRotationalContantsValue(String line) {

		line = line.substring(line.lastIndexOf(":") + 1);

		System.out.println("Rotational constants value: " + line);

		return line.trim().replaceAll(" +", " ");
	}

	/**
	 * @author nk510
	 * @param rc
	 * @return Method below returns 'Rotational constants' size. It splits string
	 *         that contains rotational constants values (strings) into array of
	 *         strings. In counting number of these values (size), it skips
	 *         empty/bank strings.
	 */
	public int getRotationalConstantsSize(String rc) {

		int size = 0;

		/**
		 * @author nk510 Skips empty strings.
		 */
		String[] nums = rc.split(" ");

		for (String n : nums) {

			if ((!n.isEmpty()) || (n != " "))
				size++;
		}

		return size;
	}

	/**
	 * @author nk510
	 * @param line
	 * @return The method below returns unit name for rotational constants. The
	 *         string is given between brackets in resulted 'rotational constants'
	 *         string.
	 */

	public String getRotationalContantsUnits(String line) {

		String units = StringUtils.substringBetween(line, "(", ")");

		units = "nonSi:" + units;

		return units;

	}
}