/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;

import org.apache.commons.lang.StringUtils;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Array;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;

// TODO: Auto-generated Javadoc
/**
 * The Class ParsingRotationalConstants.
 *
 * @author nk510
 * <p>This class implements methods for generating CompChem XML file
 *         by parsing 'Relational Constants' values by parsing from g09 files.</p>
 */

public class ParsingRotationalConstants {

	/**
	 * Generate rotational constants from G 09.
	 *
	 * @param file the file
	 * @return the property
	 * @throws Exception the exception
	 */
	public Property generateRotationalConstantsFromG09(File file) throws Exception {

		/**
		 * 
		 * @author nk510 
		 * <p>The following three lines extract 'Relational Constants' values
		 *         and units from Gaussian files (for example Cl2O6.g09).</p>
		 * 
		 */

		String rc_string = getRotationalConstantsString(file);
		String rc_value = getRotationalContantsValue(rc_string);
		String rc_units = getRotationalContantsUnits(rc_string);

		/**
		 * @author nk510 
		 * <p>'rc_property' is object variable of Property jxb class.
		 *         Property class is generated from CompChem XML schema.</p>
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
	 * Gets the rotational constants string.
	 *
	 * @author nk510
	 * <p>Code in the method below is used to extract last string that
	 *         starts with 'Rotational constants' in G09 files.</p>
	 * @param file the file
	 * @return the rotational constants string
	 * @throws FileNotFoundException the file not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */

	public static String getRotationalConstantsString(File file) throws FileNotFoundException, IOException {

		String rc_line = "";

		/**
		 * @author nk510
		 * <p>try block works under JavaSE 1.7</p>
		 */
		
//	 	try (BufferedReader br = new BufferedReader(new FileReader(file))) {
	 		BufferedReader br = new BufferedReader(new FileReader(file));
			for (String line; (line = br.readLine()) != null;) {

				if (line.contains("Rotational constants")) {

					rc_line = line;

					/**
					 * @author nk510 
					 * <p>Returns string that starts with "Rotational constants" string.
					 *         If there are more than one string it returns, last one.</p>
					 */

					line = line.substring(line.lastIndexOf(" ") + 1);

				}
			}
			br.close();
			return rc_line;
			
//		}
	 	
	}

	/**
	 * Gets the rotational contants value.
	 *
	 * @author nk510
	 * @param line the line
	 * @return <p>This method extracts numbers from given string that starts with
	 *         'Rotational constants' as a String object variable. These numbers
	 *         represent 'Rotational constants' values.</p>
	 */

	public static String getRotationalContantsValue(String line) {

		line = line.substring(line.lastIndexOf(":") + 1);

		System.out.println("Rotational constants value: " + line);

		return line.trim().replaceAll(" +", " ");
	}

	/**
	 * Gets the rotational constants size.
	 *
	 * @author nk510
	 * @param rc the rc
	 * @return <p>Method below returns 'Rotational constants' size. It splits string
	 *         that contains rotational constants values (strings) into array of
	 *         strings. In counting number of these values (size), it skips
	 *         empty/bank strings.</p>
	 */
	public int getRotationalConstantsSize(String rc) {

		int size = 0;

		/**
		 * @author nk510 
		 * <p>Skips empty strings.</p>
		 */
		String[] nums = rc.split(" ");

		for (String n : nums) {

			if ((!n.isEmpty()) || (n != " "))
				size++;
		}

		return size;
	}

	/**
	 * Gets the rotational contants units.
	 *
	 * @author nk510
	 * @param line the line
	 * @return <p>The method below returns unit name for rotational constants. The
	 *         string is given between brackets in resulted 'rotational constants'
	 *         string.</p>
	 */

	public String getRotationalContantsUnits(String line) {

		String units = StringUtils.substringBetween(line, "(", ")");

		units = "nonSi:" + units;

		return units;

	}
}