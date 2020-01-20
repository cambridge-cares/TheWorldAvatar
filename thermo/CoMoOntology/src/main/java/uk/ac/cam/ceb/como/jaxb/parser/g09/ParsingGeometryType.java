/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.File;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FormulaUtility;

/**
 * The Class ParsingGeometryType.
 *
 * @author nk510
 * <p>This class implements methods for extracting 'Geometry type' by
 *         parsing Gaussian files and using FrequencyParser. The algorithm
 *         implemented here is given by Dr Daniel Nurkowski
 *         (danieln@cmclinnovations.com).</p>
 */
public class ParsingGeometryType {

	/**
	 * Gets the geometry type from Gaussian file (g09).
	 *
	 * @author nk510
	 * @param file the file
	 * @return the geometry type from Gaussian file (g09)
	 * @throws Exception  
	 * <p>Algorithm implemented in this method determines 'Geometry type' for
	 *         each formula, based on the sum of all atom numbers and based on
	 *         'Rotational constants' values. If the sum of all atom numbers is
	 *         equal one then 'Geometry type' is 'atomic'. If sum of all atom
	 *         numbers is equal to two then 'Geometry type' is 'linear'. If sum of
	 *         all atom numbers if greater than two, then geometry type is
	 *         calculating based on 'Rotational constants'. If size of 'Rotational
	 *         constants' is three and all of these three numbers are different than
	 *         zero then 'Geometry type' is 'nonlinear', otherwise 'Geometry type'
	 *         is linear.</p>
	 * 
	 *         <p>Comment given by Dr Daniel Nurkowski (danieln@cmclinnovations.com)
	 *         during discussion: There are cases when we have one of 'Rotational
	 *         constants' to be close to zero. It may violate the calculation of
	 *         geometry type.</p>
	 */

	public Property getGeometryTypeFromG09(File file) throws Exception {

		ParsingRotationalConstants prc = new ParsingRotationalConstants();

		Property property = new Property();
		property.setDictRef("cc:geometry_type");

		/**
		 * @author nk510 
		 * <p>'Geometry type' data is type of plain text. For plain text, a
		 *         scalar should be used [1]. Scalar JAXB object variable is used to
		 *         remember 'Geometry type' value. Scalar is child element of Property
		 *         element [1].
		 * 
		 *         [1] Weerapong Phadungsukanan, Markus Kraft, Joe Townsend, and Peter
		 *         Murray-Rust, The semantics of Chemical Markup Language (CML) for
		 *         computational chemistry: CompChem, Journal of Cheminformatics 4, 15,
		 *         (2012)
		 *  </p>
		 */
		Scalar sg = new Scalar();

		sg.setDataType("xsd:string");

		FormulaUtility fUtility = new FormulaUtility();

		/**
		 * @author nk510 <p>Extracts formula name from G09 file.</p>
		 */
		String f_name = fUtility.extractFormulaName(file);

		/**
		 * @author nk510 
		 * <p>Calculates the sum of all atoms (number of atoms) given in
		 *         formula name.</p>
		 */
		int sum_atoms = fUtility.getSumOfAllAtomNumbers(f_name);

		/**
		 * @author nk510 
		 * <p>If sum of all atoms in molecule in less than or equal to one,
		 *         then Scalar's value is set to 'atomic'.</p>
		 */
		if (sum_atoms <= 1 && sum_atoms > 0) {

			sg.setValue("atomic");
			property.getScalarOrArrayOrMatrix().add(sg);

		} else {

			/**
			 * @author nk510 
			 * <p>If sum of all atoms in molecule is equal to two then Scalar's
			 *         value is set to 'linear'. </p>
			 */

			if (sum_atoms == 2) {

				sg.setValue("linear");
				property.getScalarOrArrayOrMatrix().add(sg);

			}

			/**
			 * @author nk510 
			 * <p>In case of having sum of all atoms in molecule greater than
			 *         two, we parse g09 file in order to collect all 'Rotational Constants'
			 *         values by using FrequencyParser. </p>
			 */
			if (sum_atoms > 2) {

				FrequencyParser parser = new FrequencyParser();

				parser.set(file);
				parser.parse();
				CompChem cc = (CompChem) parser.get();
				CompChemParser ccp = new CompChemParser();
				ccp.parse(cc);

				RotationalConstants rc = ccp.get().getRotationalConstants();

				System.out.println("Rotational Constants: [" + rc.getValue(0) + "] [" + rc.getValue(1) + "] ["
						+ rc.getValue(2) + "]");

				System.out.println("Rotational Constants string: " + rc.toString());

				String rotationalConstantsString = rc.toString();
				String rc_value = ParsingRotationalConstants.getRotationalContantsValue(rotationalConstantsString);

				int rotationalConstantsSizeValue = prc.getRotationalConstantsSize(rc_value);

				System.out.println("The size of RC: " + rotationalConstantsSizeValue);
				/**
				 * @author nk510 
				 * <p>If size of 'Rotational Constants' is equal three then 'Geometry
				 *         type' could be 'nonlinear' or 'linear'. If all three 'Rotational
				 *         Constants' values are different than zero then 'Geometry type' is
				 *         'nonlinear'. Otherwise is 'linear'.</p>
				 */
				if (rotationalConstantsSizeValue == 3) {

					if (rc.getValue(0) != 0 && rc.getValue(1) != 0 && rc.getValue(2) != 0) {

						sg.setValue("nonlinear");
						property.getScalarOrArrayOrMatrix().add(sg);

					} else {

						sg.setValue("linear");
						property.getScalarOrArrayOrMatrix().add(sg);
					}

				}
				;

			}

		}

		return property;
	}
}