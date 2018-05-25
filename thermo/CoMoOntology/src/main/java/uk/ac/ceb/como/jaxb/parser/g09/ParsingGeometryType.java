package uk.ac.ceb.como.jaxb.parser.g09;

import java.io.File;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;

import uk.ac.ceb.como.jaxb.parsing.utils.FormulaProperty;

/**
 * 
 * @author nk510 This class implements methods for extracting 'Geometry type' by
 *         parsing g09 files and using FrequencyParser. The algorithm
 *         implemented here is given by Dr Daniel Nurkowski
 *         (danieln@cmclinnovations.com).
 */
public class ParsingGeometryType {

	/**
	 * @author nk510
	 * @param atoms_sum
	 * @param formula
	 * @return Algorithm implemented in this method determines 'Geometry type' for
	 *         each formula, based on the sum of all atom numbers and based on
	 *         'Rotational constants' values. If the sum of all atom numbers is
	 *         equal one then 'Geometry type' is 'atomic'. If sum of all atom
	 *         numbers is equal to two then 'Geometry type' is 'linear'. If sum of
	 *         all atom numbers if greater than two, then geometry type is
	 *         calculating based on 'Rotational constants'. If size of 'Rotational
	 *         constants' is three and all of these three numbers are different than
	 *         zero then 'Geometry type' is 'nonlinear', otherwise 'Geometry type'
	 *         is linear.
	 * 
	 *         Comment given by Dr Daniel Nurkowski (danieln@cmclinnovations.com)
	 *         during discussion: There are cases when we have one of 'Rotational
	 *         constants' to be close to zero. It may violate the calculation of
	 *         geometry type.
	 * 
	 * @throws Exception
	 */

	public Property getGeometryTypeFromG09(File file) throws Exception {

		ParsingRotationalConstants prc = new ParsingRotationalConstants();

		Property pg = new Property();
		pg.setDictRef("cc:geometry_type");

		/**
		 * @author nk510 'Geometry type' data is type of plain text. For plain text, a
		 *         scalar should be used [1]. Scalar JAXB object variable is used to
		 *         remember 'Geometry type' value. Scalar is child element of Property
		 *         element [1].
		 * 
		 *         [1] Weerapong Phadungsukanan, Markus Kraft, Joe Townsend, and Peter
		 *         Murray-Rust, The semantics of Chemical Markup Language (CML) for
		 *         computational chemistry: CompChem, Journal of Cheminformatics 4, 15,
		 *         (2012)
		 */
		Scalar sg = new Scalar();

		sg.setDataType("xsd:string");

		FormulaProperty fp = new FormulaProperty();

		/**
		 * @author nk510 Extracts formula name from G09 file.
		 */
		String f_name = fp.extractFormulaName(file);

		/**
		 * @author nk510 Calculates the sum of all atoms (number of atoms) given in
		 *         formula name.
		 */
		int sum_atoms = fp.getSumOfAllAtomNumbers(f_name);

		/**
		 * @author nk510 If sum of all atoms in molecule in less than or equal to one,
		 *         then Scalar's value is set to 'atomic'.
		 */
		if (sum_atoms <= 1 && sum_atoms > 0) {

			sg.setValue("atomic");
			pg.getScalarOrArrayOrMatrix().add(sg);

		} else {

			/**
			 * @author nk510 If sum of all atoms in molecule is equal to two then Scalar's
			 *         value is set to 'linear'.
			 */

			if (sum_atoms == 2) {

				sg.setValue("linear");
				pg.getScalarOrArrayOrMatrix().add(sg);

			}

			/**
			 * @author nk510 In case of having sum of all atoms in molecule greater than
			 *         two, we parse g09 file in order to collect all 'Rotational Constants'
			 *         values by using FrequencyParser.
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

				String rc_string = rc.toString();
				String rc_value = ParsingRotationalConstants.getRotationalContantsValue(rc_string);

				int rc_size_value = prc.getRotationalConstantsSize(rc_value);

				System.out.println("The size of RC: " + rc_size_value);
				/**
				 * @author nk510 In size of 'Rotational Constants' is equal three then 'Geometry
				 *         type' could be 'nonlinear' or 'linear'. If all three 'Rotational
				 *         Constants' values are different than zero then 'Geometry type' is
				 *         'nonlinear'. Otherwise is 'linear'.
				 */
				if (rc_size_value == 3) {

					if (rc.getValue(0) != 0 && rc.getValue(1) != 0 && rc.getValue(2) != 0) {

						sg.setValue("nonlinear");
						pg.getScalarOrArrayOrMatrix().add(sg);

					} else {

						sg.setValue("linear");
						pg.getScalarOrArrayOrMatrix().add(sg);
					}

				}
				;

			}

		}

		return pg;
	}
}