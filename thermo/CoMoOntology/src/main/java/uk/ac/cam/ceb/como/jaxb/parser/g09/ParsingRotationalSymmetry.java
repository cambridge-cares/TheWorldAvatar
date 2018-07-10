/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.File;
import java.util.List;

import org.xmlcml.cml.base.CMLElement;

import org.xmlcml.cml.element.CMLProperty;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Scalar;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;

// TODO: Auto-generated Javadoc
/**
 * The Class ParsingRotationalSymmetry.
 *
 * @author nk510
 * <p>This class implements methods for extracting 'Rotational
 *         Symmetry' value from g09 file. It uses method parse() from
 *         GaussianParser class.</p>
 */
public class ParsingRotationalSymmetry {

	/**
	 * Generate rotational symmetry from G 09.
	 *
	 * @param file the file
	 * @return the property
	 * @throws Exception the exception
	 */
	public Property generateRotationalSymmetryFromG09(File file) throws Exception {

		/**
		 * @author nk510 
		 * <p>property_jaxb is object variable of Property jxb class.
		 *         Property class is generated from CompChem XML schema.</p>
		 */

		Property property_jaxb = new Property();

		FrequencyParser parser = new FrequencyParser();

		parser.set(file);
		parser.parse();

		CompChem cc = (CompChem) parser.get();

		CompChemWrapper ccw = new CompChemWrapper(cc);

		List<CMLProperty> prop = ccw.getProperties();

		/**
		 * @author nk510 
		 * <p>Iterates of List<CMLProperty> members in order extract
		 *         'Rotational Symmetry' value.</p>
		 */
		
		for (CMLProperty cmlp : prop) {

			List<CMLElement> child_elem = cmlp.getChildCMLElements();

			if (cmlp.getDictRef().equals("cc:rotational_symmetry")) {

				for (CMLElement c_elem : child_elem) {

					if (c_elem.getLocalName().equals("scalar")) {

						Scalar scalar_jxb = new Scalar();

						org.xmlcml.cml.element.CMLScalar scalar = (org.xmlcml.cml.element.CMLScalar) c_elem;

						System.out.println(scalar.getDataType() + ", " + scalar.getUnits() + ", " + scalar.getValue());

						scalar_jxb.setDataType(scalar.getDataType());
						scalar_jxb.setUnits(scalar.getUnits());
						scalar_jxb.setValue(scalar.getValue());

						property_jaxb.setDictRef(cmlp.getDictRef());

						property_jaxb.getScalarOrArrayOrMatrix().add(scalar_jxb);

					}
				}
			}
		}

		return property_jaxb;

	}
}