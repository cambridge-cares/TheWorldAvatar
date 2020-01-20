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

/**
 * The Class ParsingRotationalSymmetry.
 *
 * @author nk510
 * <p>This class implements methods for extracting 'Rotational
 *         Symmetry' value from Gaussian file. It uses method {@link uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser#parse()} from
 *         {@link uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser} class.</p>
 */
public class ParsingRotationalSymmetry {

	/**
	 * Generate rotational symmetry from Gaussian file.
	 *
	 * @param file the file
	 * @return the property <p>It is instance of class {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Property} </p>
	 * @throws Exception the exception
	 */
	public Property generateRotationalSymmetryFromG09(File file) throws Exception {

		/**
		 * @author nk510 
		 * <p>propertyJaxb is object variable of {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Property} class.
		 *         Property class is generated from CompChem XML schema.</p>
		 */

		Property propertyJaxb = new Property();

		FrequencyParser parser = new FrequencyParser();

		parser.set(file);
		parser.parse();

		CompChem cc = (CompChem) parser.get();

		CompChemWrapper ccw = new CompChemWrapper(cc);

		List<CMLProperty> prop = ccw.getProperties();

		/**
		 * @author nk510 
		 * <p>Iterates of a List of {@link org.xmlcml.cml.element.CMLProperty} members in order extract
		 *         'Rotational Symmetry' value.</p>
		 */
		
		for (CMLProperty cmlp : prop) {

			List<CMLElement> child_elem = cmlp.getChildCMLElements();

			if (cmlp.getDictRef().equals("cc:rotational_symmetry")) {

				for (CMLElement cElem : child_elem) {

					if (cElem.getLocalName().equals("scalar")) {

						Scalar scalarJxb = new Scalar();

						org.xmlcml.cml.element.CMLScalar scalar = (org.xmlcml.cml.element.CMLScalar) cElem;

						System.out.println(scalar.getDataType() + ", " + scalar.getUnits() + ", " + scalar.getValue());

						scalarJxb.setDataType(scalar.getDataType());
						scalarJxb.setUnits(scalar.getUnits());
						scalarJxb.setValue(scalar.getValue());

						propertyJaxb.setDictRef(cmlp.getDictRef());

						propertyJaxb.getScalarOrArrayOrMatrix().add(scalarJxb);

					}
				}
			}
		}

		return propertyJaxb;

	}
}