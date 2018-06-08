package uk.ac.ceb.como.jaxb.parser.g09;

import java.io.File;
import java.math.BigInteger;
import java.util.List;

import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLProperty;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Array;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Property;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;

/**
 * 
 * @author nk510 This class implements method for generating CompChem XML file
 *         by parsing 'Frequencies' data from g09 files. Parser class
 *         'FrequencyParser' of {@author pb556} is used to extract 'Frequencies'
 *         data (information).
 * 
 */

public class ParsingFrequencies {

	public Property generateFrequenciesFromG09(File file) throws Exception {

		/**
		 * 
		 * @author nk510 'fr_property' is object variable of a Property jxb class. The
		 *         Property class is generated from CompChem XML schema (see the
		 *         implementation of Property class in package
		 *         {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Property Property}).
		 * 
		 */

		Property fr_property = new Property();

		/**
		 * @author nk510 Here we use {@author pb556} 'FrequencyParser' to parser g09
		 *         file and to access to {@link org.xmlcml.cml.element.CMLProperty
		 *         CMLProperty} object variable such as DictRef ('cc:frequencies').
		 */
		FrequencyParser parser = new FrequencyParser();

		parser.set(file);
		parser.parse();
		
		CompChem cc = (CompChem) parser.get();

		CompChemWrapper ccw = new CompChemWrapper(cc);

		List<CMLProperty> prop = ccw.getProperties();

		for (CMLProperty cmlp : prop) {

			List<CMLElement> child_elem = cmlp.getChildCMLElements();

			for (CMLElement c_elem : child_elem) {

				List<CMLElement> array_list = c_elem.getChildCMLElements();

				for (CMLElement c_elem_2 : array_list) {

					CMLArray array = (CMLArray) c_elem_2;

					if (array.getDictRef().equals("cc:frequencies")) {

						/**
						 * @author nk510 Creating object variable of Array JAXB class that contains
						 *         information about frequencies.
						 */
						Array arrayXML = new Array();
						

						String arrayXMLUnits = array.getUnits();


						/**
						 * @author nk510 CompChem XML schema does not allow units to have character '^'.
						 *         Line below removes that character in a string that remembers the
						 *         units for 'Frequencies'.
						 * 
						 */
						arrayXMLUnits = arrayXMLUnits.replace("^", "");
						
						System.out.println(array.getSize());

						/**
						 * @author nk510 An Array JAXB object variable contains DictRef, Units, Value
						 *         and DataType taken from CMLArray's object variable 'array' .
						 */
						arrayXML.setDictRef(array.getDictRef());
						arrayXML.setUnits(arrayXMLUnits);
						arrayXML.setValue(array.getValue());
						/**
						 * @author nk510
						 * Adds the number of frequeny data (size).
						 */
						arrayXML.setSize(BigInteger.valueOf(array.getSize()));
						
						arrayXML.setDataType(array.getDataType());

						/**
						 * @author nk510 JAXB Property class sets DictRef value by using CMLProperty's
						 *         DictRef.
						 */
						fr_property.setDictRef(cmlp.getDictRef());

						/**
						 * @author nk510 Based on CompChem XML Schema, any property may contain one or
						 *         more arrays. Line below adds arrays (arrayXML object variable) to
						 *         Property JAXB object variable 'fr_property'.
						 */

						fr_property.getScalarOrArrayOrMatrix().add(arrayXML);

					}
				}
			}
		}

		return fr_property;
	}

}