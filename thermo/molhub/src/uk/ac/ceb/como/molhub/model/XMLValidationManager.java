package uk.ac.ceb.como.molhub.model;

import java.io.File;
import java.io.IOException;
import javax.xml.XMLConstants;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.xml.sax.SAXException;


public class XMLValidationManager {

	/**
	 * @author nk510 
	 * @param xsdPath a path to Compchem xml schema.
	 * @param xmlPath a path to Compchem xml file.
	 * @return true if generated xml file is valid, and false if generated xml file is not valid.
	 */
	public static boolean validateXMLSchema(String xsdPath, File xmlFile) {
		try {
			SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
			Schema schema = factory.newSchema(new File(xsdPath));
			Validator validator = schema.newValidator();
			validator.validate(new StreamSource(xmlFile));
		}
		catch (IOException | SAXException e) {
			System.out.println("Validation Exception: " + e.getMessage());
			return false;
		}
		return true;
}
}
