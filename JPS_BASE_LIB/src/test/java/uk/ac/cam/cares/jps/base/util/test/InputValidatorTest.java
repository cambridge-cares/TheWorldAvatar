package uk.ac.cam.cares.jps.base.util.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.InputValidator;

public class InputValidatorTest extends TestCase{
	private String iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	private String filePath = "C:\\JPS_DATA";
	/** tests if the IRI is valid
	 * 
	 */
	public void testInputValidatorIRI() {

		assertFalse(InputValidator.checkIfValidIRI( filePath ));
		assertTrue(InputValidator.checkIfValidIRI( iriofnetwork));
	}
	public void testInputValidatorFile() {
		assertFalse(InputValidator.checkIfValidFile( iriofnetwork));
		assertTrue(InputValidator.checkIfValidFile( filePath ));
	}
		
}
