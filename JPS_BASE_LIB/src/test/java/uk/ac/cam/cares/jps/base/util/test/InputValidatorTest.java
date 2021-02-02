package uk.ac.cam.cares.jps.base.util.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.util.InputValidator;

public class InputValidatorTest extends TestCase{

	public void testInputValidator() {
		String iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
		
		assertTrue(InputValidator.checkIfValidIRI( iriofnetwork));
	}
		
}
