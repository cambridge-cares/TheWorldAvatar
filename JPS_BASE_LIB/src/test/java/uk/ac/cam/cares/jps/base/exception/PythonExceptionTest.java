package uk.ac.cam.cares.jps.base.exception;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.exception.PythonException;

class PythonExceptionTest {
	
	//set arbitrary inputs
	String message = "test message";

	@Test
	public void testPythonException() {
		PythonException test = new PythonException(message);
		JPSRuntimeException expected = new JPSRuntimeException(message);
		assertEquals(expected.getMessage(),test.getMessage());
	}

}
