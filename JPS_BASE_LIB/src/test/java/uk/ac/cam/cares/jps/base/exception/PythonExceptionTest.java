package uk.ac.cam.cares.jps.base.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

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
