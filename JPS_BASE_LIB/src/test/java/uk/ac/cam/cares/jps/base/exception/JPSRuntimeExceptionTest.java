package uk.ac.cam.cares.jps.base.exception;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class JPSRuntimeExceptionTest {
	
	// Create arbitrary Throwable cause (ArithmeticException: / by zero)
		public Throwable createRuntimeExceptionCause(int a, int b) {
			try {
				int c = a / b;
			} catch (ArithmeticException e) {
				return e;
			}
			return null;
		}
		
	// Set arbitrary inputs
	String message = "test message";
	Throwable cause = createRuntimeExceptionCause(1,0);

	@Test
	public void testJPSRuntimeException_Message() {
		JPSRuntimeException testMessage = new JPSRuntimeException(message);
		RuntimeException expectedMessage = new RuntimeException(message);
		assertEquals(expectedMessage.getMessage(),testMessage.getMessage());
	}

	@Test
	public void testJPSRuntimeException_Cause() {
		JPSRuntimeException testCause = new JPSRuntimeException(cause);
		RuntimeException expectedCause = new RuntimeException(cause);
		assertEquals(expectedCause.getCause(), testCause.getCause());
	}
	
	@Test
	public void testJPSRuntimeException_MessageandCause(){
		JPSRuntimeException testMessageandCause = new JPSRuntimeException(message, cause);
		RuntimeException expectedMessageandCause = new RuntimeException(message, cause);
		assertEquals (expectedMessageandCause.getMessage(),testMessageandCause.getMessage());
		assertEquals(expectedMessageandCause.getCause(),testMessageandCause.getCause());
	}
	
}
