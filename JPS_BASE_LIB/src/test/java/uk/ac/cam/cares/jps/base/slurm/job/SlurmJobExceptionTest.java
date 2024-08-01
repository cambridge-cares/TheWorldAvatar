package uk.ac.cam.cares.jps.base.slurm.job;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.junit.jupiter.api.Test;

public class SlurmJobExceptionTest {

	// Create arbitrary Throwable cause (ArithmeticException: / by zero)
	public Throwable createExceptionCause(int a, int b) {
		try {
			int c = a / b;
		} catch (ArithmeticException e) {
			return e;
		}
		return null;
	}

	// Set arbitrary inputs
	String message = "test message";
	Throwable cause = createExceptionCause(1, 0);
	boolean enableSuppression = false;
	boolean writableStackTrace = false;
	
	@Test
	// Test SlurmJobException() should give same results as Exception()
	public void testSlurmJobException_None() {
		SlurmJobException testNone = new SlurmJobException();
		Exception expectedNone = new Exception();
		assertEquals(expectedNone.getMessage(), testNone.getMessage());
	}
	
	@Test
	// Test SlurmJobException(message) should give same results as Exception(message)
	public void testSlurmJobException_Message() {
		SlurmJobException testMessage = new SlurmJobException(message);
		Exception expectedMessage = new Exception(message);
		assertEquals(expectedMessage.getMessage(), testMessage.getMessage());
	}
	
	@Test
	// Test SlurmJobException(message, cause) should give same results as Exception(message, cause)
	public void testSlurmJobException_MessageandCause() {
		SlurmJobException testMessageandCause = new SlurmJobException(message, cause);
		Exception expectedMessageandCause = new Exception(message, cause);
		assertEquals(expectedMessageandCause.getMessage(), testMessageandCause.getMessage());
		assertEquals(expectedMessageandCause.getCause(), testMessageandCause.getCause());
	}
	
	@Test
	// Test SlurmJobException(cause) should give same results as Exception(cause)
	public void testSlurmJobException_Cause() {
		SlurmJobException testCause = new SlurmJobException(cause);
		Exception expectedCause = new Exception(cause);
		assertEquals(expectedCause.getCause(), testCause.getCause());
	}
	
	@Test
	// Test SlurmJobException(message, cause, enableSuppression, writableStackTrace)
	public void testAll() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {
		Constructor<SlurmJobException> constrct = SlurmJobException.class.getDeclaredConstructor(String.class, Throwable.class, boolean.class, boolean.class);
		constrct.setAccessible(true);
		SlurmJobException testAll = constrct.newInstance(message, cause, enableSuppression, writableStackTrace);
		
		assertEquals("test message", testAll.getMessage());
		assertEquals(cause, testAll.getCause());

		// enableSuppression and writableStackTrace are false, should return zero length array
		assertEquals(0, testAll.getSuppressed().length);
		assertEquals(0, testAll.getStackTrace().length);
	}

}