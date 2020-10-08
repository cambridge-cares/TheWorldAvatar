package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import org.cts.CRSFactory;
import org.cts.crs.CRSException;
import org.cts.crs.CoordinateReferenceSystem;

/**
 * This class contains unit tests for CRSTransformer 
 * 
 * @author CSL
 *
 */

public class CRSTransformerTest {

	CRSTransformer crsTransformer;
	
	@Before
	public void init() {
		
		crsTransformer = new CRSTransformer();
	}
	
	/**
	 * Tests the private method getFactory. Asserts a CRSFactory object is returned.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testGetFactory() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
	
		// access private member
		assertNotNull(crsTransformer.getClass().getDeclaredMethod("getFactory"));
	    Method getFactory = crsTransformer.getClass().getDeclaredMethod("getFactory");
	    getFactory.setAccessible(true);
		
	    CRSFactory crsFactory = null;
	    try {
	    	crsFactory = (CRSFactory) getFactory.invoke(crsTransformer);
	    } finally {
	    	assertNotNull(crsFactory);
	    }
	}
	
	/**
	 * Tests the transform method with a coordinate transformation from spherical Mercator to WSG84.
	 */
	@Test
	public void testTransform() {
		  
		double[] result = CRSTransformer.transform(CRSTransformer.EPSG_3857, CRSTransformer.EPSG_4326, new double[] {11563323.926, 143305.896, 11560879.832, 140107.739});
		double[] expected = {103.87510617, 1.28723046, 103.85315050, 1.25850802};
		
		assertArrayEquals(expected, result, 0.00000001);
	}
	
	/**
	 * Check runtime exception thrown by transform when given a bad source CRS name. 
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testTransformExceptionSource() {
		
		CRSTransformer.transform("badSource", CRSTransformer.EPSG_4326, new double[] {11563323.926, 143305.896, 11560879.832, 140107.739});
	}
	
	/**
	 * Check runtime exception thrown by transform when given a bad target CRS name. 
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testTransformExceptionTarget() {
		
		CRSTransformer.transform(CRSTransformer.EPSG_3857, "badTarget", new double[] {11563323.926, 143305.896, 11560879.832, 140107.739});
	}
	
	/**
	 * 	Test transformInternal, a private method called by transform to perform coordinate transformation.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */ 
	@Test
	public void testTransformInternal() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(crsTransformer.getClass().getDeclaredMethod("transformInternal", String.class, String.class, double[].class));
	    Method transformInternal = crsTransformer.getClass().getDeclaredMethod("transformInternal", String.class, String.class, double[].class);
	    transformInternal.setAccessible(true);
		
	    double[] expected = {103.87510617, 1.28723046};
	    double[] sourcePoints = {11563323.926, 143305.896};
	    
	    double[] result = (double[]) transformInternal.invoke(crsTransformer, "EPSG:3857", "EPSG:4326", sourcePoints);
	    
	    assertArrayEquals(expected, result, 0.00000001);
	}
		
	/**
	 * Checks a CoordinateReferenceSystem object is returned by getCRS method.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	@Test
	public void testgetCRS() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		// access private member
		assertNotNull(crsTransformer.getClass().getDeclaredMethod("getCRS", String.class));
	    Method getCRS = crsTransformer.getClass().getDeclaredMethod("getCRS", String.class);
	    getCRS.setAccessible(true);
	    
	    CoordinateReferenceSystem crs = null;
	    try {
	    	crs = (CoordinateReferenceSystem) getCRS.invoke(crsTransformer, "EPSG:4326");
	    } finally {
	    	assertNotNull(crs);
	    }
	}
	
	/**
	 * Checks CRS exception is thrown by getCRS given a bad CRS name.
	 * 
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	@Test
	public void testgetCRSException() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException {
		
		// access private member
		assertNotNull(crsTransformer.getClass().getDeclaredMethod("getCRS", String.class));
	    Method getCRS = crsTransformer.getClass().getDeclaredMethod("getCRS", String.class);
	    getCRS.setAccessible(true);
	    
	    try {
	    	getCRS.invoke(crsTransformer, "badSource");
	    } catch (InvocationTargetException ex){
	    	assertEquals(CRSException.class, ex.getCause().getClass());
	    }
	}
	
	// getCoordinateOperations not tested separately
}
