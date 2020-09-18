package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import org.junit.Test;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class CRSTransformerTest {

	@Test
	public void testTransformHK() {
		  
		// HK grid to WGS84
		// Pass CRS name as string
		double[] expected = {114.03058, 22.19656};
		double[] result = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {821182.43, 806448.76});
		
		assertArrayEquals(expected, result, 0.00001);
	}

	@Test
	public void testTransformSphMercator() {
		  
		// Spherical Mercator to WGS84
		// Pass CRS name as variable
		double[] result = CRSTransformer.transform(CRSTransformer.EPSG_3857, CRSTransformer.EPSG_4326, new double[] {11563323.926, 143305.896, 11560879.832, 140107.739});
		double[] expected = {103.87510617, 1.28723046, 103.85315050, 1.25850802};
		
		assertArrayEquals(expected, result, 0.00000001);
	}
}
