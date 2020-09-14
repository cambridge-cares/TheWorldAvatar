package uk.ac.cam.cares.jps.base.util.test;

import static org.junit.Assert.*;

import org.junit.Test;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class CRSTransformerTest {

	@Test
	public void testTransformHK() {
		  
		// coordinates copied from JPS_SHIP TestCoordinatorCollection testgetCurrentBkgConcentration()
		// HK grid to WGS84
		double[] expected = {114.03058, 22.19656};
		double[] result = CRSTransformer.transform("EPSG:2326", "EPSG:4326", new double[] {821182.43, 806448.76});
		
		assertArrayEquals(expected, result, 0.00001);
	}

	@Test
	public void testTransformSphMercator() {
		  
		// coordinates copied from JPS_DISPERSION WeatherTestAgent testextractcontext()
		// Spherical Mercator to WGS84
		double[] result = CRSTransformer.transform(CRSTransformer.EPSG_3857, CRSTransformer.EPSG_4326, new double[] {11563323.926, 143305.896, 11560879.832, 140107.739});
		double[] expected = {103.875, 1.287, 103.853, 1.259};
		
		assertArrayEquals(expected, result, 0.001);
	}
}
