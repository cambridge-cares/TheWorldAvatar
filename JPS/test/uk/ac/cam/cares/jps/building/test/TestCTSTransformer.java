package uk.ac.cam.cares.jps.building.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SparqlConstants;

public class TestCTSTransformer extends TestCase implements SparqlConstants {
	
	 /**
     * Check if the result point is equal in X and Y to the target point using
     * a given tolerance
     *
     * @param c1 expected coordinates
     * @param c2 actual coordinates
     * @param tolerance
     */
    protected boolean checkEquals2D(double[] c1, double[] c2, double tolerance) {
        double dx = Math.abs(c1[0] - c2[0]);
        double dy = Math.abs(c1[1] - c2[1]);
        double delta = Math.max(dx, dy);
        return (delta <= tolerance);
    }
    
	public void testCTSTransformationFromEPSG4326ToEPSG27582() {

        String sourceCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] sourcePoint = new double[]{2.114551393, 50.345609791};
        String targetCRS = "EPSG:27582"; //Target EPSG lambert 2 etendu france
        double[] targetPoint = new double[]{584173.736, 2594514.828};
        double tolerance = 0.001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
        
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationFromEPSG27582ToEPSG4326() {    
       
		String sourceCRS = "EPSG:27582"; //Target EPSG lambert 2 etendu france
		double[] sourcePoint = new double[]{584173.736, 2594514.828};
		String targetCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] targetPoint = new double[]{2.114551393, 50.345609791};
        double tolerance = 0.0001;
         
        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
  
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationFromEPSG4326ToEPSG28992() {

        String sourceCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] sourcePoint = new double[]{2.114551393, 50.345609791};
        String targetCRS = CRSTransformer.EPSG_28992; // Amersfoort / RD New (The Hague)
        double[] targetPoint = new double[]{-77933.49015247656, 266871.66338106303};
        double tolerance = 0.001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
       
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationFromEPSG28992ToEPSG4326() {

        String sourceCRS = CRSTransformer.EPSG_28992; // Amersfoort / RD New (The Hague)
        double[] sourcePoint = new double[]{-77933.49015247656, 266871.66338106303};
        String targetCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] targetPoint = new double[]{2.114551393, 50.345609791};
        double tolerance = 0.0001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
        
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationFromEPSG4326ToEPS25833() {

        String sourceCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] sourcePoint = new double[]{2.114551393, 50.345609791};
        String targetCRS = CRSTransformer.EPSG_25833; // ETRS89 / UTM zone 33N (Berlin)
        double[] targetPoint = new double[]{-415337.4593325241, 5656915.37819996};
        double tolerance = 0.001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
       
        System.out.println(result[0] + ", " + result[1]);
        
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationFromEPS25833ToEPSG4326() {

        String sourceCRS = CRSTransformer.EPSG_25833; // ETRS89 / UTM zone 33N (Berlin)
        double[] sourcePoint = new double[]{-415337.4593325241, 5656915.37819996};
        String targetCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] targetPoint = new double[]{2.114551393, 50.345609791};
        double tolerance = 0.0001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
         
        assertTrue(checkEquals2D(result, targetPoint, tolerance));
	}
	
	public void testCTSTransformationForBerlinPlant1() {
		
		String sourceCRS = CRSTransformer.EPSG_25833; // Berlin
        double[] sourcePoint = new double[]{392825, 5819122};
        String targetCRS = CRSTransformer.EPSG_28992; // The Hague
        double[] targetPoint = new double[] {699583.49, 532938.39};
        double tolerance = 1;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
          
        assertTrue(checkEquals2D(result, targetPoint, tolerance));  
	}
	
	public void testCTSTransformationForBerlinRegion() {
		
		String sourceCRS = CRSTransformer.EPSG_28992; // The Hague
        double[] sourcePoint = new double[]{699983, 533338};
        String targetCRS = CRSTransformer.EPSG_4326; // WGS 84
        double[] targetPoint = new double[] {13.420712, 52.511667};
        double tolerance = 0.0001;

        double[] result = CRSTransformer.transform(sourceCRS, targetCRS, sourcePoint);
          
        System.out.println(result[0] + ", "  + result[1]);
        
        //assertTrue(checkEquals2D(result, targetPoint, tolerance));  
	}
}
