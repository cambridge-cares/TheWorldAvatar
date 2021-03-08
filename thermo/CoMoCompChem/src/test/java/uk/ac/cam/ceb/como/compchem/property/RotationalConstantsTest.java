package uk.ac.cam.ceb.como.compchem.property;

import org.apache.log4j.Logger;
import org.junit.Test;
import static org.junit.Assert.*;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;

/**
 *
 * @author pb556
 */
public class RotationalConstantsTest {
    private Logger logger = Logger.getLogger(getClass());

    /**
     * Test of setValuesInGHz method, of class RotationalConstants.
     */
    @Test
    public void testSetValuesInGHz() {
        logger.info("========== setValuesInGHz");
        double r0InGHz = 34.35900;
        double r1InGHz = 9.12224;
        double r2InGHz = 8.06190;

        double f = PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr;
        RotationalConstants rotc = RotationalConstants.fromPMOI(new double[] {52.52600 * f, 197.83976 * f, 223.86056 * f});
        logger.info(rotc.toString().trim());
        assertEquals(r0InGHz, rotc.getValueInGHz(0), 0.001);
        assertEquals(r1InGHz, rotc.getValueInGHz(1), 0.00001);
        assertEquals(r2InGHz, rotc.getValueInGHz(2), 0.00001);
    }

}