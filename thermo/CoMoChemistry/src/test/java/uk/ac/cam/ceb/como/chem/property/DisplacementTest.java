package uk.ac.cam.ceb.como.chem.property;



import uk.ac.cam.ceb.como.chem.property.Displacement;

/**
 *
 * @author pb556
 */

public class DisplacementTest {

    private final Displacement dpm;

    
    public DisplacementTest() {
        dpm = new Displacement();
        dpm.setdxInA(-1.23456);
        dpm.setdyInA(1.92833);
        dpm.setdzInA(-3.13426);
    }
}