/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.io.file.name.interpreter;

import org.junit.Test;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoExtended;

/**
 *
 * @author pb556
 */
public class ExtendedHRFileNameInterpreterTest {
    
    @Test
    public void fileNameInterpretationTest() throws Exception {
        String ref = "W:\\Data\\TTIP\\g09_hr\\i-rigid-1dhr_test\\sp_a-discr-freq-hr-fine-m-refined-species-0890-radical-0-restricted_[CH3]_1_10.g09";
        HPFileNameInfoExtended interpreter = new HPFileNameInfoExtended();
        interpreter.interpret(ref);
        assert(interpreter.get(HPFileNameInfoExtended.PROP_CALC_TYPE).compareTo("sp") == 0);
        assert(interpreter.getBase().compareTo("a-discr-freq-hr-fine-m-refined-species-0890-radical-0-restricted") == 0);
        assert(interpreter.get(HPFileNameInfoExtended.PROP_ROTOR).compareTo("[CH3]") == 0);
        assert(interpreter.get(HPFileNameInfoExtended.PROP_INDEX).compareTo("1") == 0);
        assert(interpreter.get(HPFileNameInfoExtended.PROP_TORS_ANGLE).compareTo("10") == 0);
    }
}
