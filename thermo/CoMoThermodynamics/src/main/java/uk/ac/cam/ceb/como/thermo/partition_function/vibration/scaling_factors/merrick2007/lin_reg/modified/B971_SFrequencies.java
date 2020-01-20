/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.vibration.scaling_factors.merrick2007.lin_reg.modified;

import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import uk.ac.cam.ceb.como.math.function.simple.LinearFunction;

/**
 *
 * @author pb556
 */
public class B971_SFrequencies extends LinearFunction {
    
    public B971_SFrequencies() {
        super(1.0236530214343011, -3.579650951518815E-5);
    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        double v = super.f(x, additionalData);
        if (v < 0.9684) {
            return 0.9684;
        }
        return v;
    }
}
