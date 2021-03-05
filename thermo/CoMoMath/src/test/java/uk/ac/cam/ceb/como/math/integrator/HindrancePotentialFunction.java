/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.integrator;

import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */
public class HindrancePotentialFunction extends Function<Double, Double> {

    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        return Math.exp(13000.0 * (1 - Math.cos(3.0 * x.doubleValue())) / (2.0 * 1600000 * 8.3144621));
    }
}
