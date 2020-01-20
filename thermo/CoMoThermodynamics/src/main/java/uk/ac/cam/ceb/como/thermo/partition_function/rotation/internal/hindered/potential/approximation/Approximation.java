/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */
public abstract class Approximation<T> {
    
    // Function!!!
    public abstract Function approximate() throws ApproximationException;
    
    public abstract DataPoint approximate(T[] coordinate) throws ApproximationException, FunctionCalculationException;
}
