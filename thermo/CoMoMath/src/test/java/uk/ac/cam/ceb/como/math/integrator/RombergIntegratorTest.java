/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.integrator;

import uk.ac.cam.ceb.como.math.integrator.RombergIntegrator;
import org.apache.commons.math.ConvergenceException;
import org.apache.commons.math.FunctionEvaluationException;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class RombergIntegratorTest {
    
    @Test
    public void test() throws FunctionCalculationException, ConvergenceException, FunctionEvaluationException {
          RombergIntegrator integrator = new RombergIntegrator();
          integrator.setLimit(0, 2.0*Math.PI/3.0);
          integrator.setFunction(new HindrancePotentialFunction());
          System.out.println(integrator.getIntegral());
    }
}
