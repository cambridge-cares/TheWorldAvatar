/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.variability;

/**
 *
 * @author pb556
 */
public class StandardDeviation extends Variability {

    @Override
    public double calculate(int[] values) throws Exception {
        Variance v = new Variance();
        return Math.sqrt(v.calculate(values));
    }

    @Override
    public double calculate(long[] values) throws Exception {
        Variance v = new Variance();
        return Math.sqrt(v.calculate(values));
    }

    @Override
    public double calculate(float[] values) throws Exception {
        Variance v = new Variance();
        return Math.sqrt(v.calculate(values));
    }

    @Override
    public double calculate(double[] values) throws Exception {
        Variance v = new Variance();
        return Math.sqrt(v.calculate(values));
    }
    
}
