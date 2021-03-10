/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.variability;

import uk.ac.cam.ceb.como.math.minmax.MinMax;

/**
 *
 * @author pb556
 */
public class Range extends Variability {

    @Override
    public double calculate(int[] values) throws Exception {
        int max = MinMax.getMaximum(values);
        int min = MinMax.getMinimum(values);
        return (max - min);
    }

    @Override
    public double calculate(long[] values) throws Exception {
        long max = MinMax.getMaximum(values);
        long min = MinMax.getMinimum(values);
        return (max - min);
    }

    @Override
    public double calculate(float[] values) throws Exception {
        float max = MinMax.getMaximum(values);
        float min = MinMax.getMinimum(values);
        return (max - min);
    }

    @Override
    public double calculate(double[] values) throws Exception {
        double max = MinMax.getMaximum(values);
        double min = MinMax.getMinimum(values);
        return (max - min);
    }
    
}
