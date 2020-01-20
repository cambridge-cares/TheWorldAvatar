/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.variability;

import uk.ac.cam.ceb.como.math.average.Mean;
import uk.ac.cam.ceb.como.math.average.Median;

/**
 *
 * @author pb556
 */
public class MadianAbsoluteDeviation extends Variability {

    @Override
    public double calculate(int[] values) throws Exception {
        Mean mean = new Mean();
        Median median = new Median();
        double m = mean.calculate(values);
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = Math.abs(values[i] - m);
        }
        return median.calculate(val);
    }

    @Override
    public double calculate(long[] values) throws Exception {
        Mean mean = new Mean();
        Median median = new Median();
        double m = mean.calculate(values);
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = Math.abs(values[i] - m);
        }
        return median.calculate(val);
    }

    @Override
    public double calculate(float[] values) throws Exception {
        Mean mean = new Mean();
        Median median = new Median();
        double m = mean.calculate(values);
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = Math.abs(values[i] - m);
        }
        return median.calculate(val);
    }

    @Override
    public double calculate(double[] values) throws Exception {
        Mean mean = new Mean();
        Median median = new Median();
        double m = mean.calculate(values);
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = Math.abs(values[i] - m);
        }
        return median.calculate(val);
    }
}
