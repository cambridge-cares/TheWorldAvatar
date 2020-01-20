/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.variability;

import uk.ac.cam.ceb.como.math.average.Mean;

/**
 *
 * @author pb556
 */
public class Variance extends Variability {

    @Override
    public double calculate(int[] values) throws Exception {
        double res = 0.0;
        Mean mean = new Mean();
        double m = mean.calculate(values);
        for (int i = 0; i < values.length; i++) {
            res += Math.pow(values[i] - m, 2) / (values.length - 1);
        }
        return res;
    }

    @Override
    public double calculate(long[] values) throws Exception {
        double res = 0.0;
        Mean mean = new Mean();
        double m = mean.calculate(values);
        for (int i = 0; i < values.length; i++) {
            res += Math.pow(values[i] - m, 2) / (values.length - 1);
        }
        return res;
    }

    @Override
    public double calculate(float[] values) throws Exception {
        double res = 0.0;
        Mean mean = new Mean();
        double m = mean.calculate(values);
        for (int i = 0; i < values.length; i++) {
            res += Math.pow(values[i] - m, 2) / (values.length - 1);
        }
        return res;
    }

    @Override
    public double calculate(double[] values) throws Exception {
        double res = 0.0;
        Mean mean = new Mean();
        double m = mean.calculate(values);
        for (int i = 0; i < values.length; i++) {
            res += Math.pow(values[i] - m, 2) / (values.length - 1);
        }
        return res;
    }
    
}
