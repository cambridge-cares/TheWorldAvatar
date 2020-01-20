/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.average;

/**
 *
 * @author pb556
 */
public class Mean extends Average {

    @Override
    public double calculate(int[] values) throws Exception {
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);
    }

    @Override
    public double calculate(long[] values) throws Exception {
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);
    }

    @Override
    public double calculate(float[] values) throws Exception {
        double[] val = new double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);
    }

    @Override
    public double calculate(double[] values) throws Exception {
        double mean = 0;
        for (int i = 0; i < values.length; i++) {
            mean += values[i];
        }
        return mean / (double) values.length;
    }
    
}
