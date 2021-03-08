/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.average;

import com.cmclinnovations.data.sort.numeric.BubbleSort;

/**
 *
 * @author pb556
 */
public class Median extends Average {
    
    @Override
    public double calculate(int[] values) throws Exception {
        Double[] val = new Double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);  
    }

    @Override
    public double calculate(long[] values) throws Exception {
        Double[] val = new Double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);  
    }

    @Override
    public double calculate(float[] values) throws Exception {
        Double[] val = new Double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);  
    }

    @Override
    public double calculate(double[] values) throws Exception {
        Double[] val = new Double[values.length];
        for (int i = 0; i < values.length; i++) {
            val[i] = (double) values[i];
        } 
        return this.calculate(val);        
    }
    
    private double calculate(Double[] values) {
        BubbleSort<Double> sorter = new BubbleSort<Double>();
        sorter.sort(values);
        if (values.length == 1) {
            return values[0];
        }
        if (values.length % 2 == 0) {
            return (values[(int) (values.length / 2)] + values[(int) (values.length / 2) - 1]) / 2.0;
        }
        return values[(int) (values.length / 2)];
    }
    
}
