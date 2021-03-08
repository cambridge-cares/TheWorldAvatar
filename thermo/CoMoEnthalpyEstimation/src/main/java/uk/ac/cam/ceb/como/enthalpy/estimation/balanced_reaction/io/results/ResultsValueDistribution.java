/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import java.util.ArrayList;
import uk.ac.cam.ceb.como.math.average.Mean;
import uk.ac.cam.ceb.como.math.average.Median;

/**
 *
 * @author pb556
 */
public class ResultsValueDistribution extends ArrayList<Double>{
    
    public double getMean() throws Exception {
        return new Mean().calculate(toDoubleArray());
    }
    
    public double getMedian() throws Exception{
        return new Median().calculate(toDoubleArray());
    }
    
    protected double[] toDoubleArray() {
        double[] arr = new double[size()];
        for (int i = 0; i < size(); i++) {
            arr[i] = get(i);
        }
        return arr;
    }
    
    @Override
    public String toString() {
        String str = "[";
        for (int i = 0; i < size() - 1; i++) {
            str += get(i) + ", ";
        }
        str += get(size() - 1) + "]";
        return str;
    }
}
