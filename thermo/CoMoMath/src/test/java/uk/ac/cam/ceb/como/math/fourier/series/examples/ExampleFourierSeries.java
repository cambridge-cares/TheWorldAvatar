/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.examples;

/**
 *
 * @author pb556
 */
public abstract class ExampleFourierSeries {
    
    protected double T;
    protected double lower;
    protected double upper;
    
    public double getLower() {
        return lower;
    }
    
    public double getUpper() {
        return upper;
    }
    
    public double getPeriodLength() {
        return T;
    }
    
    protected double checkValue(double x) {
        if (x % T != 0) {
            x = x - ((int)(x / T)) * T;
        }
        return x;
    }
    
    public abstract double f(double x);
    public abstract double a(int index) throws ExampleFourierSeriesOutOfValueRangeException;
    public abstract double b(int index) throws ExampleFourierSeriesOutOfValueRangeException;
    
    public double approximation(double x, int order) throws ExampleFourierSeriesOutOfValueRangeException {
        double[] a = new double[order];
        double[] b = new double[order];
        for (int i = 0; i < order; i++) {
            a[i] = a(i);
            b[i] = b(i);
        }
        return approximation(x, a, b);
    }
    
    public double approximation(double x, double[] a, double[] b) throws ArrayIndexOutOfBoundsException, IndexOutOfBoundsException {
        if (a.length != b.length) {
            throw new ArrayIndexOutOfBoundsException("Array lenghts are not the same.");
        }
        if (a.length == 0) {
            throw new IndexOutOfBoundsException("No coefficients have been defined.");
        }
        
        double fx = a[0] / 2.0;
        for (int i = 1; i < a.length; i++) {
            double iDouble = (double) i;
            fx += a[i] * Math.cos(iDouble * x) + b[i] * Math.sin(iDouble * x);
        }
        return fx;
    }
}
