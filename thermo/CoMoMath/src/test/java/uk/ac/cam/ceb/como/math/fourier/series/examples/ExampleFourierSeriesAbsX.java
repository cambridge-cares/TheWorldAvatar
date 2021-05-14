/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.examples;

/**
 *
 * @author pb556
 */
public class ExampleFourierSeriesAbsX extends ExampleFourierSeries {

    public ExampleFourierSeriesAbsX() {
        super();
        lower = -Math.PI;
        upper = Math.PI;
        T = upper - lower;
    }
    
    @Override
    public double f(double x) {
        x = checkValue(x);
        return Math.abs(x);
    }

    @Override
    public double a(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        if (index == 0) {
            return Math.PI;
        }
        double dIndex = (double) index;
        return 2.0 * (Math.pow(-1, dIndex) - 1) / (Math.PI * Math.pow(dIndex, 2));
    }

    @Override
    public double b(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        return 0.0;
    }
    
}
