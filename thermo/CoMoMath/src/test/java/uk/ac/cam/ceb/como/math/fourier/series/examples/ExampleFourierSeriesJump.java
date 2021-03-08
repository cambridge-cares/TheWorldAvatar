/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.examples;

/**
 *
 * @author pb556
 */
public class ExampleFourierSeriesJump extends ExampleFourierSeries {

    public ExampleFourierSeriesJump() {
        super();
        lower = -3.0;
        upper = 3.0;
        T = upper - lower;
    }
    
    @Override
    public double f(double x) {
        x = checkValue(x);
        if (x >= -3 && x < 0) {
            return -1.0;
        }
        return 1.0;
    }

    @Override
    public double a(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        return 0.0;
    }

    @Override
    public double b(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        double dIndex = (double) index;
        return (2.0 - 2.0*(Math.pow(-1, index))) / (dIndex * Math.PI);
    }
}
