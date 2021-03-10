/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.examples;

/**
 *
 * @author pb556
 */
public class ExampleFourierSeriesOddStepFunction extends ExampleFourierSeries {

    public ExampleFourierSeriesOddStepFunction() {
        super();
        lower = -Math.PI;
        upper = Math.PI;
        T = upper - lower;
    }
    
    @Override
    public double f(double x) {
        x = checkValue(x);
        if (x > -Math.PI && x < 0) {
            return 0.0;
        }
        return 1.0;
    }

    @Override
    public double a(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        if (index == 0) {
            return 1.0;
        }
        return 0.0;
    }

    @Override
    public double b(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        double dIndex = (double) index;
        return (-(Math.pow(-1, index)) + 1) / (Math.PI * dIndex);
    }
    
}
