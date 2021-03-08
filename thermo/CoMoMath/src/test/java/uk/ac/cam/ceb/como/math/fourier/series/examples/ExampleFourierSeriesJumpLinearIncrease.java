/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.examples;

/**
 *
 * @author pb556
 */
public class ExampleFourierSeriesJumpLinearIncrease extends ExampleFourierSeries {

    public ExampleFourierSeriesJumpLinearIncrease() {
        super();
        lower = -1.0;
        upper = 1.0;
        T = upper - lower;
    }
    
    @Override
    public double f(double x) {
        x = checkValue(x);
        if (x >= -1 && x < 0) {
            return 1.0;
        }
        if (x == 0.5) {
            return 0.0;
        }
        return x;
    }

    @Override
    public double a(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        if (index == 0) {
            return 3.0 / 2.0;
        }
        double dIndex = (double) index;
        return (Math.pow(-1, index) - 1.0) / (dIndex * dIndex * Math.PI * Math.PI);
    }

    @Override
    public double b(int index) throws ExampleFourierSeriesOutOfValueRangeException {
        if (index < 0) {
            throw new ExampleFourierSeriesOutOfValueRangeException("Index cannot be negative.");
        }
        double dIndex = (double) index;
        return (-1) / (dIndex * Math.PI);
    }
}
