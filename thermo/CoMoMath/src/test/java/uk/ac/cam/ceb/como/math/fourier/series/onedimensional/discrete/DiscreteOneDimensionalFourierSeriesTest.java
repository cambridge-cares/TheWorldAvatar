/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete;

import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete.DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficientFittingException;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimit;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesParameters;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesAbsX;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesJump;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesJumpLinearIncrease;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesOddStepFunction;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesOutOfValueRangeException;
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeriesXSquare;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class DiscreteOneDimensionalFourierSeriesTest {
    
    @Test
    public void discr1DFittingTest_AbsX() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        test(new ExampleFourierSeriesAbsX(), 0.1);
    }
    
    @Test
    public void discr1DFittingTest_Jump() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        test(new ExampleFourierSeriesJump(), 1.001);
    }
    
    @Test
    public void discr1DFittingTest_JumpLinearIncrease() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        test(new ExampleFourierSeriesJumpLinearIncrease(), 0.7);
    }
    
    @Test
    public void discr1DFittingTest_OddStepFunction() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        test(new ExampleFourierSeriesOddStepFunction(), 0.6);
    }
    
    @Test
    public void discr1DFittingTest_XSquare() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        test(new ExampleFourierSeriesXSquare(), 0.1);
    }
    
    protected void test(ExampleFourierSeries series, double tol) throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
        // get the values
        int n = 100;
        double dx = series.getPeriodLength() / (double) n;
        DataSpace space = new DataSpace();
        for (int i = 0; i < n; i++) {
            space.add(new DataPoint<Double>(new Double[]{i * dx + series.getLower()}, series.f(i * dx + series.getLower())));
        }
        
        int order = 50;
        FourierSeriesLimit lowerLimit = new FourierSeriesLimit(1, series.getLower());
        FourierSeriesLimit upperLimit = new FourierSeriesLimit(1, series.getUpper());
        FourierSeriesLimits lower = new FourierSeriesLimits();
        FourierSeriesLimits upper = new FourierSeriesLimits();
        lower.add(lowerLimit);
        upper.add(upperLimit);
        
        DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting fitting = 
                new DiscreteClassicalOneDimensionalFourierSeriesCoefficientFitting(space, order, lower, upper);
        fitting.fitFourierSeriesCoefficients();
        FourierSeriesParameters parameters = fitting.getFourierSeriesParameters();
        FourierSeriesCoefficients coeff = parameters.getFourierSeriesCoefficients();
        
        // a1 should be large compared to all the other coefficients
        for (DataPoint p : space) {
            double realValue = (Double) p.getValue();
            double calcValue = series.approximation((Double) p.getCoordinate()[0], order);
            System.out.println(realValue);
            //assert (Math.abs(realValue - calcValue) < tol);
        }
        System.out.println();
        for (DataPoint p : space) {
            double realValue = (Double) p.getValue();
            double calcValue = series.approximation((Double) p.getCoordinate()[0], order);
            System.out.println(calcValue);
            //assert (Math.abs(realValue - calcValue) < tol);
        }
    }
}
