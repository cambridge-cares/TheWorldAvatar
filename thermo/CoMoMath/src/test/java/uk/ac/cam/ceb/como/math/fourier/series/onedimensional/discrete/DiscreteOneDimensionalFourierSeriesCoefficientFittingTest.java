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
import uk.ac.cam.ceb.como.math.fourier.series.examples.ExampleFourierSeries1PlusX;
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
public class DiscreteOneDimensionalFourierSeriesCoefficientFittingTest {
    
    private double tolerance = 0.0001;
    
//    @Test
//    public void discr1DFittingTest_1PlusX() throws FourierSeriesCoefficientFittingException, ExampleFourierSeriesOutOfValueRangeException {
          // has a peack at the first value - otherwise nice fit!!!
//        //test(new ExampleFourierSeries1PlusX(), 0.5);
//    }
    
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
            assert (Math.abs(realValue - calcValue) < tol);
        }
    }

    private double rectangleFunction(double x) {
        if (x < Math.PI) {
            return -0.5;
        }
        return 0.5;
    }
    
    private double cosFunction(double x) {
        return Math.cos(x);
    }
    
    private double sinFunction(double x) {
        return Math.sin(x);
    }
    
    @Test
    public void discr1DFittingTest_Cosine() throws FourierSeriesCoefficientFittingException {
        // get the values
        int n = 100;
        double dx = 2 * Math.PI / (double) n;
        DataSpace space = new DataSpace();
        for (int i = 0; i < n; i++) {
            space.add(new DataPoint<Double>(new Double[]{i * dx - Math.PI}, cosFunction(i * dx - Math.PI)));
        }
        
        int order = 50;
        FourierSeriesLimit lowerLimit = new FourierSeriesLimit(1, -Math.PI);
        FourierSeriesLimit upperLimit = new FourierSeriesLimit(1, Math.PI);
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
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(0).get(0).getValue()[0]) < tolerance);
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(1).get(0).getValue()[0]) > tolerance);
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(1).get(0).getValue()[0]) > 0.9);
        
        for (int i = 0; i < order; i++) {
            assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[1]) < tolerance);
        }
        for (int i = 2; i < order; i++) {
            assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[0]) < tolerance);
        }
    }
    
    @Test
    public void discr1DFittingTest_Sine() throws FourierSeriesCoefficientFittingException {
        // get the values
        int n = 100;
        double dx = 2 * Math.PI / (double) n;
        DataSpace space = new DataSpace();
        for (int i = 0; i < n; i++) {
            space.add(new DataPoint<Double>(new Double[]{i * dx - Math.PI}, sinFunction(i * dx - Math.PI)));
        }
        
        int order = 50;
        FourierSeriesLimit lowerLimit = new FourierSeriesLimit(1, -Math.PI);
        FourierSeriesLimit upperLimit = new FourierSeriesLimit(1, Math.PI);
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
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(0).get(0).getValue()[1]) < tolerance);
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(1).get(0).getValue()[1]) > tolerance);
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(1).get(0).getValue()[1]) > 0.9);
        
        for (int i = 0; i < order; i++) {
            assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[0]) < tolerance);
        }
        for (int i = 2; i < order; i++) {
            assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[1]) < tolerance);
        }
    }
    
    @Test
    public void discr1DFittingTest_RectangleFunction() throws FourierSeriesCoefficientFittingException {
        // get the values
        int n = 100;
        double dx = 2 * Math.PI / (double) n;
        DataSpace space = new DataSpace();
        for (int i = 0; i < n; i++) {
            space.add(new DataPoint<Double>(new Double[]{i * dx}, rectangleFunction(i * dx)));
        }
        
        int order = 45;
        FourierSeriesLimit lowerLimit = new FourierSeriesLimit(1, 0);
        FourierSeriesLimit upperLimit = new FourierSeriesLimit(1, 2.0 * Math.PI);
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
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(0).get(0).getValue()[0]) < tolerance);
        assert (Math.abs(coeff.getFourierCoefficientsByIndex(0).get(0).getValue()[1]) < tolerance);
        
        for (int i = 1; i < order; i++) {
            if (i % 2 == 0) {
                assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[0]) < tolerance);
                assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[1]) < tolerance);
            } else {
                assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[0]) > 0.01);
                assert (Math.abs(coeff.getFourierCoefficientsByIndex(i).get(0).getValue()[1]) > 0.001);
            }
        }
    }
}
