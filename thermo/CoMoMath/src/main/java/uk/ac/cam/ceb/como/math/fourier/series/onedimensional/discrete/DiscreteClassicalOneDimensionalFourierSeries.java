/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.onedimensional.discrete;

import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesParameters;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.OneDimensionalFourierSeriesException;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.classical.ClassicalOneDimensionalFourierSeries;

/**
 *
 * @author pb556
 */
public class DiscreteClassicalOneDimensionalFourierSeries extends ClassicalOneDimensionalFourierSeries {

    public DiscreteClassicalOneDimensionalFourierSeries(FourierSeriesParameters parameters) throws OneDimensionalFourierSeriesException {
        super(parameters);
    }

    public DiscreteClassicalOneDimensionalFourierSeries(int order, FourierSeriesCoefficients coefficients, FourierSeriesLimits lower, FourierSeriesLimits upper) throws OneDimensionalFourierSeriesException {
        super(order, coefficients, lower, upper);
    }
}
