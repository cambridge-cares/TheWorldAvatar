/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.onedimensional.classical;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesCoefficients;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesLimits;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeriesParameters;
import uk.ac.cam.ceb.como.math.fourier.series.onedimensional.OneDimensionalFourierSeriesException;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */
public class ClassicalOneDimensionalFourierSeries extends FourierSeries {

    public ClassicalOneDimensionalFourierSeries(FourierSeriesParameters parameters) throws OneDimensionalFourierSeriesException {
        super(parameters);
        inputValidation();
    }

    public ClassicalOneDimensionalFourierSeries(int order, FourierSeriesCoefficients coefficients, FourierSeriesLimits lower, FourierSeriesLimits upper) throws OneDimensionalFourierSeriesException {
        super(order, coefficients, lower, upper);
        inputValidation();
    }

    protected final void inputValidation() throws OneDimensionalFourierSeriesException {
        // validation of the input data
        if (parameters.getFourierSeriesCoefficients() == null || parameters.getLowerLimits() == null || parameters.getUpperLimits() == null) {
            throw new OneDimensionalFourierSeriesException("Invalid parameters have been set.");
        }
        boolean validDim = true;
        for (int i = 0; i < parameters.getOrder(); i++) {
            validDim &= parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(i).size() == 1;
        }
        validDim &= parameters.getLowerLimits().getLimitsByDimension(1).size() == 1;
        validDim &= parameters.getUpperLimits().getLimitsByDimension(1).size() == 1;

        if (!validDim) {
            throw new OneDimensionalFourierSeriesException("Invalid dimensionality.");
        }
    }

    @Override
    public DataPoint f(Double[] x, Object... additionalData) throws FunctionCalculationException {
        if (x.length > 1) {
            throw new FunctionCalculationException("Invalid dimensionality.");
        }

        // calculations
        double yApprox = parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(0).get(0).getValue()[0] / 2.0;
        for (int k = 1; k < parameters.getOrder(); k++) {
            yApprox = yApprox
                    + parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(k).get(0).getValue()[0] * Math.cos((double) k * x[0])
                    + parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(k).get(0).getValue()[1] * Math.sin((double) k * x[0]);
        }
        return new DataPoint(x, yApprox);
    }

    @Override
    public DataPoint fDerivative(int order, Double[] x) throws Exception {
        if (order == 0) {
            return f(x);
        }
        // integreate the order
        double yApprox = 0;
        for (int k = 1; k < parameters.getOrder(); k++) {
            double nPower = Math.pow(k, order);
            double an = parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(k).get(0).getValue()[0];
            double bn = parameters.getFourierSeriesCoefficients().getFourierCoefficientsByIndex(k).get(0).getValue()[0];
            if (order % 4 == 0) {
                yApprox = yApprox + nPower * an * Math.cos((double) (k) * x[0])
                        + nPower * bn * Math.sin((double) k * x[0]);
            } else if (order % 4 == 1) {
                yApprox = yApprox - nPower * an * Math.sin((double) (k) * x[0])
                        + nPower * bn * Math.cos((double) k * x[0]);
            } else if (order % 4 == 2) {
                yApprox = yApprox - nPower * an * Math.cos((double) (k) * x[0])
                        - nPower * bn * Math.sin((double) k * x[0]);
            } else if (order % 4 == 3) {
                yApprox = yApprox + nPower * an * Math.sin((double) (k) * x[0])
                        - nPower * bn * Math.cos((double) k * x[0]);
            }
        }
        return new DataPoint(x, yApprox);
    }
}
