/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.function.Function;

/**
 *
 * @author pb556
 */
public abstract class FourierSeries extends Function<DataPoint, Double[]> {
    
    // dependency injection
    protected FourierSeriesCalculator calculator;
    protected FourierSeriesParameters parameters;
    
    public FourierSeries(FourierSeriesParameters parameters) {
        this.parameters = parameters;
    }
    
    public FourierSeries(int order, FourierSeriesCoefficients coefficients, FourierSeriesLimits lower, FourierSeriesLimits upper) {
        this.parameters = new FourierSeriesParameters(order, coefficients, lower, upper);
    }
    
    public FourierSeriesParameters getParameters() {
        return parameters;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FourierSeries) {
            return ((FourierSeries)(obj)).getParameters().equals(parameters);
        }
        return false;
    }
    
    public abstract DataPoint fDerivative(int order, Double[] x) throws Exception;
}
