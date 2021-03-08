/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

/**
 *
 * @author pb556
 */
public abstract class FourierSeriesCoefficientFitting {
    
    protected FourierSeriesParameters parameters = null;
    protected FourierSeriesCoefficients coeff = null;
    protected FourierSeriesLimits lower = null;
    protected FourierSeriesLimits upper = null;
    protected int order = 0;
    
    public FourierSeriesCoefficientFitting() {
    }
    
    public FourierSeriesCoefficientFitting(int order) {
        this.order = order;
    }
    
    public FourierSeriesCoefficientFitting(FourierSeriesLimits lower, FourierSeriesLimits upper) {
        this.lower = lower;
        this.upper = upper;
    }
    
    public FourierSeriesCoefficientFitting(int order, FourierSeriesLimits lower, FourierSeriesLimits upper) {
        this.lower = lower;
        this.upper = upper;
        this.order = order;
    }
    
    public void setOrder(int order) throws Exception {
        if (order < 0) {
            throw new FourierSeriesInvalidValueException("Invalid order.");
        }
        this.order = order;
    }
    
    public void setLimits(FourierSeriesLimits lower, FourierSeriesLimits upper) throws Exception {
        this.lower = lower;
        this.upper = upper;
    }
    
    public abstract void fitFourierSeriesCoefficients() throws FourierSeriesCoefficientFittingException;
    
    public FourierSeriesParameters getFourierSeriesParameters() {
        return parameters;
    }
}
