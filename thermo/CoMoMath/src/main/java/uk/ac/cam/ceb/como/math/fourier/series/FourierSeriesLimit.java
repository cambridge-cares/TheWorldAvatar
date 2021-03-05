/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

/**
 *
 * @author pb556
 */
public class FourierSeriesLimit {
    
    protected int dim;
    protected double limit;
    
    public FourierSeriesLimit(int dimension, double limit) {
        this.dim = dimension;
        this.limit = limit;
    }
    
    public int getDimension() {
        return dim;
    }
    
    public double getLimit() {
        return limit;
    }
    
    public boolean equals(FourierSeriesLimit limit) {
        return getDimension() == limit.getDimension() && getLimit() == limit.getLimit();
    }
}
