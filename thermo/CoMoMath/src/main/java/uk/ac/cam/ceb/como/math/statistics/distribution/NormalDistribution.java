/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.statistics.distribution;

/**
 *
 * @author pb556
 */
public class NormalDistribution implements Distribution {
    
    protected double expValue = 0;
    protected double variance = 0;
    
    public NormalDistribution(double expValue, double variance) {
        this.expValue = expValue;
        this.variance = variance;
    }
    
    @Override
    public double getPDF(double x) {
        return 0.0;
    }
    
    @Override
    public double getCDF(double x) {
        return 0.0;
    }
    
    public double getExpValue() {
        return expValue;
    }
    
    public double getVariance() {
        return variance;
    }
    
    public void normalise() {
        
    }
}
