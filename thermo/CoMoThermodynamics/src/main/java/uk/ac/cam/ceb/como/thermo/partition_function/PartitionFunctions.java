/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function;

import java.util.ArrayList;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.simple.UniformFunction;

/**
 *
 * @author pb556
 */
public class PartitionFunctions extends ArrayList<PartitionFunction> {

    // define the partition functions!!!
    private Function freqScaling = new UniformFunction(1.0);
    private Function freqScalingZPE = null;
    private Function freqScalingLowFreq = null;
    private Function freqScalingCp = null;
    private Function freqScalingH = null;
    private Function freqScalingS = null;

    public PartitionFunctions() {
        freqScaling = new UniformFunction(1.0);
    }
    
    public PartitionFunctions(Function sFactor) {
        freqScaling = sFactor;
    }

    public PartitionFunctions(Function sFactor, Function sFactorZPE, Function sFactorLowFreq, Function sFactorH, Function sFactorS, Function sFactorCp) {
        freqScaling = sFactor;
        freqScalingZPE = sFactorZPE;
        freqScalingLowFreq = sFactorLowFreq;
        freqScalingCp = sFactorCp;
        freqScalingH = sFactorH;
        freqScalingS = sFactorS;
    }
    
    public void setScalingFactor(Function factor) {
        freqScaling = factor;
    }
    
    public void setFundamentalScalingFactor(Function factor) {
        freqScaling = factor;
    }
    
    public void setZPEScalingFactor(Function factor) {
        freqScalingZPE = factor;
    }
    
    public void setLowFreqScalingFactor(Function factor) {
        freqScalingLowFreq = factor;
    }
    
    public void setHScalingFactor(Function factor) {
        freqScalingH = factor;
    }
    
    public void setSScalingFactor(Function factor) {
        freqScalingS = factor;
    }
    
    public void setCpScalingFactor(Function factor) {
        freqScalingCp = factor;
    }
    
    public Function getScalingFactor() {
        return freqScaling;
    }
    
    public Function getFundamentalScalingFactor() {
        return freqScaling;
    }
    
    public Function getZPEScalingFactor() {
        return freqScalingZPE;
    }
    
    public Function getHScalingFactor() {
        return freqScalingH;
    }
    
    public Function getSScalingFactor() {
        return freqScalingS;
    }
    
    public Function getCpScalingFactor() {
        return freqScalingCp;
    }
    
    public Function getLowFreqScalingFactor() {
        return freqScalingLowFreq;
    }
    
    // get - unscaled
    
    public PartitionFunction getFundamental(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScaling);
        return q;
    }
    
    public PartitionFunction getZPE(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScalingZPE);
        return q;
    }
    
    public PartitionFunction getLowFreq(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScalingLowFreq);
        return q;
    }
    
    public PartitionFunction getH(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScalingH);
        return q;
    }
    
    public PartitionFunction getS(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScalingS);
        return q;
    }
    
    public PartitionFunction getCp(int index) {
        PartitionFunction q = get(index);
        q.setScalingFactor(freqScalingCp);
        return q;
    }
}
