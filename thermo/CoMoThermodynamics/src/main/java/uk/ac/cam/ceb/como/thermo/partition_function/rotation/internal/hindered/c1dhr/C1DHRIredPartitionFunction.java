/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.c1dhr;

import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.integrator.SimpsonIntegrator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;

/**
 *
 * @author pb556
 */
public class C1DHRIredPartitionFunction extends PartitionFunction {

    protected double T = 0.0;
    protected Function approximation = null;
    //protected RombergIntegrator integrator = new RombergIntegrator();
    protected SimpsonIntegrator integrator = new SimpsonIntegrator();
    private ITorsionBarrier1 iTorsionBarrier1 = new ITorsionBarrier1();
    private ITorsionBarrier2 iTorsionBarrier2 = new ITorsionBarrier2();
    private ITorsionBarrier3 iTorsionBarrier3 = new ITorsionBarrier3();

    public C1DHRIredPartitionFunction() {
        super();
    }

    public C1DHRIredPartitionFunction(Function approximation) {
        super();
        this.approximation = approximation;
    }
    private int symmetry = -1;
    private Function reducedMoment = null;

    public void setRotorSymmetry(int symmetry) {
        this.symmetry = symmetry;
    }

    public void setReducedMoment(Function reducedMoment) {
        this.reducedMoment = reducedMoment;
    }
    
    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        PartitionValues Q = new PartitionValues();
        //symmetry = 1;
        integrator.setLimit(0, 2.0 * Math.PI / (double) symmetry);
        
        try {
            // CALCULATE integrals
            integrator.setFunction(iTorsionBarrier1);
            double val_iTorsionBarrier1 = integrator.getIntegral();
            integrator.setFunction(iTorsionBarrier2);
            double val_iTorsionBarrier2 = integrator.getIntegral();
            integrator.setFunction(iTorsionBarrier3);
            double val_iTorsionBarrier3 = integrator.getIntegral();

            double K = Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B / (PhysicalConstants.h * PhysicalConstants.h));

            double m = 1.0 / PhysicalConstants.k_B;
            
            // CALCULATE q
            Q.q = K * Math.sqrt(T) * val_iTorsionBarrier1;

            // CALCULATE dqBydT
            double first_in = 0.5 / T;
            
            Q.dqBydT = Q.q * first_in + (K * m * val_iTorsionBarrier2 * Math.pow(T, -1.5));
            
            double term1 = (-1.0) * K * val_iTorsionBarrier1 / (4.0 * Math.pow(T, (3.0 / 2.0)));
            double term2 = (K * val_iTorsionBarrier2 * (1.0 / (T * T * PhysicalConstants.k_B))) / (Math.sqrt(T));
            double term3 = (-1.0) * K * Math.sqrt(T) * 2.0 * val_iTorsionBarrier2 / (Math.pow(T, 3.0) * PhysicalConstants.k_B);
            double term4 = K * Math.sqrt(T) * val_iTorsionBarrier3 / (Math.pow(T, 4.0) * PhysicalConstants.k_B * PhysicalConstants.k_B);
            
            Q.d2qBydT2 = term1 + term2 + term3 + term4;

        } catch (Exception fce) {
            return null;
        }
        return Q;
    }

    private class ITorsionBarrier1 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) {
            try {
                double redMoment = (Double) reducedMoment.f(x);
                double fx = ((Double) ((DataPoint) approximation.f(new Double[]{x})).getValue()).doubleValue() ;
                return Math.sqrt(redMoment) * Math.exp(-fx / (PhysicalConstants.k_B * T));
            } catch (Exception ex) {
                Logger.getLogger(C1DHRPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
            }
            return 0.0;
        }
    }

    private class ITorsionBarrier2 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) {
            try {
                double redMoment = (Double) reducedMoment.f(x);
                double fx = ((Double) ((DataPoint) approximation.f(new Double[]{x})).getValue()).doubleValue();
                return fx * Math.sqrt(redMoment) * Math.exp(-fx / (PhysicalConstants.k_B * T));
            } catch (Exception ex) {
                Logger.getLogger(C1DHRPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
            }
            return 0.0;
        }
    }

    private class ITorsionBarrier3 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) {
            try {
                double redMoment = (Double) reducedMoment.f(x);
                double fx = ((Double) ((DataPoint) approximation.f(new Double[]{x})).getValue()).doubleValue() ;
                return fx * fx * Math.sqrt(redMoment) * Math.exp(-fx / (PhysicalConstants.k_B * T));
            } catch (Exception ex) {
                Logger.getLogger(C1DHRPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
            }
            return 0.0;
        }
    }
}