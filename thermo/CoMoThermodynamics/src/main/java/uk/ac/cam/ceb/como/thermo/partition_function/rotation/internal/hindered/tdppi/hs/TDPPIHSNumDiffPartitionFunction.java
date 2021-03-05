/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.tdppi.hs;

import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.fourier.series.FourierSeries;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.integrator.RombergIntegrator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.c1dhr.C1DHRPartitionFunction;

/**
 *
 * @author pb556
 */
public class TDPPIHSNumDiffPartitionFunction extends PartitionFunction {

    protected double T = 0.0;
    protected FourierSeries approximation = null;
    protected double frequency = 0.0;
    protected double w = 0.0;
    protected RombergIntegrator integrator = new RombergIntegrator();
    private ITorsionBarrier1 iTorsionBarrier1 = new ITorsionBarrier1();

    public TDPPIHSNumDiffPartitionFunction() {
        super();
    }

    public TDPPIHSNumDiffPartitionFunction(FourierSeries approximation, double frequency) {
        super();
        this.approximation = approximation;
        this.frequency = frequency;
    }
    private int symmetry = -1;
    private double reducedMoment = 0.0;

    public void setRotorSymmetry(int symmetry) {
        this.symmetry = symmetry;
    }

    public void setReducedMoment(double reducedMoment) {
        this.reducedMoment = reducedMoment;
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        PartitionValues Q = new PartitionValues();

        double dT = Math.pow(10, -15);

        Q.q = calculateQ(T);
        double q1 = calculateQ(T + dT);
        double q2 = calculateQ(T - dT);

        Q.dqBydT = (q1 - Q.q) / dT;
        Q.dqBydT = (q1 - 2.0* Q.q + q2) / dT;

        return Q;
    }

    private double calculateQ(double T) {
        integrator.setLimit(0, 2.0 * Math.PI / (double) symmetry);

        try {
            // CALCULATE integrals
            integrator.setFunction(iTorsionBarrier1);
            double val_iTorsionBarrier1 = integrator.getIntegral();
            double K = Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B / (PhysicalConstants.h * PhysicalConstants.h));
            return K * Math.sqrt(T) * val_iTorsionBarrier1;
        } catch (Exception ex) {
            Logger.getLogger(TDPPIHSPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
        }
        return 0.0;
    }

    private double calculateC(double k) {
        w = Math.sqrt(Math.abs(k) / reducedMoment);//

        double beta = (1.0) / (PhysicalConstants.k_B * T);
        double kappa;

        double y = (beta * PhysicalConstants.hbar * w) / (2.0);
        kappa = (Math.sinh(y)) / (y);
        return Math.sqrt((8 / (beta * Math.abs(k))) * Math.log(kappa));
    }

    private class ITorsionBarrier1 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) {
            try {
                double k = (Double) approximation.fDerivative(2, new Double[]{0.0}).getValue();
                double c = calculateC(k);
                double fx_A = (Double) ((DataPoint) approximation.f(new Double[]{x + c / 2})).getValue();
                double fx_B = (Double) ((DataPoint) approximation.f(new Double[]{x - c / 2})).getValue();
                return Math.sqrt((Double) reducedMoment) * Math.exp(-1 * (fx_A + fx_B) / (2 * PhysicalConstants.k_B * T));
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
                double k = (Double) approximation.fDerivative(2, new Double[]{0.0}).getValue();
                double c = calculateC(k);
                double fx_A = (Double) ((DataPoint) approximation.f(new Double[]{x + c / 2})).getValue();
                double fx_B = (Double) ((DataPoint) approximation.f(new Double[]{x - c / 2})).getValue();
                return (fx_A + fx_B) * Math.sqrt((Double) reducedMoment) * Math.exp(-(fx_A + fx_B) / (2 * PhysicalConstants.k_B * T));
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
                double k = (Double) approximation.fDerivative(2, new Double[]{0.0}).getValue();
                double c = calculateC(k);
                double fx_A = (Double) ((DataPoint) approximation.f(new Double[]{x + c / 2})).getValue();
                double fx_B = (Double) ((DataPoint) approximation.f(new Double[]{x - c / 2})).getValue();
                return (fx_A + fx_B) * (fx_A + fx_B) * Math.sqrt(reducedMoment) * Math.exp(-(fx_A + fx_B) / (2 * PhysicalConstants.k_B * T));
            } catch (Exception ex) {
                Logger.getLogger(C1DHRPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
            }
            return 0.0;
        }
    }
}
