package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.s1dhr;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;
import uk.ac.cam.ceb.como.math.integrator.RombergIntegrator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;

public class S1DHRPartitionFunction extends PartitionFunction {

    private double Ir = 0.0;
    private double V = 0.0;
    private double m = 0.0;
    private int n = 1;
    private double T = 0.0;
    private RombergIntegrator integrator = new RombergIntegrator();
    private Icos0 icos0 = new Icos0();
    private Icos1 icos1 = new Icos1();
    private Icos2 icos2 = new Icos2();

    @Override
    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable) {
        super.setThermoAnalyzable(thermoAnalyzable);
    }

    public void setReducedMoment(double Ir, int rotor_symmetry) {
        this.Ir = Ir;
        this.n = rotor_symmetry;
    }

    public void setBarrierEnergy(double V) {
        this.V = V;
        m = V / 2 / PhysicalConstants.k_B;
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        PartitionValues Q = new PartitionValues();
        integrator.setLimit(0, 2.0 * Math.PI / n);

        try {
            // CALCULATE integrals
            integrator.setFunction(icos0);
            double val_icos0 = integrator.getIntegral();
            integrator.setFunction(icos1);
            double val_icos1 = integrator.getIntegral();
            integrator.setFunction(icos2);
            double val_icos2 = integrator.getIntegral();

            // UNIVERSAL constants
            //double K = Math.sqrt(2 * Math.PI * PhysicalConstants.k_B * Ir / PhysicalConstants.hbar / PhysicalConstants.hbar) / n;
            double K = Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B * Ir / (PhysicalConstants.h * PhysicalConstants.h));

            // CALCULATE q
            Q.q = K * Math.exp(-m / T) * Math.sqrt(T) * val_icos0;
            //System.out.println("Symm Q.q = " + Q.q);

            // CALCULATE dqBydT
            double term1 = (1 / (2 * Math.sqrt(T))) * val_icos0;
            double term2 = (V / (2 * PhysicalConstants.k_B * Math.pow(T, 1.5))) * (val_icos0 - val_icos1);
            Q.dqBydT = K * Math.exp(-V / (2 * PhysicalConstants.k_B * T)) * (term1 + term2);
            //System.out.println("Symm Q.dqBydT = " + Q.dqBydT);

            // CALCULATE d2qBydT2
            double term3 = (-1 / (4 * Math.pow(T, 1.5))) * val_icos0;
            double term4a = (1 / (Math.pow(T, 2.5))) * (val_icos1 - val_icos0);
            double term4b = (V / (2 * PhysicalConstants.k_B * Math.pow(T, 3.5))) * (val_icos2 + val_icos0 - 2 * val_icos1);
            double term4 = (V / (2 * PhysicalConstants.k_B)) * (term4a + term4b);
            Q.d2qBydT2 = K * Math.exp(-V / (2 * PhysicalConstants.k_B * T)) * (term3 + term4);
            //System.out.println("Symm Q.d2qBydT2 = " + Q.d2qBydT2);

            return Q;
        } catch (Exception e) {
        }
        return null;
    }

    private class Icos0 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
            return Math.exp(V / (2 * PhysicalConstants.k_B * T) * Math.cos(x));
        }
    }

    private class Icos1 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
            double cosx = Math.cos(x);
            return cosx * Math.exp(V / (2 * PhysicalConstants.k_B * T) * Math.cos(x));
        }
    }

    private class Icos2 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
            double cosx = Math.cos(x);
            return cosx * cosx * Math.exp(V / (2 * PhysicalConstants.k_B * T) * Math.cos(x));
        }
    }
}
