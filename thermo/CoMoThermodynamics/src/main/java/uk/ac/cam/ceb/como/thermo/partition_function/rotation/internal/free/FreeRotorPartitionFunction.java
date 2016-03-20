package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.free;

import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.integrator.RombergIntegrator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.c1dhr.C1DHRPartitionFunction;

/**
 *
 * @author pb556
 */
public class FreeRotorPartitionFunction extends PartitionFunction {

    private double T = 0.0;
    private double Ir = 0.0;
    private double V = 0.0;
    private int n = 1;
    protected RombergIntegrator integrator = new RombergIntegrator();
    private ITorsionBarrier1 iTorsionBarrier1 = new ITorsionBarrier1();
    private ITorsionBarrier2 iTorsionBarrier2 = new ITorsionBarrier2();
    private ITorsionBarrier3 iTorsionBarrier3 = new ITorsionBarrier3();

    public void setReducedMoment(double Ir, int rotor_symmetry) {
        this.Ir = Ir;
        this.n = rotor_symmetry;
    }

    public void setBarrierEnergy(double V) {
        this.V = V;
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        PartitionValues Q = new PartitionValues();

        this.T = T;
        //symmetry = 1;
        integrator.setLimit(0, 2.0 * Math.PI / (double) n);

        try {
            // CALCULATE integrals
            integrator.setFunction(iTorsionBarrier1);
            double val_iTorsionBarrier1 = integrator.getIntegral();
            integrator.setFunction(iTorsionBarrier2);
            double val_iTorsionBarrier2 = integrator.getIntegral();
            integrator.setFunction(iTorsionBarrier3);
            double val_iTorsionBarrier3 = integrator.getIntegral();

            double K = 2.0 * Math.PI
                    * Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B * Ir
                    / (PhysicalConstants.h * PhysicalConstants.h))
                    / (double) n;
            
//            double K = Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B * Ir
//                    / (PhysicalConstants.h * PhysicalConstants.h));

            
            Q.q = K * Math.sqrt(T);
            Q.dqBydT = K * Math.pow(T, -0.5) / 2.0;
            
            Q.dlnqBydT = 1.0 / (2.0 * T) + 1.0 / (T * T);
            
            Q.d2qBydT2 = K * Math.pow(T, -1.5) * 0.5 * (-0.5);
            
//            System.out.println("Temp = " + T);
//            
//            System.out.println("q  free = " + Q.q);
//            System.out.println("dq free = " + Q.dqBydT);
            
            return Q;

//            double K = Math.sqrt(2.0 * Math.PI * PhysicalConstants.k_B * Ir / (PhysicalConstants.h * PhysicalConstants.h));
            //double K = Math.sqrt(2.0 * Math.PI * uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.k_B * reducedMoment / (uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.h * uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.h));

            //double KemT = K * Math.exp(-m / T);
            //double T2 = T * T;

            //NUMERICAL SOLUTION
//            double K = Math.sqrt(2.0 * Math.PI * uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.k_B * Ir / (uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.h * uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.h));
//
//            //double KemT = K * Math.exp(-m / T);
//            double T2 = T * T;
//
//            double m = 1.0 / uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.k_B;
//
//            // CALCULATE q
//            Q.q = K * Math.sqrt(T) * (2.0 * Math.PI / ((double) n));
//
//            // CALCULATE dqBydT
//            double first_in = 0.5 / T; // + m / T2;
//            // plus set instead of a minus!!!
//            Q.dqBydT = Q.q * first_in + (K * m * 0.0 * Math.pow(T, -1.5));
//
//            // CALCULATE d2qBydT2
////            double firstTerm = Q.dqBydT * first_in - Q.q * (0.5 / T2); //  + 2 * m / T2 / T
////            double secondTerm = (Q.q * first_in - Q.dqBydT) * (-1.5 / T - m * val_iTorsionBarrier3 / T2 / val_iTorsionBarrier2);
////            Q.d2qBydT2 = firstTerm - secondTerm;
//
//            double term1 = Q.dqBydT / (2.0 * T);
//            double term2 = Q.q / (2.0 * T2);
//            double term3 = K * Math.pow(T, -1.5) * Math.pow(T, -2) * 0.0 * m * m;
//            double term4 = (3.0 / 2.0) * K * Math.pow(T, -2.5) * m * Math.pow(T, -2) * 0.0 * m * m;
//
//            Q.d2qBydT2 = term1 - term2 + term3 - term4;
//
//            return Q;


            // ANALYTICAL SOLUTION
//            double m = 1.0 / uk.ac.cam.ceb.como.thermo.partitionfunc.PhysicalConstants.k_B;
//
//            // CALCULATE q
//            Q.q = K * Math.sqrt(T) * 2.0 * Math.PI / ((double) n);
//
//            // CALCULATE dqBydT
//            double first_in = 0.5 / T; // + m / T2;
//            Q.dqBydT = Q.q * first_in;
//
//            // CALCULATE d2qBydT2
//            double firstTerm = Q.dqBydT * first_in - Q.q * (0.5 / (T * T)); //  + 2 * m / T2 / T
//            double secondTerm = (Q.q * first_in - Q.dqBydT) * (-1.5 / T);
//            Q.d2qBydT2 = firstTerm - secondTerm;
//
//            return Q;

//             //CALCULATE q
//            Q.q = K * Math.sqrt(T);
//            //System.out.println("FS Q.q = " + Q.q);
//
//            // CALCULATE dqBydT
//            double term1 = (1 / (2.0 * Math.sqrt(T)));
//            Q.dqBydT = K * (term1);
//
//            //System.out.println("FS Q.dqBydT = " + Q.dqBydT);
//
//            double term3 = (-1 / (4.0 * Math.pow(T, 1.5)));
//            Q.d2qBydT2 = K * (term3);
//
//            return Q;



//        System.out.println("T = " + T);
//        System.out.println("Free rotor (q):      " + Q.q);
//        System.out.println("Free rotor (dqBydT): " + Q.dqBydT);

            //return Q;

        } catch (Exception fce) {
            return null;
        }
    }

    private class ITorsionBarrier1 extends Function<Double, Double> {

        @Override
        public Double f(Double x, Object... additionalData) {
            try {
                double fx = 0.0;
                return Math.exp(-fx / (PhysicalConstants.k_B * T));
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
                double fx = 0.0;
                return fx * Math.exp(-fx / (PhysicalConstants.k_B * T));
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
                double fx = 0.0;
                return fx * fx * Math.exp(-fx / (PhysicalConstants.k_B * T));
            } catch (Exception ex) {
                Logger.getLogger(C1DHRPartitionFunction.class.getName()).log(Level.SEVERE, null, ex);
            }
            return 0.0;
        }
    }
//    @Override
//    public PartitionValues getPartitionValues(double T) {
//        PartitionValues Q = new PartitionValues();
//        // CALCULATE q
//        double c_in = 2 * Math.PI * PhysicalConstants.k_B * Ir * T;
//        c_in = Math.sqrt(c_in) / PhysicalConstants.hbar;
//        Q.q = (c_in / n) * Math.exp(-V / (2 * PhysicalConstants.k_B * T));
//
//        // CALCULATE dqBydT
//        double T2 = T * T;
//        double d_in = V / (2 * PhysicalConstants.k_B * T2) + 0.5 / T;
//        Q.dqBydT = Q.q * d_in;
//
//        // CALCULATE d2qBydT2
//        double e_in = V / (PhysicalConstants.k_B * T2 * T) + 0.5 / (T2);
//        Q.d2qBydT2 = (Q.dqBydT * Q.dqBydT / Q.q) - (Q.q * e_in);
//        return Q;
//    }
}
