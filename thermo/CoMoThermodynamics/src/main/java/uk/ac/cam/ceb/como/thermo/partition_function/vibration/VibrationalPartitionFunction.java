package uk.ac.cam.ceb.como.thermo.partition_function.vibration;

import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;

/**
 *
 * @author pb556
 */
public class VibrationalPartitionFunction extends PartitionFunction {
    private double omega = 0.0;
    private double freq = 0.0;
    private double T = 0.0;

    /**
     * Set frequency of the partition function.
     * @param wavenumber in cm^-1
     */
    public void setFrequency(double wavenumber) {
        freq = wavenumber;
        omega = wavenumber * 200 * PhysicalConstants.c * Math.PI;
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        return getPartitionValues(T, omega, 1.0);
    }

    @Override
    public PartitionValues getPartitionValues(double T, double scalingFactor) {
        this.T = T;
        return getPartitionValues(T, omega, scalingFactor);
    }

    public PartitionValues getPartitionValues(double T, double omega, double scalingFactor) {
        this.T = T;
        PartitionValues Q = new PartitionValues();
        double hbar_k_B = PhysicalConstants.hbar / PhysicalConstants.k_B;
        double m = hbar_k_B * omega * scalingFactor;
        double m_T = m/T;
        // CALCULATE q
        Q.q = 1.0 / (1 - Math.exp(-m_T)); // if the zero of energy scale is at hw/2kT

        // exp(-Thetav(i)/(2*T))/(1-exp(-Thetav(i)/(T)))
        //double x = freq / (PhysicalConstants.k_B/(PhysicalConstants.h *100*PhysicalConstants.c));
        //double newq = 1/(1-Math.exp(-x/(T)));
        
        //System.out.println("qvib(" + freq + ") = " + Q.q);
        //System.out.println("qvib_new(" + freq + ") = " + newq);
        
        // CALCULATE dqBydT
        Q.dqBydT = Q.q * Q.q * m /T / T * Math.exp(-m_T);

        // CALCULATE d2qBydT2
        Q.d2qBydT2 = Q.dqBydT*(2*Q.dqBydT/Q.q - 2/T + m/T/T);

        return Q;
    }
}
