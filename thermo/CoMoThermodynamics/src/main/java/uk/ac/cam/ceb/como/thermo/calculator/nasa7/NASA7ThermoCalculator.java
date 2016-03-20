package uk.ac.cam.ceb.como.thermo.calculator.nasa7;

import uk.ac.cam.ceb.como.thermo.converter.nasa7.NASA7;
import uk.ac.cam.ceb.como.thermo.property.ThermoState;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.thermo.calculator.ThermoCalculator;

/**
 *
 * @author pb556 Phadungsuakanan
 */
public class NASA7ThermoCalculator implements ThermoCalculator {

    protected Logger logger = Logger.getLogger(getClass().getName());
    private final NASA7 nasa7;
    private double P = PhysicalConstants.P_1atm;

    /**
     * Default constructor of 7-term <code>NASAPolynomial</code>.
     * @param nasa7
     */
    public NASA7ThermoCalculator(NASA7 nasa7) {
        this.nasa7 = nasa7;
    }

    public NASA7 getNASA7() {
        return nasa7;
    }

    /**
     * Get thermochemistry at given T (K) and P = 1 atm.
     * @param T Temperature (K)
     * @return Thermochemistry at given T (K).
     */
    @Override
    public ThermoState getThermoState(double T) {
        double[] a = nasa7.getPolynomialArray(T);
        if (a == null) {
            logger.warn("Temperature is not in the range for calculating thermochemistry from NASA polynomials.");
            return null;
        } else {
            ThermoState therm = new ThermoState();
            therm.T = T;
            // Pre-calculated temperature exponents.
            double T2 = T * T;
            double T3 = T2 * T;
            double T4 = T3 * T;
            double T5 = T4 * T;
            double lnT = Math.log(T);

            therm.Cp = PhysicalConstants.R * (a[0] + a[1] * T + a[2] * T2 + a[3] * T3 + a[4] * T4);
            therm.H = PhysicalConstants.R * (a[0] * T + a[1] * T2 / 2 + a[2] * T3 / 3 + a[3] * T4 / 4 + a[4] * T5 / 5 + a[5]);
            therm.S = PhysicalConstants.R * (a[0] * lnT + a[1] * T + a[2] * T2 / 2 + a[3] * T3 / 3 + a[4] * T4 / 4 + a[6]);
            therm.Cv = therm.Cp - PhysicalConstants.R;
            //therm.delta_H = therm.H - H298p15K;
            //therm.delta_H = therm.H - PhysicalConstants.R * nasa7.getALow(5);
            therm.delta_H = therm.H - PhysicalConstants.R * a[5];

            // now correct with pressure term
            therm.P = P;
            therm.S -= PhysicalConstants.R * Math.log(P / PhysicalConstants.P_1atm);
            // Molar Cv, Molar Cp does not depend on pressue, thus no change.
            // Molar delta_H and H does not depend on pressue, thus no change. (because it is an integral of Cp)
            therm.G = therm.H - therm.T * therm.S;

            return therm;
        }
    }

    /**
     * Set pressure to thermochemistry calculator (N/m<sup>2</sup>). default is 1 atm.
     * @param P Pressure in (N/m^2)
     */
    @Override
    public void setPressure(double P) {
        this.P = P;
    }

    /**
     * Give a string description of the <code>NASAPolynomial</code> class.
     * @return String description of the <code>NASAPolynomial</code> object.
     */
    @Override
    public String toString() {
        return "NASA 7-Cofficient Calculator for :+\n" + nasa7;
    }
}
