package uk.ac.cam.ceb.como.thermo.calculator;

import uk.ac.cam.ceb.como.thermo.property.ThermoState;

/**
 *
 * @author pb556
 */
public interface ThermoCalculator {
    /**
     * Get thermochemistry (ThermoState) at given T (K).
     * @param T Temperature (K)
     * @return ThermoState at given T (K).
     */
    ThermoState getThermoState(double T) throws Exception;

    /**
     * Set pressure to thermochemistry calculator (N/m<sup>2</sup>)
     * @param P Pressure in (N/m^2)
     */
    void setPressure(double P);
}
