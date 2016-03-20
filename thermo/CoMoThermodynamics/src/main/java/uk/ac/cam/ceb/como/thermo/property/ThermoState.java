package uk.ac.cam.ceb.como.thermo.property;

import uk.ac.cam.ceb.como.math.unit.CompositeProperty;
import uk.ac.cam.ceb.como.math.unit.UnitSystem;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;

/**
 * Molar thermodynamic properties
 *
 * @author pb556
 */
public class ThermoState extends CompositeProperty {

    public double T = 0.0;
    public double S = 0.0;
    public double Cv = 0.0;
    public double Cp = 0.0;
    public double delta_H = 0.0;
    public double G = 0.0;
    public double H = 0.0;
    public double P = PhysicalConstants.P_1atm;

    @Override
    public String toString(UnitSystem unit) {
        switch (unit) {
            case GSD:
                return formatString(1 / PhysicalConstants.Cal, "cal");
            case SI:
            default: // SI
                return formatString(1, "J");
        }
    }

    private String formatString(double factor, String energy_unit) {
        return "*** Thermodynamic Properties at T = " + T + " K and P = " + P / PhysicalConstants.P_1atm + " atm ***" + "\n"
                + "Entropy (S)                             = " + S * factor + " " + energy_unit + "/mol-K" + "\n"
                + "Heat capacity at constant volume (Cv)   = " + Cv * factor + " " + energy_unit + "/mol" + "\n"
                + "Heat capacity at constant pressure (Cp) = " + Cp * factor + " " + energy_unit + "/mol" + "\n"
                + "Delta H(T) = dH(T) = H(T) - H(0K)       = " + delta_H * factor / 1000 + " k" + energy_unit + "/mol" + "\n"
                + "Enthalpy (H) = dH(T) + H(0K)            = " + H * factor / 1000 + " k" + energy_unit + "/mol" + "\n"
                + "Gibb free energy (G)                    = " + G * factor / 1000 + " k" + energy_unit + "/mol";
    }
}
