package uk.ac.cam.ceb.como.chem.property;

import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;

public class EnthalpyOfFormation {

    private Phase phase = Phase.Unknown;
    /**
     * Enthalpy of formation in J/mol.
     */
    private double delta_H_f = 0.0;
    /**
     * error or standard deviation of enthalpy of formation in J/mol.
     */
    private Double error = null;
    /**
     * Temperature in K.
     */
    private double T = PhysicalConstants.T_25C;
    /**
     * Temperature in K.
     */
    private double P = PhysicalConstants.P_1atm;

    protected EnthalpyOfFormation() {
    }

    /**
     * T is set to 298.15 K
     *
     * @param delta_Hf in J/mol
     */
    
    public EnthalpyOfFormation(double delta_Hf) {
        this(PhysicalConstants.T_25C, delta_Hf, 0.0);
    }

    /**
     *
     * @param T in K
     * @param delta_Hf in J/mol
     */
    
    public EnthalpyOfFormation(double T, double delta_Hf) {
        this(T, delta_Hf, 0.0);
    }

    /**
     *
     * @param T in K
     * @param delta_Hf in J/mol
     * @param error J/mol
     */
    
    public EnthalpyOfFormation(double T, double delta_Hf, double error) {
        this.delta_H_f = delta_Hf;
        this.error = error;
        this.T = T;
    }

    public EnthalpyOfFormation(EnthalpyOfFormation hf) {
        this.delta_H_f = hf.delta_H_f;
        this.error = hf.error;
        this.T = hf.T;
    }

    public Phase getPhase() {
        return phase;
    }

    public void setPhase(Phase phase) {
        this.phase = phase;
    }

    public static EnthalpyOfFormation from_kcal_Per_mol(double T, double dHfInkcal_Per_mol, double errorInkcal_Per_mol) {
        return new EnthalpyOfFormation(T, dHfInkcal_Per_mol * PhysicalConstants.Cal, errorInkcal_Per_mol * PhysicalConstants.Cal);
    }

    public static EnthalpyOfFormation from_kcal_Per_mol(double T, double dHfInkcal_Per_mol) {
        return new EnthalpyOfFormation(T, dHfInkcal_Per_mol * PhysicalConstants.Cal);
    }

    public static EnthalpyOfFormation from_kcal_Per_mol(double dHfInkcal_Per_mol) {
        return new EnthalpyOfFormation(dHfInkcal_Per_mol * PhysicalConstants.Cal);
    }

    public double getValue() {
        return delta_H_f;
    }

    public double getValueIncal_Per_mol() {
        return delta_H_f / PhysicalConstants.Cal;
    }

    public Double getError() {
        return error;
    }

    public Double getErrorIncal_Per_mol() {
        if (error == null) {
            return null;
        } else {
            return error / PhysicalConstants.Cal;
        }
    }

    public double getTemperature() {
        return this.T;
    }

    protected void setValue(double value) {
        this.delta_H_f = value;
    }

    protected void setError(Double error) {
        this.error = error;
    }

    protected void setTemperature(double T) {
        this.T = T;
    }

    public double getPressure() {
        return P;
    }

    public void setPressure(double pressure) {
        this.P = pressure;
    }

    protected String print(double factor, String energy_unit) {
        return "Enthalpy of Formation at T = " + T + " K is " + (delta_H_f * factor) + " \u00B1 " + (error * factor) + " " + energy_unit + "/mol";
    }
}
