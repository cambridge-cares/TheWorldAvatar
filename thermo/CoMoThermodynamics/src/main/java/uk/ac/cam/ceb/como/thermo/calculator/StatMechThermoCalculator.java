/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator;

import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunctions;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;
import uk.ac.cam.ceb.como.thermo.partition_function.electronic.ElectronicPartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.free.FreeRotorPartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.overall.RotationalPartionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.translational.TranslationalPartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.vibration.VibrationalPartitionFunction;
import uk.ac.cam.ceb.como.thermo.property.ThermoState;
import uk.ac.cam.ceb.como.math.function.simple.UniformFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.tools.FrequencyChecker;

/**
 *
 * @author pb556
 */
public abstract class StatMechThermoCalculator implements ThermoCalculator {

    private Logger logger = Logger.getLogger(getClass());
    private PartitionFunctions Q = null;
    private ThermoAnalyzable thermoAnalyzable = null;
    private CMLMolecule mol = null;
    private boolean isPartitionFunctionChanged = true;
    private double P = PhysicalConstants.P_1atm;
    private double H_0K = 0;
    private boolean hasTranslation = true;
    private boolean hasRotation = true;
    private boolean hasVibration = true;
    private boolean hasElectronic = true;
    private boolean checkForNegativeFrequecy = true;
    
    private Function freqScalingZPE = new UniformFunction(1.0);
    private Function freqScalingLowFreq = new UniformFunction(1.0);
    private Function freqScalingC = new UniformFunction(1.0);
    private Function freqScalingH = new UniformFunction(1.0);
    private Function freqScalingS = new UniformFunction(1.0);

    public void setZPEScalingFactor(Function factor) {
        freqScalingZPE = factor;
    }

    public void setRotFreqScalingFactor(Function factor) {
        freqScalingLowFreq = factor;
    }

    public void setHScalingFactor(Function factor) {
        freqScalingH = factor;
    }

    public void setSScalingFactor(Function factor) {
        freqScalingS = factor;
    }

    public void setCScalingFactor(Function factor) {
        freqScalingC = factor;
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

    public Function getCScalingFactor() {
        return freqScalingC;
    }

    public Function getRotFreqScalingFactor() {
        return freqScalingLowFreq;
    }
    
    public void setZPEScalingFactor(double factor) throws Exception {
        freqScalingZPE = new UniformFunction(factor);
    }

    public void setRotFreqScalingFactor(double factor) throws Exception {
        freqScalingLowFreq = new UniformFunction(factor);
    }

    public void setHScalingFactor(double factor) throws Exception {
        freqScalingH = new UniformFunction(factor);
    }

    public void setCScalingFactor(double factor) throws Exception {
        freqScalingC = new UniformFunction(factor);
    }

    public void setSScalingFactor(double factor) throws Exception {
        freqScalingS = new UniformFunction(factor);
    }

    public PartitionFunctions getQ() {
        return Q;
    }

    protected void set(PartitionFunctions Q) {
        this.Q = Q;
    }

    public void setCheckForNegativeFrequecy(boolean checked) {
        this.checkForNegativeFrequecy = checked;
    }

    public boolean negativeFrequencyCheckRequested() {
        return checkForNegativeFrequecy;
    }
    
    public void setCMLMolecule(CMLMolecule mol) {
        this.mol = mol;
    }
    
    public CMLMolecule getCMLMolecule() {
        return mol;
    }

    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable, boolean validate) {
        this.thermoAnalyzable = thermoAnalyzable;
        if (validate) {
            FrequencyChecker.ensureCorrectFrequencyModes(thermoAnalyzable);
        }
        isPartitionFunctionChanged = true;
    }

    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable) {
        setThermoAnalyzable(thermoAnalyzable, true);
    }

    public ThermoAnalyzable getThermoAnalyzable() {
        return thermoAnalyzable;
    }

    public void setContributions(boolean hasTranslation, boolean hasRotation, boolean hasVibration, boolean hasElectronic) {
        this.hasTranslation = hasTranslation;
        this.hasRotation = hasRotation;
        this.hasVibration = hasVibration;
        this.hasElectronic = hasElectronic;
        isPartitionFunctionChanged = true;
    }

    public boolean hasTranslation() {
        return hasTranslation;
    }

    public boolean hasRotation() {
        return hasRotation;
    }

    public boolean hasVibration() {
        return hasVibration;
    }
    
    public boolean hasElectronic() {
        return hasElectronic;
    }

    public void recalculation(boolean status) {
        isPartitionFunctionChanged = status;
    }

    public boolean recalculationNeeded() {
        return isPartitionFunctionChanged;
    }

    public double getH_0K() {
        return H_0K;
    }

    protected void setH_0K(double H_0K) {
        this.H_0K = H_0K;
    }

    /**
     * Set pressure to thermochemistry calculator (N/m<sup>2</sup>). default is
     * 1 atm.
     *
     * @param P Pressure in (N/m^2)
     */
    @Override
    public void setPressure(double P) {
        this.P = P;
        isPartitionFunctionChanged = true;
    }

    public double getPressure() {
        return P;
    }

    protected abstract void applyChangesToPartionFunction() throws Exception;

    // add basic calculations and make them re-usable
    public PartitionFunction getTranslationalParitionFunction() {
        TranslationalPartitionFunction q_trans = new TranslationalPartitionFunction();
        q_trans.setThermoAnalyzable(getThermoAnalyzable());
        q_trans.setPressure(getPressure());
        return q_trans;
    }
    
    public PartitionFunction getElectronicParitionFunction() {
        ElectronicPartitionFunction q_elec = new ElectronicPartitionFunction();
        q_elec.setThermoAnalyzable(getThermoAnalyzable());
        return q_elec;
    }

    public PartitionFunction getRotationalPartitionFunction() {
        RotationalPartionFunction q_rot = new RotationalPartionFunction();
        q_rot.setThermoAnalyzable(getThermoAnalyzable());
        return q_rot;
    }

    public List<PartitionFunction> getVibrationalPartitionFunction() throws Exception {
        List<Double> freqs = new ArrayList<Double>();
        for (int i = 0; i < getThermoAnalyzable().getVibrationCount(); i++) {
            freqs.add(getThermoAnalyzable().getVibration(i).getFrequency());
        }
        return getVibrationalPartitionFunction(freqs);
    }

    public PartitionFunction getVibrationalPartitionFunction(double freq) throws Exception {
        if (negativeFrequencyCheckRequested() && freq < 0) {
            throw new Exception("Negative frequency detected, f_unscaled = " + freq);
        } else if (freq < 0) {
            logger.warn("Negative frequency detected, f_unscaled = " + freq);
        }
        VibrationalPartitionFunction q_vib = new VibrationalPartitionFunction();
        q_vib.setThermoAnalyzable(getThermoAnalyzable());
        q_vib.setFrequency(freq);
        return q_vib;
    }

    public List<PartitionFunction> getVibrationalPartitionFunction(List<Double> freq) throws Exception {
        List<PartitionFunction> q = new ArrayList<PartitionFunction>();
        for (int i = 0; i < freq.size(); i++) {
            if ((getThermoAnalyzable().getVibration(i).getFrequency() > 0)) {
                VibrationalPartitionFunction q_vib = new VibrationalPartitionFunction();
                q_vib.setThermoAnalyzable(getThermoAnalyzable());
                q.add(getVibrationalPartitionFunction(freq.get(i)));
            } else if (negativeFrequencyCheckRequested()) {
                throw new Exception("Negative frequency detected, f_unscaled = " + freq);
            } else {
                logger.warn("Negative frequency detected, f_unscaled = " + freq);
            }
        }
        return q;
    }

    public PartitionFunction getFreeRotorPartitionFunction(Bond rBond, double redMOI, int symmetry) {
        FreeRotorPartitionFunction q_free = new FreeRotorPartitionFunction();
        q_free.setThermoAnalyzable(getThermoAnalyzable());
        q_free.setBarrierEnergy(rBond.getTorsionBarrierInkcal_Per_mol());
        q_free.setReducedMoment(redMOI, symmetry);
        return q_free;
    }

    public double calculateH_0K() throws Exception {
        EnthalpyOfFormation enf = getThermoAnalyzable().getHf();
        if (enf != null) {
            // Try to bring the given enthalpy down to 0K
            if (enf.getTemperature() >= 200.0) {
                ThermoState thermo = getThermoState(enf.getTemperature());
                return enf.getValue() - thermo.delta_H;
            } else if (enf.getTemperature() == 0.0) {
                return enf.getValue();
            } else {
                logger.warn("Given enthalpy of formation is in the non-classical limit range (0 < T < 200). In this region, thermodynamic properties calculated from partition function are likely to be incorrect.");
                throw new RuntimeException("Enthalpy of formation must be at 0K or greater than 200K.");
            }
        }
        return 0.0;
    }
    
    @Override
    public ThermoState getThermoState(double T) throws Exception {
        if (T == 0.0) {
            throw new RuntimeException("Does not yet support Thermochemistry at 0 K");
        }
        if (recalculationNeeded()) {
            applyChangesToPartionFunction();
        }
        ThermoState thermo = new ThermoState();
        thermo.P = getPressure();
        for (int i = 0; i < getQ().size(); i++) {


            // needs to be adjusted!!!
            PartitionValues q_vals_unscaled = getQ().get(i).getPartitionValues(T);

            try {
                PartitionValues q_vals_C = getQ().get(i).getPartitionValues(T, (Double) getCScalingFactor().f(T));
                PartitionValues q_vals_H = getQ().get(i).getPartitionValues(T, (Double) getHScalingFactor().f(T));
                PartitionValues q_vals_S = getQ().get(i).getPartitionValues(T, (Double) getSScalingFactor().f(T));
                
                if (getCScalingFactor() != null) {
                    q_vals_C = getQ().get(i).getPartitionValues(T, (Double) getCScalingFactor().f(T));
                }
                
                if (getHScalingFactor() != null) {
                    q_vals_H = getQ().get(i).getPartitionValues(T, (Double) getHScalingFactor().f(T));
                }
                
                if (getSScalingFactor() != null) {
                    q_vals_S = getQ().get(i).getPartitionValues(T, (Double) getSScalingFactor().f(T));
                }

                thermo.S += Math.log(q_vals_S.q) + T / q_vals_S.q * q_vals_S.dqBydT;
                thermo.Cv += (2 * q_vals_C.dqBydT - T * q_vals_C.dqBydT * q_vals_C.dqBydT / q_vals_C.q + T * q_vals_C.d2qBydT2) / q_vals_C.q;
                thermo.delta_H += q_vals_H.dqBydT / q_vals_H.q;
                
            } catch (Exception e) {
                double q_i, dq_i, d2q_i;
                q_i = q_vals_unscaled.q;
                dq_i = q_vals_unscaled.dqBydT;
                d2q_i = q_vals_unscaled.d2qBydT2;
                thermo.S += Math.log(q_i) + T / q_i * dq_i;
                thermo.Cv += (2 * dq_i - T * dq_i * dq_i / q_i + T * d2q_i) / q_i;
                thermo.delta_H += dq_i / q_i;
            }
        }
        double NK_BT = PhysicalConstants.R * T;
        thermo.T = T;
        thermo.S = thermo.S * PhysicalConstants.R;
        thermo.Cv = thermo.Cv * NK_BT;
        thermo.Cp = thermo.Cv;
        if (hasTranslation()) {
            thermo.Cp += PhysicalConstants.R;
            thermo.S += PhysicalConstants.R;
        }
        thermo.delta_H = (thermo.delta_H * T + 1) * NK_BT;
        thermo.H = thermo.delta_H + getH_0K();
        thermo.G = thermo.H - T * thermo.S;
        return thermo;
    }

//    @Override
//    public ThermoState getThermoState(double T) {
//        if (T == 0.0) {
//            throw new RuntimeException("Does not yet support Thermochemistry at 0 K");
//        }
//        if (isPartitionFunctionChanged) {
//            applyChangesToPartionFunction();
//        }
//        ThermoState thermo = new ThermoState();
//        thermo.P = P;
//        for (int i = 0; i < Q.size(); i++) {
//            PartitionValues q_vals = Q.getFundamental(i).getPartitionValues(T);
//            double q_i, dq_i, d2q_i;
//            q_i = q_vals.q;
//            dq_i = q_vals.dqBydT;
//            d2q_i = q_vals.d2qBydT2;
//            thermo.S += Math.log(q_i) + T / q_i * dq_i;
//            thermo.Cv += (2 * dq_i - T * dq_i * dq_i / q_i + T * d2q_i) / q_i;
//            thermo.delta_H += dq_i / q_i;
//        }
//        double NK_BT = PhysicalConstants.R * T;
//        thermo.T = T;
//        thermo.S = thermo.S * PhysicalConstants.R;
//        thermo.Cv = thermo.Cv * NK_BT;
//        thermo.Cp = thermo.Cv;
//        if (hasTranslation) {
//            thermo.Cp += PhysicalConstants.R;
//            thermo.S += PhysicalConstants.R;
//        }
//        thermo.delta_H = (thermo.delta_H * T + 1) * NK_BT;
//        thermo.H = thermo.delta_H + H_0K;
//        thermo.G = thermo.H - T * thermo.S;
//
//        return thermo;
//    }
}