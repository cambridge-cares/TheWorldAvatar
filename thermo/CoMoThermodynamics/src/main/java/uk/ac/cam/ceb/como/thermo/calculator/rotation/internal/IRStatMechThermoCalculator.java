/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rotation.internal;

import java.util.Map;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.thermo.calculator.StatMechThermoCalculator;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule.RotorRule;

/**
 *
 * @author pb556
 */
public abstract class IRStatMechThermoCalculator extends StatMechThermoCalculator {
    
    private Map<String, Function> potApprox = null;
    private RotorRule irRule = null;
    private boolean hasInternalRotor = true;
    
    public IRStatMechThermoCalculator() {
    }

    public void setUseInternalRotor(boolean hasInternalRotor) {
        this.hasInternalRotor = hasInternalRotor;
    }

    public void setInternalRotorRule(RotorRule rule, Map<String, Function> potApprox) {
        irRule = rule;
        this.potApprox = potApprox;
        recalculation(true);
    }
    
    public boolean hasInternalRotation() {
        return hasInternalRotor;
    }
    
    public RotorRule getInternalRotorRule() {
        return irRule;
    }

    public void setContributions(boolean hasTranslation, boolean hasRRHO, boolean hasVibration, boolean hasElectronic, boolean hasInternalRotor) {
        setContributions(hasTranslation, hasRRHO, hasVibration, hasElectronic);
        this.hasInternalRotor = hasInternalRotor;
        recalculation(true);
    }
    
    public Map<String, Function> getHindrancePotential() {
        return potApprox;
    }
    
//    public Function getHindrancePotential(Bond bond) {
//        for (Bond b : potApprox.keySet()) {
//            String id1 = b.toString();
//            String id2 = bond.toString();
//            if (id1.compareToIgnoreCase(id2) == 0) {
//                return potApprox.get(b);
//            }
//        }
//        return null;
//    }
    
    public Function getHindrancePotential(Bond bond) {
        for (String id : potApprox.keySet()) {
            if (id.compareToIgnoreCase(bond.getAtomA().getId() + "-" + bond.getAtomB().getId()) == 0
                || id.compareToIgnoreCase(bond.getAtomB().getId() + "-" + bond.getAtomA().getId()) == 0) {
                //Object retVal = potApprox.get(id);
                return potApprox.get(id);
            }
        }
        return null;
    }
}
