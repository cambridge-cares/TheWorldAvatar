/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.ethanol;

import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.chem.property.EnthalpyOfFormation;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import java.io.File;
import java.util.HashMap;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule.RotorRule;
import uk.ac.cam.ceb.como.thermo.property.ThermoState;
import org.junit.Test;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.chem.property.Rotation;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.math.function.simple.UniformFunction;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
import uk.ac.cam.ceb.como.thermo.calculator.rrho.RRHOThermoCalculator;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule.PitzerReducedMOIFunction;

/**
 *
 * @author pb556
 */
public class EthanolThermoCalc {

    @Test
    public void calc() throws Exception {
        String ref = "W:\\Data\\TTIPMech\\thermo\\revised\\ethanol\\64-17-5-ethanol";

        FrequencyParser parser = new FrequencyParser();
        parser.set(new File(ref));
        parser.parse();
        CompChem cc = (CompChem) parser.get();
        IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
        CMLMolecule mol = ccw.getFinalMolecule();
        Compound compound = CompoundConverter.convert(mol);
        
        Bond[] rotors = new Bond[2];
        for (int i = 0; i < compound.getBondCount(); i++) {
             Bond b = compound.getBond(i);
             if (b.getAtomA().getElement().getSymbol().compareToIgnoreCase("C") == 0
                     && b.getAtomB().getElement().getSymbol().compareToIgnoreCase("C") == 0) {
                 rotors[0] = b;
             }
             if (b.getAtomA().getElement().getSymbol().compareToIgnoreCase("C") == 0
                     && b.getAtomB().getElement().getSymbol().compareToIgnoreCase("O") == 0) {
                 rotors[1] = b;
             }
             if (b.getAtomB().getElement().getSymbol().compareToIgnoreCase("C") == 0
                     && b.getAtomA().getElement().getSymbol().compareToIgnoreCase("O") == 0) {
                 rotors[1] = b;
             }
        }

        // add frequencies to the compound
        Vibrations modes = ccw.getVibrationalNormalModes();
        HashMap<Bond, Integer> r = new HashMap<Bond, Integer>();
        for (Object obj : modes) {

            Vibration mode = (Vibration) obj;
            if (mode.getFrequency() == 233.7508) {
                Rotation rot = new Rotation();
                rot.setTorsionalBond(rotors[0]);
                rot.setSymmetryNumber(3);
                rot.setDisplacements(mode.getDisplacements());
                rot.setForceConstant(mode.getForceConstant());
                rot.setFrequency(mode.getFrequency());
                rot.setIRInten(mode.getIRInten());
                rot.setMode(mode.getMode());
                rot.setReducedMass(mode.getReducedMass());
                r.put(rotors[0], 3);
                compound.addVibration(rot);
            } else if (mode.getFrequency() == 271.1639) {
                Rotation rot = new Rotation();
                rot.setTorsionalBond(rotors[1]);
                rot.setSymmetryNumber(1);
                rot.setDisplacements(mode.getDisplacements());
                rot.setForceConstant(mode.getForceConstant());
                rot.setFrequency(mode.getFrequency());
                rot.setIRInten(mode.getIRInten());
                rot.setMode(mode.getMode());
                rot.setReducedMass(mode.getReducedMass());
                compound.addVibration(rot);
                r.put(rotors[1], 1);
            } else {
                compound.addVibration(mode);
            }
        }

        compound.setHf(new EnthalpyOfFormation(-234.0 * 1000));

        RRHOThermoCalculator calculatorRRHO = new RRHOThermoCalculator();
        calculatorRRHO.setUseInternalRotor(true);
        
//      RotorRule rule = new PitzerReducedMOIFunction(compound, , r.keySet(), true);
//      rule.setInternalRotorType(RotorRule.InternalRotorType.NONE);
//      calculatorRRHO.setInternalRotorRule(rule, null);
        
        calculatorRRHO.setCMLMolecule(mol);
        calculatorRRHO.setCheckForNegativeFrequecy(true);
        calculatorRRHO.setContributions(true, true, true, true);
        calculatorRRHO.setPressure(PhysicalConstants.P_1atm);
        calculatorRRHO.setCScalingFactor(new UniformFunction(0.9684));
        calculatorRRHO.setHScalingFactor(new UniformFunction(0.9684));
        calculatorRRHO.setSScalingFactor(new UniformFunction(0.9684));
        calculatorRRHO.setZPEScalingFactor(new UniformFunction(0.9684));
        calculatorRRHO.setRotFreqScalingFactor(new UniformFunction(0.9684));
        calculatorRRHO.setThermoAnalyzable(compound);
        calculatorRRHO.recalculationNeeded();

        ThermoState s = calculatorRRHO.getThermoState(3000);
        System.out.println(s.S);
    }
}