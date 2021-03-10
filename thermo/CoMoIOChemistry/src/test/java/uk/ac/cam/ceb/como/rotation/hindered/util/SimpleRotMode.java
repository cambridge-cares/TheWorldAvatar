/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.util;

import java.util.Collection;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalMode;
import org.xmlcml.cml.element.CMLAtom;

/**
 *
 * @author pb556
 */
public class SimpleRotMode {
    
    protected SimpleVibNormalMode vibMode;
    protected int multiplicity;
    protected int periodicity;
    protected int symmetry;
    protected double freq;
    protected double redMoment;
    protected Collection<SimpleAtom> composition;
    
    public SimpleRotMode(double freq, double redMoment, 
            int multiplicity, int periodicity, int symmetry, 
            SimpleVibNormalMode vibMode, Collection<SimpleAtom> composition) {
        this.freq = freq;
        this.redMoment = redMoment;
        this.multiplicity = multiplicity;
        this.periodicity = periodicity;
        this.symmetry = symmetry;
        this.vibMode = vibMode;
        this.composition = composition;
    }
    
    public double getFrequency() {
        return freq;
    }
    
    public double getReducedMoment() {
        return redMoment;
    }
    
    public int getMultiplicity() {
        return multiplicity;
    }
    
    public int getPeriodicity() {
        return periodicity;
    }
    
    public int getSymmetryNumber() {
        return symmetry;
    }
    
    public SimpleVibNormalMode getVibrationalNormalMode() {
        return vibMode;
    }
    
    public Collection<SimpleAtom> getComposition() {
        return composition;
    }
    
    public void setFrequency(double freq) {
        this.freq = freq;
    }
    
    public void setReducedMoment(double redMoment) {
        this.redMoment = redMoment;
    }
    
    public void setMultiplicity(int multiplicity) {
        this.multiplicity = multiplicity;
    }
    
    public void setPeriodicity(int periodicity) {
        this.periodicity = periodicity;
    }
    
    public void setSymmetryNumber(int symmetry) {
        this.symmetry = symmetry;
    }
    
    public void setVibrationalNormalMode(SimpleVibNormalMode vibMode) {
        this.vibMode = vibMode;
    }
    
    public void setComposition(Collection<SimpleAtom> composition) {
        this.composition = composition;
    }
    
    public boolean equals(SimpleRotMode mode) {
        double tolerance = 0.01;
        boolean equal = Math.abs(freq - mode.getFrequency()) < tolerance &&
                Math.abs(redMoment - mode.getReducedMoment()) < tolerance &&
                multiplicity == mode.getMultiplicity() &&
                periodicity == mode.getPeriodicity() &&
                symmetry == mode.getSymmetryNumber() &&
                vibMode.equals(mode.getVibrationalNormalMode()) &&
                composition.size() == mode.getComposition().size();
        for (SimpleAtom aRef : composition) {
            boolean identified = false;
            for (SimpleAtom a : mode.getComposition()) {
                if (aRef.equals(a)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                equal = false;
                break;
            }
        }
        return equal;
    }
    
    public boolean equals(RotationalMode mode) {
        double tolerance = 0.01;
        boolean equal = Math.abs(freq - mode.getFrequency()) < tolerance &&
                Math.abs(redMoment - mode.getReducedMoment()) < tolerance &&
                multiplicity == mode.getMultiplicity() &&
                periodicity == mode.getPeriodicity() &&
                symmetry == mode.getSymmetryNumber() &&
                vibMode.equals(mode.getVibrationalNormalMode()) &&
                composition.size() == mode.getTop().getAtoms().size();
        for (SimpleAtom aRef : composition) {
            boolean identified = false;
            for (CMLAtom a : mode.getTop().getAtoms()) {
                if (aRef.equals(a)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                equal = false;
                break;
            }
        }
        return equal;
    }
}
