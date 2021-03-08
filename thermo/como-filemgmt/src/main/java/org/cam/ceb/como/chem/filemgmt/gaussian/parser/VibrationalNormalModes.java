/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author pb556
 */
public class VibrationalNormalModes extends ArrayList<VibrationalNormalMode> {

    protected String freqUnit = "";
    protected String redMassUnit = "";
    protected String irInternalUnit = "";
    protected String forceConstUnit = "";
    protected String displacementUnit = "";
    
    public void setUnits(String freqUnit, String redMassUnit, String irInternalUnit, String forceConstUnit, String displacementUnit) {
        this.freqUnit = freqUnit;
        this.redMassUnit = redMassUnit;
        this.irInternalUnit = irInternalUnit;
        this.forceConstUnit = forceConstUnit;
        this.displacementUnit = displacementUnit;
    }

    public void setFrequencyUnit(String unit) {
        freqUnit = unit;
    }

    public void setReducedMassUnit(String unit) {
        redMassUnit = unit;
    }

    public void setIRInternalUnit(String unit) {
        irInternalUnit = unit;
    }

    public void setForceConstantUnit(String unit) {
        forceConstUnit = unit;
    }
    
    public void setDisplacementUnit(String unit) {
        displacementUnit = unit;
    }

    public String getFrequencyUnit() {
        return freqUnit;
    }

    public String getReducedMassUnit() {
        return redMassUnit;
    }

    public String getIRInternalUnit() {
        return irInternalUnit;
    }

    public String getForceConstantUnit() {
        return forceConstUnit;
    }
    
    public String getDisplacementUnit() {
        return displacementUnit;
    }

    public void setUnit(Property p, String unit) {
        switch (p) {
            case FORCE_CONSTANT:
                forceConstUnit = unit;
                break;
            case FREQUENCY:
                freqUnit = unit;
                break;
            case IR_INTEN:
                irInternalUnit = unit;
                break;
            case REDUCED_MASS:
                redMassUnit = unit;
                break;
            case DISPLACEMENTS:
                displacementUnit = unit;
                break;
        }
    }
    
    public String getUnit(Property p) {
        switch (p) {
            case FORCE_CONSTANT:
                return forceConstUnit;
            case FREQUENCY:
                return freqUnit;
            case IR_INTEN:
                return irInternalUnit;
            case REDUCED_MASS:
                return redMassUnit;
            case DISPLACEMENTS:
                return displacementUnit;
        }
        return null;
    }

    public enum Property {

        FREQUENCY, REDUCED_MASS, FORCE_CONSTANT, IR_INTEN, MODE, REDUCED_MOMENTS, DISPLACEMENTS
    }

    public void add(Property p, int index, double... value) {
        if (value.length < 1) {
            return;
        }
        if (index >= this.size()) {
            VibrationalNormalMode normalMode = new VibrationalNormalMode();
            switch (p) {
                case FORCE_CONSTANT:
                    normalMode.frcConst = value[0];
                    break;
                case FREQUENCY:
                    normalMode.freq = value[0];
                    break;
                case IR_INTEN:
                    normalMode.irInten = value[0];
                    break;
                case MODE:
                    normalMode.mode = (int) value[0];
                    break;
                case REDUCED_MASS:
                    normalMode.redMass = value[0];
                    break;
                case DISPLACEMENTS:
                    if (value.length == 3) {
                        VibrationalNormalMode.XYZ xyz = normalMode.new XYZ();
                        xyz.setX(value[0]);
                        xyz.setY(value[1]);
                        xyz.setZ(value[2]);
                        normalMode.pos.add(xyz);
                    } else {
                        // logger has to be added
                        System.out.println("Parsing error during the extraction of the displacement values.");
                    }
            }
            this.add(normalMode);
        } else {
            switch (p) {
                case FORCE_CONSTANT:
                    this.get(index).frcConst = value[0];
                    break;
                case FREQUENCY:
                    this.get(index).freq = value[0];
                    break;
                case IR_INTEN:
                    this.get(index).irInten = value[0];
                    break;
                case MODE:
                    this.get(index).mode = (int) value[0];
                    break;
                case REDUCED_MASS:
                    this.get(index).redMass = value[0];
                    break;
                case DISPLACEMENTS:
                    if (value.length == 3) {
                        //VibrationalNormalMode.XYZ xyz = normalMode.new XYZ();
                        //xyz.x = value[0];
                        //xyz.y = value[1];
                        //xyz.z = value[2];
                        //this.normalModes.get(index).pos.get(index)
                    } else {
                        // logger has to be added
                        System.out.println("Parsing error during the extraction of the displacement values.");
                    }
            }
        }
    }

    public void addAll(Property p, Collection<Double> values) {
        //if (this.normalModes.size() <= values.size()) {
        int i = this.size();
        for (Double val : values) {
            this.add(p, i, val);
            i++;
        }
//        } else {
//            // do not do anything
//        }
    }

    public void addAll(Property p, int index, Collection<Double> values) {
        int i = index;
        for (Double val : values) {
            this.add(p, i, val);
            i++;
        }
    }

    public List<Double> get(Property p) {
        List<Double> values = new ArrayList<Double>();
        for (int i = 0; i < this.size(); i++) {
            values.add(this.get(p, i));
        }
        return values;
    }

    public double get(Property p, int index) {
        switch (p) {
            case FORCE_CONSTANT:
                return this.get(index).frcConst;
            case FREQUENCY:
                return this.get(index).freq;
            case IR_INTEN:
                return this.get(index).irInten;
            case MODE:
                return this.get(index).mode;
            case REDUCED_MASS:
                return this.get(index).redMass;
        }
        return 0;
    }
}