/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author pb556
 */
public class VibrationalNormalMode {
    
    protected double freq = -1;
    protected double redMass = -1;
    protected double frcConst = -1;
    protected double irInten = -1;
    protected int mode = -1;
    protected List<Integer> atomNumber = null;
    protected List<XYZ> pos = null;
    
    public class XYZ {
        private double x;
        private double y;
        private double z;
        private String atomId;
        
        public void setX(double x) {
            this.x = x;
        }
        
        public void setY(double y) {
            this.y = y;
        }
        
        public void setZ(double z) {
            this.z = z;
        }
        
        public void setAtomId(String id) {
            atomId = id;
        }
        
        public double getX() {
            return x;
        }
        
        public double getY() {
            return y;
        }
        
        public double getZ() {
            return z;
        }
        
        public String getAtomId() {
            return atomId;
        }
    }

    protected class FreqMode {
        final List<XYZ> coord = new ArrayList<XYZ>();
    }
    
    public double getFrequency() {
        return freq;
    }
    
    public double getReducedMass() {
        return redMass;
    }
    
    public double getForceConstant() {
        return frcConst;
    }
    
    public double getIRInten() {
        return irInten;
    }
    
    public int getMode() {
        return mode;
    }
    
    public List<Integer> getAtomNumber() {
        return atomNumber;
    }
    
    public List<XYZ> getDisplacement() {
        return pos;
    }
    
    public void setFrequency(double freq) {
        this.freq = freq;
    }
    
    public void setReducedMass(double redMass) {
        this.redMass = redMass;
    }
    
    public void setForceConstant(double frcConst) {
        this.frcConst = frcConst;
    }
    
    public void setIRInten(double irInten) {
        this.irInten = irInten;
    }
    
    public void setMode(int mode) {
        this.mode = mode;
    }
    
    public void setAtomNumber(List<Integer> num) {
        this.atomNumber = num;
    }
    
    public void setDisplacement(List<XYZ> pos) {
        this.pos = pos;
    }
    
    public boolean isSimilar(VibrationalNormalMode cmp) {
        boolean equal = true;
        if (cmp != null) {
            equal &= getFrequency() == cmp.getFrequency();
            equal &= getForceConstant()== cmp.getForceConstant();
            equal &= getIRInten()== cmp.getIRInten();
            equal &= getReducedMass()== cmp.getReducedMass();
            equal &= getMode() == cmp.getMode();
            //equal &= getAtomNumber().size() == cmp.getAtomNumber().size();
            //equal &= getDisplacement().size() == cmp.getDisplacement().size();
            // check displacement
//            for (Integer atomNumberA : getAtomNumber()) {
//                int indexA = getAtomNumber().indexOf(atomNumberA);
//                boolean identified = false;
//                for (Integer atomNumberB : cmp.getAtomNumber()) {
//                    if (atomNumberA == atomNumberB) {
//                        int indexB = cmp.getAtomNumber().indexOf(atomNumberB);
//                        identified = getDisplacement().get(indexA).getAtomId().compareToIgnoreCase(cmp.getDisplacement().get(indexB).getAtomId()) == 0
//                                && getDisplacement().get(indexA).getX() == cmp.getDisplacement().get(indexB).getX()
//                                && getDisplacement().get(indexA).getY() == cmp.getDisplacement().get(indexB).getY()
//                                && getDisplacement().get(indexA).getZ() == cmp.getDisplacement().get(indexB).getZ();
//                    }
//                }
//                if (!identified) {
//                    return false;
//                }
//            }
            return equal;
        }
        return false;
    }
}
