package uk.ac.cam.ceb.como.chem.property;


/**
 *
 * @author pb556
 */
public class Vibration {
    
    protected DisplacementList DisplacementList = new DisplacementList();
    
    protected double freq = 0.0;
    protected double redMass = 0.0;
    protected double frcConst = 0.0;
    protected double irInten = 0.0;
    protected int mode = 0;

    // protected List<Bond> internalRotorAxis = new ArrayList<Bond>();
    
    public DisplacementList getDisplacements() {
        return DisplacementList;
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
    
    public void setDisplacements(DisplacementList list) {
        this.DisplacementList = list;
    }
    
//    public void addIRotorBond(Bond bond_axis) {
//        if (!hasIRotorBond(bond_axis)) {
//            internalRotorAxis.add(bond_axis);
//        }
//    }
//
//    public List<Bond> clonedRotorBonds() {
//        return new ArrayList<Bond>(internalRotorAxis);
//    }
//
//    public Bond getIRotorBond(int index) {
//        return internalRotorAxis.get(index);
//    }
//
//    public int getIRotorBondCount() {
//        return internalRotorAxis.size();
//    }
//
//    public void removeAllIRotorBond() {
//        internalRotorAxis.clear();
//    }
//
//    public void removeIRotorBond(Bond bond_axis) {
//        for (int i = 0; i < internalRotorAxis.size(); i++) {
//            if (internalRotorAxis.get(i).equals(bond_axis))
//                internalRotorAxis.remove(i);
//        }
//    }
//
//    public boolean hasIRotorBond(Bond bond_axis) {
//        for (int i = 0; i < internalRotorAxis.size(); i++) {
//            if (internalRotorAxis.get(i).equals(bond_axis)) return true;
//        }
//        return false;
//    }

    @Override
    public String toString() {
        return "Freq : " + freq + " [cm^-1]";
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        Vibration clone = new Vibration();
        clone.setForceConstant(getForceConstant());
        clone.setFrequency(getFrequency());
        clone.setIRInten(getIRInten());
        clone.setMode(getMode());
        clone.setReducedMass(getReducedMass());
        clone.setDisplacements((DisplacementList) getDisplacements().clone());
        return clone;
    }
    
    public boolean isSimilar(Vibration cmp) {
        boolean equal = true;
        if (cmp != null) {
            equal &= getFrequency() == cmp.getFrequency();
            equal &= getForceConstant()== cmp.getForceConstant();
            equal &= getIRInten()== cmp.getIRInten();
            equal &= getReducedMass()== cmp.getReducedMass();
            equal &= getMode() == cmp.getMode();
            return equal;
        }
        return false;
    }
 }
