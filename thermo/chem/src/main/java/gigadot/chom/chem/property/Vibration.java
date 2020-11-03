package gigadot.chom.chem.property;

import gigadot.chom.chem.structure.Bond;
import java.util.ArrayList;
import java.util.List;


/**
 *
 * @author Weerapong Phadungsukanan
 */
public class Vibration {
    public List<Displacement> DisplacementList = new ArrayList<Displacement>();
    public double Frequency = 0.0;
    public double ReducedMass = 0.0;
    public double ForceConstant = 0.0;
    public double IRIntensity = 0.0;

    private List<Bond> internalRotorAxis = new ArrayList<Bond>();

    public void addIRotorBond(Bond bond_axis) {
        if (!hasIRotorBond(bond_axis)) {
            internalRotorAxis.add(bond_axis);
        }
    }

    public List<Bond> clonedRotorBonds() {
        return new ArrayList<Bond>(internalRotorAxis);
    }

    public Bond getIRotorBond(int index) {
        return internalRotorAxis.get(index);
    }

    public int getIRotorBondCount() {
        return internalRotorAxis.size();
    }

    public void removeAllIRotorBond() {
        internalRotorAxis.clear();
    }

    public void removeIRotorBond(Bond bond_axis) {
        for (int i = 0; i < internalRotorAxis.size(); i++) {
            if (internalRotorAxis.get(i).equals(bond_axis))
                internalRotorAxis.remove(i);
        }
    }

    public boolean hasIRotorBond(Bond bond_axis) {
        for (int i = 0; i < internalRotorAxis.size(); i++) {
            if (internalRotorAxis.get(i).equals(bond_axis)) return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return "Freq : " + Frequency + " [cm^-1]";
    }
 }
