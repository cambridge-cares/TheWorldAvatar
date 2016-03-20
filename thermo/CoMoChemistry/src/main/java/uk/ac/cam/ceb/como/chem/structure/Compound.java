package uk.ac.cam.ceb.como.chem.structure;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;

/**
 * compounds represent several molecules
 * @author pb556
 */
public class Compound extends CompoundDelegator {

    private Logger logger = Logger.getLogger(getClass());
    /**
     * An auxiliary data attached to the object.
     */
    private Map<String, Object> UserDataMap = new HashMap<String, Object>();
    /**
     * A list of Molecule object.
     */
    private List<Molecule> MoleculeList = new ArrayList<Molecule>();
    public static final String OBMol_KEY = "OBMol";
    public static final String CMLData_KEY = "CMLData";

    public Map<String, Object> getUserDataMap() {
        return UserDataMap;
    }

    @Override
    public void clear() {
        super.clear();
        MoleculeList.clear();
        UserDataMap.clear();
    }

    public Molecule getMolecule(int index) {
        return MoleculeList.get(index);
    }

    public int getMoleculeCount() {
        return MoleculeList.size();
    }

    public void sortByMolecularSize() throws Exception {
        if (MoleculeList != null) {
            if (MoleculeList.size() < 2) {
                // do nothing
            } else if (MoleculeList.size() == 2) {
                if (MoleculeList.get(0).getAtomCount() > MoleculeList.get(1).getAtomCount()) {
                    Collections.swap(MoleculeList, 0, 1);
                }
            } else {
                throw SortingError();
            }
        }
    }

    /**
     * Recreated a list of molecules. A molecule is defined by a connected graph
     * where Atom is vertex and Bond is edge of the graph. This function uses all
     * the Atom coordinates and all the Bond currently exists to recreate the
     * molecule list.
     */
    public void recreateMoleculeList() {
        MoleculeList = createMoleculeListByConnectivity(clonedBondList());
        List<Atom> atomsInMoleculeList = new ArrayList<Atom>();
        for (Molecule molecule : MoleculeList) {
            for (int i = 0; i < molecule.getAtomCount(); i++) {
                atomsInMoleculeList.add(molecule.getAtom(i));
            }
        }

        List<Atom> atomsInCompound = new ArrayList<Atom>();
        for (int i = 0; i < getAtomCount(); i++) {
            atomsInCompound.add(getAtom(i));
        }

        atomsInCompound.removeAll(atomsInMoleculeList);

        // atomsInCompound now contains only disconnected atoms so we will create molecule for each of these atoms.
        for (Atom atom : atomsInCompound) {
            Molecule molecule = new Molecule();
            molecule.addAtom(atom);
            MoleculeList.add(molecule);
        }
    }

    /**
     * This method create a list of molecules from a given set of connectivities.
     *
     * Caution: even if the given bond list is all the bonds of compound, it is not necessarily that all the atoms
     * in compound will be in the list of returning molecule. The example is that if there is a single unconnected atom
     * , it will be left out from this returning molecule list. So the method recreateMoleculeList should be used if
     * it is for the compound itself.
     *
     * @param bondList
     * @return
     *
     */
    public List<Molecule> createMoleculeListByConnectivity(List<Bond> bondList) {
        List<Molecule> molList = new ArrayList<Molecule>();
        List<Bond> remainingBonds = new ArrayList<Bond>(bondList);

        boolean hasToCreateNewMolecule = true;
        // Repeat until remaining bonds is none
        while (remainingBonds.size() > 0) {
            // Make a bond list for iternation
            List<Bond> bonds = new ArrayList<Bond>(remainingBonds);
            // Clear the remaining bond list
            remainingBonds.clear();
            for (int i = 0; i < bonds.size(); i++) {
                boolean isBondAdded = false;

                if (hasToCreateNewMolecule) {
                    // Create new molecule
                    Molecule mol = new Molecule();
                    mol.addBond(bonds.get(i));
                    molList.add(mol);
                    isBondAdded = true;
                    hasToCreateNewMolecule = false;
                } else {
                    // Trying to add bonds from bonds list to existing molecule
                    for (int j = 0; j < molList.size(); j++) {
                        Atom atomA = molList.get(j).findAtom(bonds.get(i).getAtomA());
                        Atom atomB = molList.get(j).findAtom(bonds.get(i).getAtomB());
                        if ((atomA != null) || (atomB != null)) {
                            molList.get(j).addBond(bonds.get(i));
                            isBondAdded = true;
                            break;
                        }
                    }
                }

                // If a bond is not added then put it to the remaining bond list
                if (!isBondAdded) {
                    remainingBonds.add(bonds.get(i));
                }
            }

            // If the numbers of remaining bonds and bonds lists are the same then
            // there is no bond added to any existing molecule, therefore, next time
            // it needs to create a new molecule with the first bond in the remaining
            // bond list.
            if (remainingBonds.size() == bonds.size()) {
                // No bond has been removed so a molecule must be created now
                hasToCreateNewMolecule = true;
            }
        }
        return molList;
    }

    private Exception SortingError() {
        return new Exception("Number of molecules is more than 2. Sorting has yet supported.");
    }
}
