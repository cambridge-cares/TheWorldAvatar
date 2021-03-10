package gigadot.chom.chem.structure;

import gigadot.chom.model.brownie.Atom;
import gigadot.chom.model.chemistry.Element;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import gigadot.chom.chem.structure.tool.CompoundConverter;
import gigadot.chom.compchem.math.ArrayMathUtils;
import gigatools.lite.constant.PhysicalConstants;
import java.util.Collections;
import java.util.Comparator;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 * compounds represent several molecules
 * @author Weerapong Phadungsukanan
 */
public class StructureImpl implements Structure {

    /**
     * A list of Atom objects.
     */
    private List<Atom> AtomList = new ArrayList<Atom>();
    /**
     * A list of Bond objects.
     */
    private List<Bond> BondList = new ArrayList<Bond>();
    // === delegated methods are not tested ===
    @Override
    public void clear() {
        BondList.clear();
        AtomList.clear();
    }

    @Override
    public Atom getAtom(int index) {
        return AtomList.get(index);
    }

    @Override
    public int getAtomCount() {
        return AtomList.size();
    }

    @Override
    public Bond getBond(int index) {
        return BondList.get(index);
    }

    @Override
    public int getBondCount() {
        return BondList.size();
    }

    /**
     * Molecular weigth relative to 1/12 of C12.
     * @return
     */
    @Override
    public double getWeigth() {
        double mol_weight = 0.0;
        for (Atom atom : AtomList) {
            mol_weight += atom.getElement().atomicWeight;
        }
        return mol_weight;
    }

    @Override
    public double[] getCentreOfMassInA() {
        return ArrayMathUtils.copyAndScale(getCentreOfMass(), 1d / PhysicalConstants.A);
    }

    @Override
    public double[] getCentreOfMass() {
        double[] CM = {0d, 0d, 0d};
        for (Atom atom : AtomList) {
            Element e = atom.getElement();
            CM[0] += e.atomicWeight * atom.getX();
            CM[1] += e.atomicWeight * atom.getY();
            CM[2] += e.atomicWeight * atom.getZ();
        }
        ArrayMathUtils.copyAndScale(CM, CM, 1d / getWeigth());
        return CM;
    }

    @Override
    public Atom getAtomByOrder(int atom_order) {
        for (int i = 0; i < AtomList.size(); i++) {
            if (AtomList.get(i).getOrder() == atom_order) {
                return AtomList.get(i);
            }
        }
        return null;
    }

    @Override
    public Bond findBond(Bond b) {
        if (BondList == null) {
            return null;
        }
        for (int i = 0; i < BondList.size(); i++) {
            if (BondList.get(i).equals(b)) {
                return BondList.get(i);
            }
        }
        return null;
    }

    @Override
    public Bond findSimilarBond(Bond b) {
        if (BondList == null) {
            return null;
        }
        for (int i = 0; i < BondList.size(); i++) {
            if (BondList.get(i).isSimilar(b)) {
                return BondList.get(i);
            }
        }
        return null;
    }

    @Override
    public boolean hasBond(Bond b) {
        if (findBond(b) != null) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean removeBond(Bond b) {
        if (BondList == null) {
            return false;
        }
        for (int i = 0; i < BondList.size(); i++) {
            if (BondList.get(i).equals(b)) {
                return BondList.remove(BondList.get(i));
            }
        }
        return false;
    }

    /**
     * Add atom to Structure, atom order is automatically set to
     * a unique id. If an adding atom has the same coordinate and
     * same element it will not add.
     * @param atom
     * @return
     */
    @Override
    public Atom addAtom(Atom atom) {
        if (atom != null) {
            Atom fatom = findAtom(atom);
            if (fatom != null) {
                return fatom;
            } else {
                atom.setOrder(getNextAtomOrder());
                AtomList.add(atom);
                return atom;
            }
        }
        return null;
    }

    @Override
    public Bond addBond(Bond bond) {
        if (bond != null) {
            Bond fbond = findBond(bond);
            if (fbond != null) {
                return fbond;
            } else {
                addAtom(bond.getAtomA());
                addAtom(bond.getAtomB());
                BondList.add(bond);
                return bond;
            }
        }
        return null;
    }

    private int getNextAtomOrder() {
        int largest_order = AtomList.size() + 1;
        for (int i = 0; i < AtomList.size(); i++) {
            if (AtomList.get(i).getOrder() > largest_order) {
                largest_order = AtomList.get(i).getOrder();
            }
        }
        return largest_order;
    }

    @Override
    public List<Bond> clonedBondList() {
        return new ArrayList<Bond>(BondList);
    }

    @Override
    public Atom findAtom(Atom atom) {
        if (AtomList == null) {
            return null;
        }
        for (int i = 0; i < AtomList.size(); i++) {
            if (AtomList.get(i).equalsIgnoreOrder(atom)) {
                return AtomList.get(i);
            }
        }
        return null;
    }

    @Override
    public boolean hasAtom(Atom atom) {
        if (AtomList == null) {
            return false;
        }
        for (int i = 0; i < AtomList.size(); i++) {
            if (AtomList.get(i).equalsIgnoreOrder(atom)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void calculateBonds() {
        CMLMolecule cmlMol = CompoundConverter.convertToCMLMolecule(this);
        List<CMLBond> cmlbonds = cmlMol.getBonds();
        List<CMLAtom> cmlatoms = cmlMol.getAtoms();

        BondList.clear();
        Map<String, Integer> atomMap = new HashMap<String, Integer>(cmlatoms.size());
        for (int i = 0; i < cmlatoms.size(); i++) {
            atomMap.put(cmlatoms.get(i).getId(), i + 1);
        }
        for (int i = 0; i < cmlbonds.size(); i++) {
            CMLBond b = cmlbonds.get(i);

            Bond newBond = Bond.createBond(
                    getAtomByOrder(atomMap.get(b.getAtom(0).getId())), // AtomA
                    getAtomByOrder(atomMap.get(b.getAtom(1).getId())), // AtomB
                    BondType.parseCMLBondType(b.getOrder()),
                    this);
        }
    }

}
