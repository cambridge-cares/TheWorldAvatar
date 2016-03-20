package uk.ac.cam.ceb.como.chem.structure;

import java.util.List;

/**
 *
 * @author pb556
 */
public interface Structure {

    /**
     * Add atom to JChemDocument, atom order is automatically set to give unique id.
     * @param atom
     * @return
     */
    Atom addAtom(Atom atom);

    Bond addBond(Bond bond);

    void calculateBonds();

    void clear();

    Atom findAtom(Atom atom);

    Bond findBond(Bond b);

    Bond findSimilarBond(Bond b);

    Atom getAtom(int index);

    int getAtomCount();

    Atom getAtomByOrder(int atom_order);

    Bond getBond(int index);

    int getBondCount();

    double[] getCentreOfMass();

    double[] getCentreOfMassInA();

    List<Bond> clonedBondList();

    /**
     * Molecular weigth relative to 1/12 of C12.
     * @return
     */
    double getWeigth();

    boolean hasAtom(Atom atom);

    boolean hasBond(Bond b);

    boolean removeBond(Bond b);
}
