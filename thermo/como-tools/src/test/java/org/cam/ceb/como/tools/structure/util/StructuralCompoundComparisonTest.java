/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util;

import org.cam.ceb.como.tools.structure.util.StructuralCompoundComparison;
import static org.cam.ceb.como.tools.structure.util.StructuralCompoundComparison.isEqual;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.model.brownie.Atom;
import org.junit.Test;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;

/**
 *
 * @author pb556
 */
public class StructuralCompoundComparisonTest {

    @Test
    public void isEqualMolecules() {
        // need CMLMolecules
        // need Compound molecules
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), MockCMLMolecules.getC2H4()));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), MockCMLMolecules.getC2H6O()));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), MockCMLMolecules.getC2H4()));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), MockCMLMolecules.getC2H6O()));

        assert (StructuralCompoundComparison.isEqual(MockCMLMolecules.getC2H4(), MockCMLMolecules.getC2H4()));
        assert (StructuralCompoundComparison.isEqual(MockCMLMolecules.getC2H6O(), MockCMLMolecules.getC2H6O()));
        assert (!StructuralCompoundComparison.isEqual(MockCMLMolecules.getC2H4(), MockCMLMolecules.getC2H6O()));
        assert (!StructuralCompoundComparison.isEqual(MockCMLMolecules.getC2H6O(), MockCMLMolecules.getC2H4()));

        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), MockCompounds.getC2H4()));
        assert (StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), MockCompounds.getC2H6O()));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H4(), MockCompounds.getC2H6O()));
        assert (!StructuralCompoundComparison.isEqual(MockCompounds.getC2H6O(), MockCompounds.getC2H4()));
    }

    @Test
    public void isEqualAtoms() {
        CMLAtom c1CMLAtom = new CMLAtom("C1", new ChemicalElement("C", 6, 12.0107));
        c1CMLAtom.setXYZ3(new Point3(0.5971, 0.9317, 0.0938));
        CMLAtom c2CMLAtom = new CMLAtom("C2", new ChemicalElement("C", 6, 12.0107));
        c2CMLAtom.setXYZ3(new Point3(1.9285, 0.9160, 0.0536));

        Atom c1Atom = new Atom("C");
        c1Atom.setCoordinateInA(0.5971, 0.9317, 0.0938);
        Atom c2Atom = new Atom("C");
        c2Atom.setCoordinateInA(1.9285, 0.9160, 0.0536);

        assert (StructuralCompoundComparison.isEqual(c1Atom, c1CMLAtom));
        assert (StructuralCompoundComparison.isEqual(c2Atom, c2CMLAtom));
        assert (!StructuralCompoundComparison.isEqual(c2Atom, c1CMLAtom));
        assert (!StructuralCompoundComparison.isEqual(c1Atom, c2CMLAtom));

        assert (StructuralCompoundComparison.isEqual(c1Atom, c1Atom));
        assert (StructuralCompoundComparison.isEqual(c2Atom, c2Atom));
        assert (!StructuralCompoundComparison.isEqual(c2Atom, c1Atom));
        assert (!StructuralCompoundComparison.isEqual(c1Atom, c2Atom));

        assert (StructuralCompoundComparison.isEqual(c1CMLAtom, c1CMLAtom));
        assert (StructuralCompoundComparison.isEqual(c2CMLAtom, c2CMLAtom));
        assert (!StructuralCompoundComparison.isEqual(c2CMLAtom, c1CMLAtom));
        assert (!StructuralCompoundComparison.isEqual(c1CMLAtom, c2CMLAtom));
    }

    @Test
    public void isEqualBonds() {
        // check the bonds list
        assert (equalBonds(MockCompounds.getC2H4(), MockCMLMolecules.getC2H4()));
        assert (equalBonds(MockCompounds.getC2H6O(), MockCMLMolecules.getC2H6O()));
        assert (!equalBonds(MockCompounds.getC2H6O(), MockCMLMolecules.getC2H4()));
        assert (!equalBonds(MockCompounds.getC2H4(), MockCMLMolecules.getC2H6O()));

        assert (equalBonds(MockCMLMolecules.getC2H4(), MockCMLMolecules.getC2H4()));
        assert (equalBonds(MockCMLMolecules.getC2H6O(), MockCMLMolecules.getC2H6O()));
        assert (!equalBonds(MockCMLMolecules.getC2H4(), MockCMLMolecules.getC2H6O()));
        assert (!equalBonds(MockCMLMolecules.getC2H6O(), MockCMLMolecules.getC2H4()));

        assert (equalBonds(MockCompounds.getC2H4(), MockCompounds.getC2H4()));
        assert (equalBonds(MockCompounds.getC2H6O(), MockCompounds.getC2H6O()));
        assert (!equalBonds(MockCompounds.getC2H4(), MockCompounds.getC2H6O()));
        assert (!equalBonds(MockCompounds.getC2H6O(), MockCompounds.getC2H4()));        
    }
    
    public boolean equalBonds(Compound c1, CMLMolecule c2) {
        for (int i = 0; i < c1.getBondCount(); i++) {
            boolean identified = false;
            Bond bond = c1.getBond(i);
            for (CMLBond cmlBond : c2.getBonds()) {
                if (isEqual(bond, cmlBond)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }
    
    public boolean equalBonds(CMLMolecule c1, CMLMolecule c2) {
        for (int i = 0; i < c1.getBonds().size(); i++) {
            boolean identified = false;
            for (int j = 0; j < c2.getBonds().size(); j++) {
                if (isEqual(c1.getBonds().get(i), c2.getBonds().get(j))) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }
    
    public boolean equalBonds(Compound c1, Compound c2) {
        for (int i = 0; i < c1.getBondCount(); i++) {
            boolean identified = false;
            for (int j = 0; j < c2.getBondCount(); j++) {
                if (isEqual(c1.getBond(i), c2.getBond(j))) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }
}
