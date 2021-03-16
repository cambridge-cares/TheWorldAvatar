/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util;

import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;

/**
 *
 * @author pb556
 */
public class MockCMLMolecules {

    public static CMLMolecule getC2H4() {

        CMLMolecule mol = new CMLMolecule();
        CMLAtom h1 = new CMLAtom("H1", new ChemicalElement("H", 1, 1.00794));
        h1.setXYZ3(new Point3(0.0168, 0.0144, 0.1474));
        CMLAtom h2 = new CMLAtom("H2", new ChemicalElement("H", 1, 1.00794));
        h2.setXYZ3(new Point3(0.0363, 1.8623, 0.0746));
        CMLAtom h3 = new CMLAtom("H3", new ChemicalElement("H", 1, 1.00794));
        h3.setXYZ3(new Point3(2.4893, -0.0146, 0.0727));
        CMLAtom h4 = new CMLAtom("H4", new ChemicalElement("H", 1, 1.00794));
        h4.setXYZ3(new Point3(2.5088, 1.8333, -0.0001));
        CMLAtom c1 = new CMLAtom("C1", new ChemicalElement("C", 6, 12.0107));
        c1.setXYZ3(new Point3(0.5971, 0.9317, 0.0938));
        CMLAtom c2 = new CMLAtom("C2", new ChemicalElement("C", 6, 12.0107));
        c2.setXYZ3(new Point3(1.9285, 0.9160, 0.0536));
        mol.addAtom(h1);
        mol.addAtom(h2);
        mol.addAtom(h3);
        mol.addAtom(h4);
        mol.addAtom(c1);
        mol.addAtom(c2);

        CMLBond b1 = new CMLBond(mol.getAtomById("H1"), mol.getAtomById("C1"), "1");
        CMLBond b2 = new CMLBond(mol.getAtomById("H2"), mol.getAtomById("C1"), "1");
        CMLBond b3 = new CMLBond(mol.getAtomById("H3"), mol.getAtomById("C2"), "1");
        CMLBond b4 = new CMLBond(mol.getAtomById("H4"), mol.getAtomById("C2"), "1");
        CMLBond b5 = new CMLBond(mol.getAtomById("C1"), mol.getAtomById("C2"), "2");

        mol.addBond(b1);
        mol.addBond(b2);
        mol.addBond(b3);
        mol.addBond(b4);
        mol.addBond(b5);

        mol.setFormalCharge(0);
        mol.setSpinMultiplicity(1);

        return mol;
    }

    public static CMLMolecule getC2H6O() {

        CMLMolecule mol = new CMLMolecule();
        CMLAtom h1 = new CMLAtom("H1", new ChemicalElement("H", 1, 1.00794));
        h1.setXYZ3(new Point3(-0.0076, 1.2340, 0.8250));
        CMLAtom h2 = new CMLAtom("H2", new ChemicalElement("H", 1, 1.00794));
        h2.setXYZ3(new Point3(0.9989, 0.0153, 1.6382));
        CMLAtom h3 = new CMLAtom("H3", new ChemicalElement("H", 1, 1.00794));
        h3.setXYZ3(new Point3(1.4229, 0.5686, 0.0063));
        CMLAtom h4 = new CMLAtom("H4", new ChemicalElement("H", 1, 1.00794));
        h4.setXYZ3(new Point3(1.8822, 2.8647, 0.9020));
        CMLAtom h5 = new CMLAtom("H5", new ChemicalElement("H", 1, 1.00794));
        h5.setXYZ3(new Point3(1.4572, 2.3102, 2.5375));
        CMLAtom h6 = new CMLAtom("H6", new ChemicalElement("H", 1, 1.00794));
        h6.setXYZ3(new Point3(3.7473, 2.1730, 2.1189));

        CMLAtom c1 = new CMLAtom("C1", new ChemicalElement("C", 6, 12.0107));
        c1.setXYZ3(new Point3(1.0196, 0.8843, 0.9734));
        CMLAtom c2 = new CMLAtom("C2", new ChemicalElement("C", 6, 12.0107));
        c2.setXYZ3(new Point3(1.8755, 1.9905, 1.5708));

        CMLAtom o = new CMLAtom("O", new ChemicalElement("O", 8, 15.9994));
        o.setXYZ3(new Point3(3.1979, 1.4812, 1.7417));
        mol.addAtom(h1);
        mol.addAtom(h2);
        mol.addAtom(h3);
        mol.addAtom(h4);
        mol.addAtom(h5);
        mol.addAtom(h6);
        mol.addAtom(c1);
        mol.addAtom(c2);
        mol.addAtom(o);

        CMLBond b1 = new CMLBond(mol.getAtomById("H1"), mol.getAtomById("C1"), "1");
        CMLBond b2 = new CMLBond(mol.getAtomById("H2"), mol.getAtomById("C1"), "1");
        CMLBond b3 = new CMLBond(mol.getAtomById("H3"), mol.getAtomById("C1"), "1");
        CMLBond b4 = new CMLBond(mol.getAtomById("H4"), mol.getAtomById("C2"), "1");
        CMLBond b5 = new CMLBond(mol.getAtomById("H5"), mol.getAtomById("C2"), "1");
        CMLBond b6 = new CMLBond(mol.getAtomById("C1"), mol.getAtomById("C2"), "1");
        CMLBond b7 = new CMLBond(mol.getAtomById("H6"), mol.getAtomById("O"), "1");
        CMLBond b8 = new CMLBond(mol.getAtomById("C2"), mol.getAtomById("O"), "1");

        mol.addBond(b1);
        mol.addBond(b2);
        mol.addBond(b3);
        mol.addBond(b4);
        mol.addBond(b5);
        mol.addBond(b6);
        mol.addBond(b7);
        mol.addBond(b8);

        mol.setFormalCharge(0);
        mol.setSpinMultiplicity(1);

        return mol;
    }

    public static CMLMolecule getH2O() {

        CMLMolecule mol = new CMLMolecule();
        CMLAtom h1 = new CMLAtom("H1", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h2 = new CMLAtom("H2", new ChemicalElement("H", 1, 1.00794));
        CMLAtom o = new CMLAtom("O", new ChemicalElement("O", 8, 15.9994));
        mol.addAtom(h1);
        mol.addAtom(h2);
        mol.addAtom(o);

        CMLBond b1 = new CMLBond(mol.getAtomById("H1"), mol.getAtomById("O"));
        CMLBond b2 = new CMLBond(mol.getAtomById("H2"), mol.getAtomById("O"));
        mol.addBond(b1);
        mol.addBond(b2);

        mol.setFormalCharge(0);
        mol.setSpinMultiplicity(1);

        return mol;
    }
}
