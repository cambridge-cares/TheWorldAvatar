/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml;

import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.molutil.ChemicalElement;

/**
 *
 * @author pb556
 */
public class MockCMLMolecules {

    public static CMLMolecule getC2H4() {
        
        CMLMolecule mol = new CMLMolecule();
        CMLAtom h1 = new CMLAtom("H1", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h2 = new CMLAtom("H2", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h3 = new CMLAtom("H3", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h4 = new CMLAtom("H4", new ChemicalElement("H", 1, 1.00794));
        CMLAtom c1 = new CMLAtom("C1", new ChemicalElement("C", 6, 12.0107));
        CMLAtom c2 = new CMLAtom("C2", new ChemicalElement("C", 6, 12.0107));
        mol.addAtom(h1);
        mol.addAtom(h2);
        mol.addAtom(h3);
        mol.addAtom(h4);
        mol.addAtom(c1);
        mol.addAtom(c2);
        
        CMLBond b1 = new CMLBond(mol.getAtomById("H1"), mol.getAtomById("C1"));
        CMLBond b2 = new CMLBond(mol.getAtomById("H2"), mol.getAtomById("C1"));
        CMLBond b3 = new CMLBond(mol.getAtomById("H3"), mol.getAtomById("C2"));
        CMLBond b4 = new CMLBond(mol.getAtomById("H4"), mol.getAtomById("C2"));
        CMLBond b5 = new CMLBond(mol.getAtomById("C1"), mol.getAtomById("C2"), "2");
        
        mol.addBond(b1);
        mol.addBond(b2);
        mol.addBond(b3);
        mol.addBond(b4);
        mol.addBond(b5);
        
        return mol;
    }
    
    public static CMLMolecule getC2H6O() {
        
        CMLMolecule mol = new CMLMolecule();
        CMLAtom h1 = new CMLAtom("H1", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h2 = new CMLAtom("H2", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h3 = new CMLAtom("H3", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h4 = new CMLAtom("H4", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h5 = new CMLAtom("H5", new ChemicalElement("H", 1, 1.00794));
        CMLAtom h6 = new CMLAtom("H6", new ChemicalElement("H", 1, 1.00794));
        CMLAtom c1 = new CMLAtom("C1", new ChemicalElement("C", 6, 12.0107));
        CMLAtom c2 = new CMLAtom("C2", new ChemicalElement("C", 6, 12.0107));
        CMLAtom o = new CMLAtom("O", new ChemicalElement("O", 8, 15.9994));
        mol.addAtom(h1);
        mol.addAtom(h2);
        mol.addAtom(h3);
        mol.addAtom(h4);
        mol.addAtom(h5);
        mol.addAtom(h6);
        mol.addAtom(c1);
        mol.addAtom(c2);
        mol.addAtom(o);
        
        CMLBond b1 = new CMLBond(mol.getAtomById("H1"), mol.getAtomById("C1"));
        CMLBond b2 = new CMLBond(mol.getAtomById("H2"), mol.getAtomById("C1"));
        CMLBond b3 = new CMLBond(mol.getAtomById("H3"), mol.getAtomById("C1"));
        CMLBond b4 = new CMLBond(mol.getAtomById("H4"), mol.getAtomById("C2"));
        CMLBond b5 = new CMLBond(mol.getAtomById("H5"), mol.getAtomById("C2"));
        CMLBond b6 = new CMLBond(mol.getAtomById("C1"), mol.getAtomById("C2"));
        CMLBond b7 = new CMLBond(mol.getAtomById("H6"), mol.getAtomById("O"));
        CMLBond b8 = new CMLBond(mol.getAtomById("C2"), mol.getAtomById("O"));
        
        mol.addBond(b1);
        mol.addBond(b2);
        mol.addBond(b3);
        mol.addBond(b4);
        mol.addBond(b5);
        mol.addBond(b6);
        mol.addBond(b7);
        mol.addBond(b8);
        
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
        
        return mol;
    }
}
