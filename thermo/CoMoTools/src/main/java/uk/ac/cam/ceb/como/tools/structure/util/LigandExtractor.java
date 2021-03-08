/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.structure.util;

import uk.ac.cam.ceb.como.chem.structure.Compound;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class LigandExtractor {

    public static CMLMolecule extractLigand(CMLMolecule mol, CMLBond torsBond, CMLAtom atom) {
        for (CMLBond bond : mol.getBonds()) {
            if (bond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0
                    || bond.getAtomId(1).compareToIgnoreCase(atom.getId()) == 0) {
                Set<CMLBond> bonds;
                if (bond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0) {
                    bonds = getBonds(bond, bond.getAtom(0), bond.getAtom(1), mol);
                } else {
                    bonds = getBonds(bond, bond.getAtom(1), bond.getAtom(0), mol);
                }

                // only if at least one of the atoms is included
                // check bonds
                boolean valid = false;
                if (torsBond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0 || torsBond.getAtomId(1).compareToIgnoreCase(atom.getId()) == 0) {
                    String id = torsBond.getAtomId(0);
                    if (torsBond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0) {
                        id = torsBond.getAtomId(1);
                    }
                    for (CMLBond cmpBond : bonds) {
                        if (cmpBond.getAtomId(0).compareToIgnoreCase(id) == 0 || cmpBond.getAtomId(1).compareToIgnoreCase(id) == 0) {
                            valid = true;
                            break;
                        }
                    }
                } else {
                    for (CMLBond cmpBond : bonds) {
                        if (cmpBond.equals(torsBond)) {
                            valid = true;
                            break;
                        }
                    }
                }

                if (valid) {
                    bonds.add(torsBond);
                    return extractLigand(mol, bonds);
                }
            }
        }
        return null;
    }

//    public static Collection<CMLMolecule> extractLigands(CMLMolecule mol, CMLAtom atom) {
//        Collection<CMLMolecule> ligands = new HashSet<CMLMolecule>();
//        CMLMolecule clone = new CMLMolecule(mol);
//        Compound compound = CompoundConverter.convert(mol);
//        for (CMLBond bond : clone.getBonds()) {
//            if (bond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0
//                    || bond.getAtomId(1).compareToIgnoreCase(atom.getId()) == 0) {
//                compound.removeBond(CompoundConverter.convert(compound, bond));
//                
////                Set<CMLBond> bonds;
////                if (bond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0) {
////                    bonds = getBonds(bond, bond.getAtom(0), bond.getAtom(1), clone);
////                } else {
////                    bonds = getBonds(bond, bond.getAtom(1), bond.getAtom(0), clone);
////                }
////                CMLMolecule ligand = extractLigand(clone, bonds);
////                //System.out.println(CompoundConverter.convertToEmpiricalFormulaFromCML(ligand));
////                if (ligand != null) {
////                    ligands.add(ligand);
////                    System.out.println("\n\nNEXT:\n");
////                }
//            }
//        }
//        
//        compound.recreateMoleculeList();
//        for (int i = 0; i < compound.getMoleculeCount(); i++) {
//            if (compound.getMolecule(i).getAtomCount() > 1) {
//                ligands.add(CompoundConverter.convert(compound.getMolecule(i)));
//            }
//        }
//        return ligands;
//    }
    public static CMLMolecule extractLigand(CMLMolecule mol, Collection<CMLBond> validBonds) {
        CMLMolecule newMol = new CMLMolecule(mol);
        Collection<CMLBond> invalidBonds = new HashSet<CMLBond>();
        for (CMLBond bond : newMol.getBonds()) {
            boolean identified = false;
            for (CMLBond cmpBond : validBonds) {
                if ((bond.getAtomId(0).compareToIgnoreCase(cmpBond.getAtomId(0)) == 0
                        && bond.getAtomId(1).compareToIgnoreCase(cmpBond.getAtomId(1)) == 0)
                        || (bond.getAtomId(0).compareToIgnoreCase(cmpBond.getAtomId(1)) == 0
                        && bond.getAtomId(1).compareToIgnoreCase(cmpBond.getAtomId(0)) == 0)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                invalidBonds.add(bond);
            }
        }
        for (CMLBond invalidBond : invalidBonds) {
            newMol.deleteBond(invalidBond);
        }
        Collection<CMLAtom> invalidAtoms = new HashSet<CMLAtom>();
        for (CMLAtom atom : newMol.getAtoms()) {
            boolean identified = false;
            for (CMLBond bond : newMol.getBonds()) {
                if (bond.getAtomId(0).compareToIgnoreCase(atom.getId()) == 0
                        || bond.getAtomId(1).compareToIgnoreCase(atom.getId()) == 0) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                invalidAtoms.add(atom);
            }
        }
        for (CMLAtom invalidAtom : invalidAtoms) {
            newMol.deleteAtom(invalidAtom);
        }
        return newMol;
    }

    public static Set<CMLBond> getBonds(CMLBond currentBond, CMLAtom prev, CMLAtom next, CMLMolecule mol) {
        HashSet<CMLBond> bonds = new HashSet<CMLBond>();
        //System.out.println(prev.getElementType() + " - " + next.getElementType());
        for (CMLBond bond : mol.getBonds()) {
            if (bond.getAtomId(0).compareTo(next.getId()) == 0
                    && bond.getAtomId(1).compareTo(prev.getId()) != 0) {
                Set<CMLBond> s = getBonds(bond, next, bond.getAtom(1), mol);
                if (s.isEmpty()) {
                    //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(1).getElementType());
                    bonds.add(bond);
                } else {
                    bonds.addAll(s);
                    //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(1).getElementType());
                    bonds.add(bond);
                }
            } else if (bond.getAtomId(1).compareTo(next.getId()) == 0
                    && bond.getAtomId(0).compareTo(prev.getId()) != 0) {
                Set<CMLBond> s = getBonds(bond, next, bond.getAtom(0), mol);
                if (s.isEmpty()) {
                    //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(1).getElementType());
                    bonds.add(bond);
                } else {
                    bonds.addAll(s);
                    //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(1).getElementType());
                    bonds.add(bond);
                }
            }
        }
        //if (bonds.isEmpty()) {
        for (CMLBond bond : mol.getBonds()) {
            if (bond.getAtomId(0).compareTo(next.getId()) == 0
                    && bond.getAtomId(1).compareTo(prev.getId()) == 0) {
                //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(0).getElementType());
                bonds.add(bond);
            } else if (bond.getAtomId(1).compareTo(next.getId()) == 0
                    && bond.getAtomId(0).compareTo(prev.getId()) == 0) {
                //System.out.println("added " + bond.getAtom(0).getElementType() + " - " + bond.getAtom(0).getElementType());
                bonds.add(bond);
            }
        }
        //}
        return bonds;
    }
}
