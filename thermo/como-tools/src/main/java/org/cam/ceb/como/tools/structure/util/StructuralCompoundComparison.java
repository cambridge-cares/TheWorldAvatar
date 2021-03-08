/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util;

import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.BondType;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.Structure;
import gigadot.chom.model.brownie.Atom;
import java.util.List;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class StructuralCompoundComparison {

    public static boolean isEqual(Compound compound, CMLMolecule cmlMolecule) {
        List<CMLAtom> cmlAtoms = cmlMolecule.getAtoms();
        List<CMLBond> cmlBonds = cmlMolecule.getBonds();

        try {
            int formalCharge = cmlMolecule.getFormalCharge();
            int spinMultiplicity = cmlMolecule.getSpinMultiplicity();

            if (compound.getAtomCount() != cmlAtoms.size()
                    || compound.getBondCount() != cmlBonds.size()
                    || formalCharge != compound.getCharge()
                    || spinMultiplicity != compound.getSpinMultiplicity()) {
                return false;
            }
        } catch (Exception e) {
            if (compound.getAtomCount() != cmlAtoms.size()
                    || compound.getBondCount() != cmlBonds.size()) {
                return false;
            }
        }

        // check the atoms list
        for (int i = 0; i < compound.getAtomCount(); i++) {
            boolean identified = false;
            Atom atom = compound.getAtom(i);
            for (CMLAtom cmlAtom : cmlAtoms) {
                if (isEqual(atom, cmlAtom)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }

        // check the bonds list
        for (int i = 0; i < compound.getBondCount(); i++) {
            boolean identified = false;
            Bond bond = compound.getBond(i);
            for (CMLBond cmlBond : cmlBonds) {
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

    public static boolean isEqual(Structure compound, CMLMolecule cmlMolecule) {
        return isEqual((Compound) compound, cmlMolecule);
    }

    // no quantum calculations are compared
    public static boolean isEqual(Compound compound1, Compound compound2) {
        if (compound1.getAtomCount() != compound2.getAtomCount()
                || compound1.getBondCount() != compound2.getBondCount()
                || compound1.getCharge() != compound2.getCharge()
                || compound1.getSpinMultiplicity() != compound2.getSpinMultiplicity()) {
            return false;
        }

        for (int i = 0; i < compound1.getAtomCount(); i++) {
            boolean identified = false;
            for (int j = 0; j < compound2.getAtomCount(); j++) {
                if (isEqual(compound1.getAtom(i), compound2.getAtom(j))) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }

        for (int i = 0; i < compound1.getBondCount(); i++) {
            boolean identified = false;
            for (int j = 0; j < compound2.getBondCount(); j++) {
                if (isEqual(compound1.getBond(i), compound2.getBond(j))) {
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

    public static boolean isEqual(Structure compound1, Structure compound2) {
        return isEqual((Compound) compound1, (Compound) compound2);
    }

    public static boolean isEqual(CMLMolecule cmlMolecule1, CMLMolecule cmlMolecule2) {
        try {
            if (cmlMolecule1.getAtoms().size() != cmlMolecule2.getAtoms().size()
                    || cmlMolecule1.getBonds().size() != cmlMolecule2.getBonds().size()
                    || cmlMolecule1.getFormalCharge() != cmlMolecule2.getFormalCharge()
                    || cmlMolecule1.getSpinMultiplicity() != cmlMolecule2.getSpinMultiplicity()) {
                return false;
            }
        } catch (Exception e) {
            if (cmlMolecule1.getAtoms().size() != cmlMolecule2.getAtoms().size()
                    || cmlMolecule1.getBonds().size() != cmlMolecule2.getBonds().size()) {
                return false;
            }
        }

        // check the atoms list
        for (int i = 0; i < cmlMolecule1.getAtoms().size(); i++) {
            boolean identified = false;
            for (int j = 0; j < cmlMolecule2.getAtoms().size(); j++) {
                if (isEqual(cmlMolecule1.getAtom(i), cmlMolecule2.getAtom(j))) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }

        // check the bonds list
        for (int i = 0; i < cmlMolecule1.getBonds().size(); i++) {
            boolean identified = false;
            for (int j = 0; j < cmlMolecule2.getBonds().size(); j++) {
                if (isEqual(cmlMolecule1.getBonds().get(i), cmlMolecule2.getBonds().get(j))) {
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

    public static boolean isEqual(Atom atom, CMLAtom cmlAtom) {
        return atom.getElement().atomicNumber == cmlAtom.getChemicalElement().getAtomicNumber()
                //equal &= atom.getElement().atomicWeight == cmlAtom.getChemicalElement().getAtomicWeight()
                && atom.getElement().symbol.equals(cmlAtom.getChemicalElement().getSymbol())
                && cmlAtom.getXYZ3().getArray()[0] <= atom.getXInA() + 0.0001 && cmlAtom.getXYZ3().getArray()[0] >= atom.getXInA() - 0.0001
                && cmlAtom.getXYZ3().getArray()[1] <= atom.getYInA() + 0.0001 && cmlAtom.getXYZ3().getArray()[1] >= atom.getYInA() - 0.0001
                && cmlAtom.getXYZ3().getArray()[2] <= atom.getZInA() + 0.0001 && cmlAtom.getXYZ3().getArray()[2] >= atom.getZInA() - 0.0001;
    }

    public static boolean isEqual(Atom atom1, Atom atom2) {
        return atom1.getElement().atomicNumber == atom2.getElement().atomicNumber
                && atom1.getElement().atomicWeight == atom2.getElement().atomicWeight
                && atom1.getElement().symbol.equals(atom2.getElement().symbol)
                && atom1.getXInA() <= atom2.getXInA() + 0.0001 && atom1.getXInA() >= atom2.getXInA() - 0.0001
                && atom1.getYInA() <= atom2.getYInA() + 0.0001 && atom1.getYInA() >= atom2.getYInA() - 0.0001
                && atom1.getZInA() <= atom2.getZInA() + 0.0001 && atom1.getZInA() >= atom2.getZInA() - 0.0001;
    }

    public static boolean isEqual(CMLAtom cmlAtom1, CMLAtom cmlAtom2) {
        return cmlAtom1.getChemicalElement().getAtomicNumber() == cmlAtom2.getChemicalElement().getAtomicNumber()
                && cmlAtom1.getChemicalElement().getAtomicWeight() == cmlAtom2.getChemicalElement().getAtomicWeight()
                && cmlAtom1.getChemicalElement().getSymbol().equals(cmlAtom2.getChemicalElement().getSymbol())
                && cmlAtom1.getXYZ3().getArray()[0] <= cmlAtom2.getXYZ3().getArray()[0] + 0.0001 && cmlAtom1.getXYZ3().getArray()[0] >= cmlAtom2.getXYZ3().getArray()[0] - 0.0001
                && cmlAtom1.getXYZ3().getArray()[1] <= cmlAtom2.getXYZ3().getArray()[1] + 0.0001 && cmlAtom1.getXYZ3().getArray()[1] >= cmlAtom2.getXYZ3().getArray()[1] - 0.0001
                && cmlAtom1.getXYZ3().getArray()[2] <= cmlAtom2.getXYZ3().getArray()[2] + 0.0001 && cmlAtom1.getXYZ3().getArray()[2] >= cmlAtom2.getXYZ3().getArray()[2] - 0.0001;
    }

    public static boolean isEqual(Bond bond, CMLBond cmlBond) {
        return (isEqual(bond.getAtomA(), cmlBond.getAtom(0)) || isEqual(bond.getAtomA(), cmlBond.getAtom(1)))
                && (isEqual(bond.getAtomB(), cmlBond.getAtom(1)) || isEqual(bond.getAtomB(), cmlBond.getAtom(0)))
                //&& cmlBond.getBondLength(CMLElement.CoordinateType.CARTESIAN) == bond.getBondLength()
                && bond.getBondType() == BondType.parseCMLBondType(cmlBond.getOrder());
    }

    public static boolean isEqual(Bond bond1, Bond bond2) {
        return ((isEqual(bond1.getAtomA(), bond2.getAtomA()) || isEqual(bond1.getAtomA(), bond2.getAtomB())) && (isEqual(bond1.getAtomB(), bond2.getAtomB()) || isEqual(bond1.getAtomB(), bond2.getAtomA())))
                // && bond1.getBondLength() == bond2.getBondLength()
                && bond1.getBondType() == bond2.getBondType();
                //&& bond1.getTorsionBarrier() == bond2.getTorsionBarrierInkcal_Per_mol();
    }

    public static boolean isEqual(CMLBond cmlBond1, CMLBond cmlBond2) {
        if (cmlBond1.getOrder() == null && cmlBond2.getOrder() == null) {
            return ((isEqual(cmlBond1.getAtom(0), cmlBond2.getAtom(0)) || isEqual(cmlBond1.getAtom(0), cmlBond2.getAtom(1))) && (isEqual(cmlBond1.getAtom(1), cmlBond2.getAtom(1)) || isEqual(cmlBond2.getAtom(1), cmlBond2.getAtom(0))))
                    && cmlBond1.getBondLength(CMLElement.CoordinateType.CARTESIAN) == cmlBond2.getBondLength(CMLElement.CoordinateType.CARTESIAN);
        }
        if (cmlBond1.getOrder() == null || cmlBond2.getOrder() == null) {
            return false;
        }
        return ((isEqual(cmlBond1.getAtom(0), cmlBond2.getAtom(0)) || isEqual(cmlBond1.getAtom(0), cmlBond2.getAtom(1))) && (isEqual(cmlBond1.getAtom(1), cmlBond2.getAtom(1)) || isEqual(cmlBond2.getAtom(1), cmlBond2.getAtom(0))))
                && cmlBond1.getBondLength(CMLElement.CoordinateType.CARTESIAN) == cmlBond2.getBondLength(CMLElement.CoordinateType.CARTESIAN)
                && cmlBond1.getOrder().equals(cmlBond2.getOrder());
    }
}
