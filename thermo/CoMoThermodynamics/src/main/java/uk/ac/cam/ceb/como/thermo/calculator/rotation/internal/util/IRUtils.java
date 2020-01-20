/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util;

import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.BondType;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.Molecule;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.math.linear_algebra.MatrixUtil;
import java.util.ArrayList;
import java.util.Collection;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.Top;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundCopy;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;

/**
 *
 * @author pb556
 */
public class IRUtils {

    public static Top[] extractTop(CompChem ref, CMLBond bond) throws Exception {
        IRCompChemWrapper ccw = new IRCompChemWrapper(ref);
        return extractTop(ccw.getFinalMolecule(), bond);
    }
    
    public static Top[] extractTop(CMLMolecule ref, CMLBond bond) throws Exception {
        Compound clone = CompoundConverter.convert(ref);
        clone.removeBond(CompoundConverter.convert(clone, bond));
        clone.recreateMoleculeList();

        // check if two molecules are present
        if (clone.getMoleculeCount() != 2) {
            //throw new Exception("Torsional bond cannot be identified!");
            return new Top[]{};
        }

        // get both molecules
        Molecule molA = clone.getMolecule(0);
        Molecule molB = clone.getMolecule(1);

        Collection<CMLAtom> topAtomsSideA = extractTopAtoms(ref, molA);
        Collection<CMLAtom> topAtomsSideB = extractTopAtoms(ref, molB);

        if (topAtomsSideA == null || topAtomsSideA.isEmpty() || 
                topAtomsSideB == null || topAtomsSideB.isEmpty()) {
            throw new Exception("Top cannot be extracted!");
        }
        return new Top[]{new Top(ref, topAtomsSideA, bond), new Top(ref, topAtomsSideB, bond)};
    }

    private static Collection<CMLAtom> extractTopAtoms(CMLMolecule cml, Molecule mol) {
        Collection<CMLAtom> topAtoms = new ArrayList<CMLAtom>();
        for (int i = 0; i < mol.getAtomCount(); i++) {
            String r = mol.getAtom(i).getId().trim();
            for (CMLAtom a : cml.getAtoms()) {
                if (r.compareToIgnoreCase(a.getId().trim()) == 0) {
                    topAtoms.add(a);
                    break;
                }
            }
        }
        return topAtoms;
    }

    public static Compound rotate(Compound mol, Bond bond, double angle) throws Exception {
        Compound orig = mol;
        Compound clone = CompoundCopy.copy(orig);

        // remove bond
        clone.removeBond(bond);
        clone.recreateMoleculeList();

        // assumption: the smallest component is rotating
        clone.sortByMolecularSize();
        rotateMoeity(clone.getMolecule(0), bond.getAtomA(), bond.getAtomB(), angle);

        // fix the double to a certain length
        for (int j = 0; j < clone.getAtomCount(); j++) {
            clone.getAtom(j).setXInA(Math.round(clone.getAtom(j).getXInA() * Math.pow(10, 5)) / Math.pow(10, 5));
            clone.getAtom(j).setYInA(Math.round(clone.getAtom(j).getYInA() * Math.pow(10, 5)) / Math.pow(10, 5));
            clone.getAtom(j).setZInA(Math.round(clone.getAtom(j).getZInA() * Math.pow(10, 5)) / Math.pow(10, 5));
        }

        // re-create the whole bond list!
        // atoms are still alright!
        for (Bond b : clone.clonedBondList()) {
            clone.removeBond(b);
        }

        // somehow bond list is messed up after changing the atoms positions
        for (int k = 0; k < orig.getBondCount(); k++) {
            Atom a1 = null;
            Atom a2 = null;
            for (int j = 0; j < clone.getAtomCount(); j++) {
                if (orig.getBond(k).getAtomA().getId().equals(clone.getAtom(j).getId())) {
                    a1 = clone.getAtom(j);
                }
                if (orig.getBond(k).getAtomB().getId().equals(clone.getAtom(j).getId())) {
                    a2 = clone.getAtom(j);
                }
            }
            if (a1 == null || a2 == null) {
                throw new Exception("Top cannot be rotated.");
            }
            clone.addBond(Bond.createBond(a1, a2, BondType.SINGLE, clone));
        }

        clone.recreateMoleculeList();
        return clone;
    }

    public static CMLMolecule rotate(CMLMolecule mol, CMLBond bond, double angle) throws Exception {
        Compound compound = CompoundConverter.convert(mol);
        return CompoundConverter.convert(rotate(compound, CompoundConverter.convert(compound, bond), angle));
    }

    protected static void rotateMoeity(Molecule molecule, Atom atomA, Atom atomB, double angle) {
        double[][] rotMatrix = new double[3][3];
        rotationMatrixByAtoms(atomA, atomB, angle, rotMatrix);
        for (int i = 0; i < molecule.getAtomCount(); i++) {
            rotateAtom(molecule.getAtom(i), atomB, rotMatrix);
        }
    }

    protected static void rotationMatrixByAtoms(Atom atomA, Atom atomB, double angle, double[][] rotMatrix) {
        double[] axis = new double[3];
        axis[0] = atomB.getXInA() - atomA.getXInA();
        axis[1] = atomB.getYInA() - atomA.getYInA();
        axis[2] = atomB.getZInA() - atomA.getZInA();
        MatrixUtil.getRotationMatrix(axis, angle, rotMatrix);
    }

    protected static void rotateAtom(Atom atom, Atom atomRef, double[][] rotMatrix) {
        double posX = atom.getXInA();
        double posY = atom.getYInA();
        double posZ = atom.getZInA();
        double posXR = atomRef.getXInA();
        double posYR = atomRef.getYInA();
        double posZR = atomRef.getZInA();

        double[] vec = new double[3];
        double[] vecOut = new double[3];
        vec[0] = posX - posXR;
        vec[1] = posY - posYR;
        vec[2] = posZ - posZR;
        vecOut[0] = 0.0;
        vecOut[1] = 0.0;
        vecOut[2] = 0.0;

        for (int i = 0; i < rotMatrix.length; i++) {
            for (int j = 0; j < vec.length; j++) {
                vecOut[i] += vec[j] * rotMatrix[i][j];
            }
        }

        vecOut[0] += posXR;
        vecOut[1] += posYR;
        vecOut[2] += posZR;
        atom.setCoordinateInA(vecOut[0], vecOut[1], vecOut[2]);

        //System.out.println("r" + atom.getId() + " [" + vecOut[0] + ", " + vecOut[1] + ", " + vecOut[2] + "]");
    }
}
