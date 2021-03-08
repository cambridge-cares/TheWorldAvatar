/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.structure.util;

import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.Structure;
import java.util.Map;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.chem.structure.Atom;

/**
 *
 * @author pb556
 */
public class CompoundCopy {

    private final Map<String, Integer> atomMap = null;

    public static Compound copy(Compound comp) throws Exception {
        // copy the whole compound object
        Compound clone = new Compound();
        for (int i = 0; i < comp.getAtomCount(); i++) {
            clone.addAtom(copy(comp.getAtom(i)));
        }
        for (int i = 0; i < comp.getBondCount(); i++) {
            clone.addBond(copy(comp.getBond(i), clone, false));
        }
        clone.addVibrations((Vibrations) comp.getVibrations().clone());
        
        // no vibration yet - Vibration object needs to be incluede in the parser
        
        
//        for (int i = 0; i < comp.getVibrationCount(); i++) {
//            Vibration v = new Vibration();
//            
//            
//            
//            
//            for (Displacement orig : comp.getVibration(i).getDisplacements()) {
//                Displacement dClone = new Displacement();
//                dClone.setdx(orig.dx());
//                dClone.setdy(orig.dy());
//                dClone.setdz(orig.dz());
//                dClone.setdxdydzInA(orig.dxInA(), orig.dyInA(), orig.dzInA());
//                v.getDisplacements().add(dClone);
//            }
//
//            v.setForceConstant(comp.getVibration(i).getForceConstant());
//            v.setFrequency(comp.getVibration(i).getFrequency());
//            v.setIRInten(comp.getVibration(i).getIRInten());
//            v.setMode(comp.getVibration(i).getMode());
//            v.setReducedMass(comp.getVibration(i).getReducedMass());
//            
//            for (int j = 0; j < comp.getVibration(i).getIRotorBondCount(); j++) {
//                v.addIRotorBond(comp.getVibration(i).getIRotorBond(j));
//            }
//
//            for (int j = 0; j < comp.getVibration(i).getIRotorBondCount(); j++) {
//                Bond origBond = comp.getVibration(i).getIRotorBond(j);
//                Bond rotBond = null;
//                for (int k = 0; k < clone.getBondCount(); k++) {
//                    if (clone.getBond(k).isSimilar(origBond)) {
//                        rotBond = clone.getBond(k);
//                        break;
//                    }
//                }
//                if (rotBond != null) {
//                    v.addIRotorBond(rotBond);
//                }
//            }
//            
//            clone.addVibration(v);
//        }

        clone.setBasis(comp.getBasis());
        clone.setCharge(comp.getCharge());
        clone.setFinalSCFEnergy(comp.getFinalSCFEnergy());
        clone.setFinalSCFEnergyInHartree(comp.getFinalSCFEnergyInHartree());
        clone.setHf(comp.getHf());
        clone.setInchi(comp.getInchi());
        clone.setJobType(comp.getJobType());
        clone.setMethod(comp.getMethod());
        clone.setMultiplicity(comp.getSpinMultiplicity());
        try {
            clone.setPricipleMOI(comp.getPrincipalMOI());
        } catch (NullPointerException e) {
        }
        try {
            clone.setRotationalConstants(comp.getRotationalConstants());
        } catch (NullPointerException e) {
        }
        try {
            clone.setRotationalSymmetryNumber(comp.getRotationalSymmetryNumber());
        } catch (NullPointerException e) {
        }
        clone.recreateMoleculeList();

        return clone;
    }

    public static Atom copy(Atom atom) {
        Atom clone = new Atom(atom.getElement().getSymbol());
        clone.setCoordinate(atom.getX(), atom.getY(), atom.getZ());
        clone.setCoordinateInA(atom.getXInA(), atom.getYInA(), atom.getZInA());
        clone.setCount(atom.getCount());
        clone.setId(atom.getId());
        clone.setIsotope(atom.getIsotope());
        clone.setOrder(atom.getOrder());
        clone.setRef(atom.getRef());
        clone.setVersion(atom.getVersion());
        return clone;
    }

    public static Bond copy(Bond bond, Structure clonedStructure, boolean cloneAtoms) throws Exception {
        Bond clone = null;
        if (cloneAtoms) {
            clone = Bond.createBond(copy(bond.getAtomA()), copy(bond.getAtomB()), bond.getBondType(), clonedStructure);
        } else {
            // clone = Bond.createBond(clonedStructure.getAtomByOrder(bond.getAtomA().getOrder()), clonedStructure.getAtomByOrder(bond.getAtomB().getOrder()), bond.getBondType(), clonedStructure);
            Atom atomA = null;
            Atom atomB = null;
            for (int i = 0; i < clonedStructure.getAtomCount(); i++) {
                if (atomA != null && atomB != null) {
                    break;
                }
                if (clonedStructure.getAtom(i).getId().equals(bond.getAtomA().getId())) {
                    atomA = clonedStructure.getAtom(i);
                    continue;
                }
                if (clonedStructure.getAtom(i).getId().equals(bond.getAtomB().getId())) {
                    atomB = clonedStructure.getAtom(i);
                }
            }
            if (atomA == null || atomB == null) {
                throw new Exception("Bond cannot be cloned.");
            }
            clone = Bond.createBond(atomA, atomB, bond.getBondType(), clonedStructure);
        }
        clone.setTorsionBarrier(bond.getTorsionBarrier());
        clone.setTorsionBarrierInkcal_Per_mol(bond.getTorsionBarrierInkcal_Per_mol());
        return clone;
    }
}
