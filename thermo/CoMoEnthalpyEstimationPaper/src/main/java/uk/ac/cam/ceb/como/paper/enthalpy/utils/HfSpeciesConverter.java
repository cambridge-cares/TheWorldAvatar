
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.File;
import java.util.HashMap;

import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.chem.property.Vibrations;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.Molecule;
import uk.ac.cam.ceb.como.chem.structure.Structure;
import uk.ac.cam.ceb.como.chem.structure.util.CompoundConverter;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 *
 * @author pb556
 */

public class HfSpeciesConverter {

    public static Integer[] getNumberOfElectrons(Compound comp) throws Exception {
        if (comp != null) {
            int numEl = 0;
            for (int i = 0; i < comp.getAtomCount(); i++) {
                Atom a = comp.getAtom(i);
                int atomicNumber = a.getElement().getAtomicNumber();
                numEl += atomicNumber;
            }
            int unpaired = comp.getSpinMultiplicity() - 1;
            int paired = numEl - unpaired;
            if (paired % 2 != 0) {
                return null;
            }
            System.out.println(paired + " + " + unpaired + " = " + numEl);
            return new Integer[]{paired, unpaired};
        }
        return null;
    }

    public static Compound parse(File f) throws Exception {
        FrequencyParser parser = new FrequencyParser();
        parser.set(f);
        parser.parse();
        CompChem cc = (CompChem) parser.get();
        IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
        Compound comp = CompoundConverter.convert(ccw.getFinalMolecule());
        comp.recreateMoleculeList();
        return comp;
    }

    public static Species getHfSpecies(Structure comp, String id) {
        Species s = new Species(id);
        HashMap<Atom, String> atomMap = new HashMap<>();
        for (int i = 0; i < comp.getAtomCount(); i++) {
            atomMap.put(comp.getAtom(i), s.addAtom(comp.getAtom(i).getElement().getSymbol()));
        }
        for (int i = 0; i < comp.getBondCount(); i++) {
            s.addBond(mapBondType(comp.getBond(i).getBondType()), atomMap.get(comp.getBond(i).getAtomA()), atomMap.get(comp.getBond(i).getAtomB()));
        }
        return s;
    }

    public static double calculateZPE(Compound comp, double scalingFactor) {
        double zpe = 0.0;
        for (int i = 0; i < comp.getVibrationCount(); i++) {
            double theta = (comp.getVibration(i).getFrequency() * scalingFactor)
                    / (PhysicalConstants.k_B / (PhysicalConstants.h * 100 * PhysicalConstants.c));
            zpe += theta / 2;
        }
        return zpe * PhysicalConstants.R / (PhysicalConstants.NA * 4.3597439422e-18);
    }

    public static double calculateZPE(Vibrations modes, double scalingFactor) {
        double zpe = 0.0;
        for (int i = 0; i < modes.size(); i++) {
            double theta = (modes.get(i).getFrequency() * scalingFactor)
                    / (PhysicalConstants.k_B / (PhysicalConstants.h * 100 * PhysicalConstants.c));
            zpe += theta / 2;
        }
        return zpe * PhysicalConstants.R / (PhysicalConstants.NA * 4.3597439422e-18);
    }

    public static BondType mapBondType(uk.ac.cam.ceb.como.chem.structure.BondType type) {
        if (type == uk.ac.cam.ceb.como.chem.structure.BondType.AROMATIC) {
            return BondType.AROMATIC;
        }
        if (type == uk.ac.cam.ceb.como.chem.structure.BondType.DOUBLE) {
            return BondType.DOUBLE;
        }
        if (type == uk.ac.cam.ceb.como.chem.structure.BondType.SINGLE) {
            return BondType.SINGLE;
        }
        if (type == uk.ac.cam.ceb.como.chem.structure.BondType.TRIPLE) {
            return BondType.TRIPLE;
        }
        return BondType.SINGLE;
    }

//    public static Species getHfSpecies(Structure structure, String id) {
//        Species convS = new Species(id);
//        for (int i = 0; i < structure.getAtomCount(); i++) {
//            convS.addAtom(structure.getAtom(i).getId(), structure.getAtom(i).getElement().symbol);
//        }
//        for (int i = 0; i < structure.getBondCount(); i++) {
//            BondType bt = BondType.SINGLE;
//            switch (structure.getBond(i).getBondType()) {
//                case AROMATIC:
//                    bt = BondType.AROMATIC;
//                    break;
//                case DOUBLE:
//                    bt = BondType.DOUBLE;
//                    break;
//                case SINGLE:
//                    bt = BondType.SINGLE;
//                    break;
//                case TRIPLE:
//                    bt = BondType.TRIPLE;
//                    break;
//            }
//            convS.addBond(bt, structure.getBond(i).getAtomA().getId(), structure.getBond(i).getAtomB().getId());
//        }
//        return convS;
//    }
// 
    
    public static Species getHfSpecies(Molecule mol, String id) {
        return getHfSpecies(mol, id);
    }

    public static Species getHfSpecies(Compound compound, String id, double scalingFactor) {
        compound.recreateMoleculeList();
        Species s = getHfSpecies((Structure) compound, id);
        s.setTotalEnergy(compound.getFinalSCFEnergyInHartree() * 2625.5 + calculateZPE(compound, scalingFactor));
        try {
            s.setHf(compound.getHf().getValue());
        } catch (NullPointerException npe) {
        }
        return s;
    }

//    public static Species getHfSpecies(CompChem cc) throws Exception {
//        CompChemParser ccParser = new CompChemParser();
//        ccParser.setCompChem(cc);
//        ccParser.parse();
//        Species s = getHfSpecies(ccParser.get(), cc.getId());
//        return s;
//    }

    public static Species getHfSpecies(CMLMolecule mol) {
        return getHfSpecies((Compound) CompoundConverter.convert(mol), mol.getId());
    }

    public static Species getHfSpecies(CMLMolecule mol, String id) {
        return getHfSpecies((Compound) CompoundConverter.convert(mol), id);
    }
}