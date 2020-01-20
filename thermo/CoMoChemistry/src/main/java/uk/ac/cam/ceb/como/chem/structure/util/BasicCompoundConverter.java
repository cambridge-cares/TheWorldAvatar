/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.structure.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import org.xmlcml.cml.base.CMLElement.CoordinateType;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.tools.MoleculeTool;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.BondType;
import uk.ac.cam.ceb.como.chem.structure.Structure;

/**
 *
 * @author pb556
 */

public class BasicCompoundConverter {
    public static Logger logger = Logger.getLogger(BasicCompoundConverter.class);

    /**
     * Convert JChemDocument object to an empirical formula
     * @param structure
     * @return empirical formula
     */
    
    public static String convertToEmpiricalFormula(Structure structure) {
        List<Atom> atoms = StructureTools.getImplodedAtomListByElement(structure);
        return ModelUtils.convertToEmpiricalFormula(atoms);
    }

    /**
     * Required charge and spin multiplicity to be set in JChemDocument.
     * @param structure
     * @return
     */
    
    public static CMLMolecule convertToCMLMolecule(Structure structure) {
        CMLMolecule cmlMol = new CMLMolecule();

        Map<Integer, CMLAtom> atomMap = new HashMap<Integer, CMLAtom>(structure.getAtomCount());
        for (int i = 0; i < structure.getAtomCount(); i++) {
            Atom jatom = structure.getAtom(i);
            String aid = "a" + (i + 1);
            CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(jatom.getElement().getAtomicNumber()));
            atomMap.put(jatom.getOrder(), atom);
            atom.setPoint3(new Point3(jatom.getXInA(), jatom.getYInA(), jatom.getZInA()), CoordinateType.CARTESIAN);
            cmlMol.addAtom(atom);
        }
        if (structure.getBondCount() > 0) {
            for (int i = 0; i < structure.getBondCount(); i++) {
                Bond jbond = structure.getBond(i);
                int jaA = jbond.getAtomA().getOrder();
                int jaB = jbond.getAtomB().getOrder();
                CMLBond bond = new CMLBond(atomMap.get(jaA), atomMap.get(jaB));
                bond.setOrder(BondType.convertToCMLBondType(jbond.getBondType()));
                cmlMol.addBond(bond);
            }
        } else {
            // calculate bonding
            MoleculeTool moleculeTool = MoleculeTool.getOrCreateTool(cmlMol);
            moleculeTool.calculateBondedAtoms();
            moleculeTool.adjustBondOrdersToValency();
        }
        return cmlMol;
    }

    private static final class AtomWrapperForInChi {

        private final Atom atom;

        public AtomWrapperForInChi(Atom atom) {
            this.atom = atom;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof AtomWrapperForInChi)) {
                return false;
            }
            // Object must be Atom at this point
            AtomWrapperForInChi atomW = (AtomWrapperForInChi) obj;
            return this.atom.equalsIgnorePosition(atomW.atom);
        }

        @Override
        public int hashCode() {
            int hash = 7;
            hash = 79 * hash + (this.atom != null ? this.atom.hashCode() : 0);
            return hash;
        }
    }
}