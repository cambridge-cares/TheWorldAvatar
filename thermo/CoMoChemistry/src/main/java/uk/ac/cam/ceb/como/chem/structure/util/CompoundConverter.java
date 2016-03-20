package uk.ac.cam.ceb.como.chem.structure.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.chem.structure.BondType;
import uk.ac.cam.ceb.como.chem.structure.Structure;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.structure.Atom;
import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.chem.structure.Molecule;
import org.xmlcml.cml.base.CMLElement.CoordinateType;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLAtomArray;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLBondArray;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.tools.MoleculeTool;
import org.xmlcml.euclid.Point3;
import org.xmlcml.molutil.ChemicalElement;

/**
 *
 * @author pb556
 */
public abstract class CompoundConverter {

    public static Logger logger = Logger.getLogger(CompoundConverter.class);

    /**
     * Convert JChemDocument object to an empirical formula
     *
     * @param structure
     * @return empirical formula
     */
    public static String convertToEmpiricalFormula(Structure structure) {
        List<Atom> atoms = getImplodedAtomListByElement(structure);
        return convertToEmpiricalFormula(atoms);
    }

    public static String convertToEmpiricalFormula(Atom[] atoms) {
        return convertToEmpiricalFormula(Arrays.asList(atoms));
    }

    public static String convertToEmpiricalFormula(Collection<Atom> atoms) {
        List<Atom> atomList = new ArrayList<Atom>(atoms);
        Collections.sort(atomList, new ElementComparer());
        String formula = "";
        for (Atom atom : atomList) {
            formula += atom.getElement().getSymbol() + ((atom.getCount() > 1) ? atom.getCount() + "" : "");
        }
        return formula;
    }
    
    public static String convertToEmpiricalFormulaFromCML(CMLMolecule mol) {
        return convertToEmpiricalFormulaFromCML(mol.getAtoms());
    }
    
    public static String convertToEmpircalFomulaFromCML(CMLAtom[] atoms) {
        return convertToEmpiricalFormulaFromCML(Arrays.asList(atoms));
    }
    
    public static String convertToEmpiricalFormulaFromCML(Collection<CMLAtom> atoms) {
        List<Atom> atomList = new ArrayList<Atom>();
        for (CMLAtom a : atoms) {
            atomList.add(new Atom(a.getElementType()));
        }
        return convertToEmpiricalFormula(atomList);
    }

    public static String compressEmpiricalFormula(String formula) {
        char c = formula.charAt(0);
        int ctr = 1;
        String compressedFormula = "";
        for (int i = 1; i < formula.length(); i++) {
            if (c == formula.charAt(i)) {
                ctr++;
            } else {
                if (ctr != 1) {
                    compressedFormula += "" + c + ctr;
                } else {
                    compressedFormula += "" + c;
                }
                ctr = 1;
                c = formula.charAt(i);
            }
        }
        if (ctr != 1) {
            return compressedFormula + "" + c + ctr;
        }
        return compressedFormula + "" + c;
    }

    /**
     * Required charge and spin multiplicity to be set in JChemDocument.
     *
     * @param structure
     * @return
     */
    public static CMLMolecule convert(Structure structure) {
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

    public static CMLMolecule convert(Compound comp) {
        CMLMolecule cmlMol = new CMLMolecule();

        Map<Integer, CMLAtom> atomMap = new HashMap<Integer, CMLAtom>(comp.getAtomCount());
        for (int i = 0; i < comp.getAtomCount(); i++) {
            Atom jatom = comp.getAtom(i);
            String aid = "a" + (i + 1);
            CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(jatom.getElement().getAtomicNumber()));
            atomMap.put(jatom.getOrder(), atom);
            atom.setPoint3(new Point3(jatom.getXInA(), jatom.getYInA(), jatom.getZInA()), CoordinateType.CARTESIAN);
            cmlMol.addAtom(atom);
        }
        if (comp.getBondCount() > 0) {
            for (int i = 0; i < comp.getBondCount(); i++) {
                Bond jbond = comp.getBond(i);
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

        try {
            cmlMol.setFormalCharge(comp.getCharge());
            } catch (RuntimeException rte) {}
        try {
            cmlMol.setSpinMultiplicity(comp.getSpinMultiplicity());
        } catch (RuntimeException rte) {}

        return cmlMol;
    }

    public static CMLMolecule convert(Molecule comp) {
        CMLMolecule cmlMol = new CMLMolecule();

        Map<Integer, CMLAtom> atomMap = new HashMap<Integer, CMLAtom>(comp.getAtomCount());
        for (int i = 0; i < comp.getAtomCount(); i++) {
            Atom jatom = comp.getAtom(i);
            String aid = "a" + (i + 1);
            CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(jatom.getElement().getAtomicNumber()));
            atomMap.put(jatom.getOrder(), atom);
            atom.setPoint3(new Point3(jatom.getXInA(), jatom.getYInA(), jatom.getZInA()), CoordinateType.CARTESIAN);
            cmlMol.addAtom(atom);
        }
        if (comp.getBondCount() > 0) {
            for (int i = 0; i < comp.getBondCount(); i++) {
                Bond jbond = comp.getBond(i);
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

    public static List<Atom> getAtomList(Structure structure) {
        List<Atom> atoms = new ArrayList<Atom>();
        for (int i = 0; i < structure.getAtomCount(); i++) {
            final Atom atom = structure.getAtom(i);
            atoms.add(atom);
        }
        return atoms;
    }

    public static List<Atom> getImplodedAtomListByElement(Structure structure) {
        List<Atom> atomList = getAtomList(structure);
        Map<Element, Integer> elementCountMap = getImplodedElementCountMapByElement(atomList);
        List<Atom> newAtomList = new ArrayList<Atom>();
        for (Map.Entry<Element, Integer> entry : elementCountMap.entrySet()) {
            Element element = entry.getKey();
            Integer count = entry.getValue();
            Atom atom = new Atom(element);
            atom.setCount(count);
            newAtomList.add(atom);
        }
        return newAtomList;
    }

    public static Compound convert(CMLMolecule molecule) {
        Compound compound = new Compound();
        Map<String, Integer> atomMap = null;

        CMLAtomArray atomArray = molecule.getAtomArray();
        if (atomArray != null) {
            List<CMLAtom> atoms = atomArray.getAtoms();
            atomMap = new HashMap<String, Integer>(atoms.size());
            for (int i = 0; i < atoms.size(); i++) {
                String symbol = atoms.get(i).getElementType();
                Atom atom = new Atom(symbol);
                atom.setId(atoms.get(i).getId());
                compound.addAtom(atom);
                atomMap.put(atoms.get(i).getId(), i);
                atom.setOrder(i);
                Point3 point = atoms.get(i).getXYZ3();
                double posX = point.elementAt(0);
                double posY = point.elementAt(1);
                double posZ = point.elementAt(2);
                atom.setCoordinateInA(posX, posY, posZ);
            }
        } else {
            throw new RuntimeException("Molecule contains no atom");
        }

        CMLBondArray bondArray = molecule.getBondArray();
        if (bondArray != null) {
            List<CMLBond> bonds = bondArray.getBonds();
            for (int i = 0; i < bonds.size(); i++) {
                int A = atomMap.get(bonds.get(i).getAtomId(0)).intValue();
                int B = atomMap.get(bonds.get(i).getAtomId(1)).intValue();
                // in this version bond type is labelled by integer.
                Bond.createBond(compound.getAtomByOrder(A), compound.getAtomByOrder(B), BondType.parseCMLBondType(bonds.get(i).getOrder()), compound);
            }
        } else {
            throw new RuntimeException("BondArray not found.");
        }
        //compound.calculateBonds();

        try {
            compound.setCharge(molecule.getFormalCharge());
        } catch (RuntimeException rte) {}
        try {
            compound.setMultiplicity(molecule.getSpinMultiplicity());
        } catch (RuntimeException rte) {}
        try {
            compound.recreateMoleculeList();
        } catch (RuntimeException rte) {}

        return compound;
    }

    public static Bond convert(CMLMolecule cmlMol, Compound comp, CMLBond cmlBond) {
        CMLBondArray bondArray = cmlMol.getBondArray();
        if (bondArray != null) {
            for (int i = 0; i < comp.getBondCount(); i++) {
                if ((cmlBond.getAtom(0).getId().equals(comp.getBond(i).getAtomA().getId()) && cmlBond.getAtom(1).getId().equals(comp.getBond(i).getAtomB().getId()))
                        || (cmlBond.getAtom(1).getId().equals(comp.getBond(i).getAtomA().getId()) && cmlBond.getAtom(0).getId().equals(comp.getBond(i).getAtomB().getId()))) {
                    return comp.getBond(i);
                }
            }
        }
        return null;
    }

    public static Bond convert(Compound comp, CMLBond cmlBond) {
        for (int i = 0; i < comp.getBondCount(); i++) {
            if ((cmlBond.getAtom(0).getId().equals(comp.getBond(i).getAtomA().getId()) && cmlBond.getAtom(1).getId().equals(comp.getBond(i).getAtomB().getId()))
                    || (cmlBond.getAtom(1).getId().equals(comp.getBond(i).getAtomA().getId()) && cmlBond.getAtom(0).getId().equals(comp.getBond(i).getAtomB().getId()))) {
                return comp.getBond(i);
            }
        }
        return null;
    }

    public static Bond convert(CMLMolecule cmlMol, CMLBond cmlBond) {
        return convert(cmlMol, convert(cmlMol), cmlBond);
    }

    public static Atom convert(CMLMolecule cmlMol, Compound comp, CMLAtom cmlAtom) {
        CMLAtomArray atomArray = cmlMol.getAtomArray();
        if (atomArray != null) {
            for (int i = 0; i < comp.getAtomCount(); i++) {
                if (cmlAtom.getId().equals(comp.getAtom(i).getId())) {
                    return comp.getAtom(i);
                }
            }
        }
        return null;
    }

    public static Atom convert(CMLMolecule cmlMol, CMLAtom cmlAtom) {
        return convert(cmlMol, convert(cmlMol), cmlAtom);
    }
    
    private static class ElementComparer implements Comparator {

        @Override
        public int compare(Object obj1, Object obj2) {
            return ((Atom) obj1).getElement().getSymbol().compareTo(((Atom) obj2).getElement().getSymbol());
        }
    }

    public static Collection<Element> getElementList(Collection<Atom> atoms) {
        List<Element> elements = new ArrayList<Element>();
        for (Iterator<Atom> it = atoms.iterator(); it.hasNext();) {
            Atom atom = it.next();
            elements.add(atom.getElement());
        }
        return elements;
    }

    private static Map<Element, Integer> getImplodedElementCountMapByElement(List<Atom> atoms) {
        Map<Element, Integer> elementCountMap = new HashMap<Element, Integer>();
        for (Atom atom : atoms) {
            Element e = atom.getElement();
            Integer count = elementCountMap.get(e);
            elementCountMap.put(e, (count != null ? count : 0) + atom.getCount());
        }
        return elementCountMap;
    }
    
//    public static OBMol convertToOBMol(CMLMolecule jDoc) {
//        return convertToOBMol(convert(jDoc));
//    }
//    
//    public static OBMol convertToOBMol(Compound jDoc) {
//        OBMol obmol = new OBMol();
//        Map<Atom, Integer> atomMap = new HashMap<Atom, Integer>(jDoc.getAtomCount());
//        for (int i = 0; i < jDoc.getAtomCount(); i++) {
//            OBAtom atom = new OBAtom();
//            atom.SetAtomicNum(jDoc.getAtom(i).getElement().getAtomicNumber());
//            atom.SetIdx(i + 1);
//            atom.SetVector(jDoc.getAtom(i).getXInA(), jDoc.getAtom(i).getYInA(), jDoc.getAtom(i).getZInA());
//            obmol.AddAtom(atom);
//            atomMap.put(jDoc.getAtom(i), i + 1);
//        }
//
//        for (int i = 0; i < jDoc.getBondCount(); i++) {
//            OBBond bond = new OBBond();
//            Bond mybond = jDoc.getBond(i);
//            OBAtom atomA = obmol.GetAtom(atomMap.get(mybond.getAtomA()));
//            OBAtom atomB = obmol.GetAtom(atomMap.get(mybond.getAtomB()));
//            bond.Set(i, atomA, atomB, BondType.convertToOBBondType(mybond.getBondType()), 0);
//            obmol.AddBond(bond);
//        }
//        return obmol;
//    }
    
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
    
//    /**
//     * Convert JChemDocument object to an empirical formula
//     * @param structure
//     * @return empirical formula
//     */
//    public static String convertToEmpiricalFormula(Structure structure) {
//        List<Atom> atoms = StructureTools.getImplodedAtomListByElement(structure);
//        return ModelUtils.convertToEmpiricalFormula(atoms);
//    }
//
//    /**
//     * Required charge and spin multiplicity to be set in JChemDocument.
//     * @param structure
//     * @return
//     */
//    public static CMLMolecule convertToCMLMolecule(Structure structure) {
//        CMLMolecule cmlMol = new CMLMolecule();
//
//        Map<Integer, CMLAtom> atomMap = new HashMap<Integer, CMLAtom>(structure.getAtomCount());
//        for (int i = 0; i < structure.getAtomCount(); i++) {
//            Atom jatom = structure.getAtom(i);
//            String aid = "a" + (i + 1);
//            CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(jatom.getElement().getAtomicNumber()));
//            atomMap.put(jatom.getOrder(), atom);
//            atom.setPoint3(new Point3(jatom.getXInA(), jatom.getYInA(), jatom.getZInA()), CoordinateType.CARTESIAN);
//            cmlMol.addAtom(atom);
//        }
//        if (structure.getBondCount() > 0) {
//            for (int i = 0; i < structure.getBondCount(); i++) {
//                Bond jbond = structure.getBond(i);
//                int jaA = jbond.getAtomA().getOrder();
//                int jaB = jbond.getAtomB().getOrder();
//                CMLBond bond = new CMLBond(atomMap.get(jaA), atomMap.get(jaB));
//                bond.setOrder(BondType.convertToCMLBondType(jbond.getBondType()));
//                cmlMol.addBond(bond);
//            }
//        } else {
//            // calculate bonding
//            MoleculeTool moleculeTool = MoleculeTool.getOrCreateTool(cmlMol);
//            moleculeTool.calculateBondedAtoms();
//            moleculeTool.adjustBondOrdersToValency();
//        }
//        
//        cmlMol.setFormalCharge(null);
//        return cmlMol;
//    }
//
//    private static final class AtomWrapperForInChi {
//
//        private final Atom atom;
//
//        public AtomWrapperForInChi(Atom atom) {
//            this.atom = atom;
//        }
//
//        @Override
//        public boolean equals(Object obj) {
//            if (this == obj) {
//                return true;
//            }
//            if (!(obj instanceof AtomWrapperForInChi)) {
//                return false;
//            }
//            // Object must be Atom at this point
//            AtomWrapperForInChi atomW = (AtomWrapperForInChi) obj;
//            return this.atom.equalsIgnorePosition(atomW.atom);
//        }
//
//        @Override
//        public int hashCode() {
//            int hash = 7;
//            hash = 79 * hash + (this.atom != null ? this.atom.hashCode() : 0);
//            return hash;
//        }
//    }
}
