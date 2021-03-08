package gigadot.chom.chem.structure.tool;

import gigadot.chom.model.brownie.Atom;
import gigadot.chom.chem.structure.Bond;
import gigadot.chom.chem.structure.BondType;
import gigadot.chom.model.chemistry.Element;
import gigadot.chom.chem.structure.Structure;
import gigadot.chom.model.util.ModelUtils;
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

/**
 *
 * @author Weerapong
 */
public abstract class CompoundConverter {

    public static Logger logger = Logger.getLogger(CompoundConverter.class);

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
            CMLAtom atom = new CMLAtom(aid, ChemicalElement.getElement(jatom.getElement().atomicNumber));
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
