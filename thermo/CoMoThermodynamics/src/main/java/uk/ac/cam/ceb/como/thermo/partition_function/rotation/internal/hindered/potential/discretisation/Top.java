/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.chem.structure.util.StructuralCompoundComparison;

/**
 *
 * @author pb556
 */
public class Top {

    protected String id = "";
    // reference geometry
    //protected CompChem ref;
    protected CMLMolecule ref;
    // rotational atoms
    protected Collection<CMLAtom> atoms;
    // torsional bond
    protected CMLBond bond;
    private Logger logger = Logger.getLogger(getClass().getName());

    public Top(CMLMolecule reference, Collection<CMLAtom> atoms, CMLBond bond) {
        ref = reference;
        this.atoms = atoms;
        this.bond = bond;
    }
    
    public Top(String id, CMLMolecule reference, Collection<CMLAtom> atoms, CMLBond bond) {
        ref = reference;
        this.atoms = atoms;
        this.bond = bond;
        this.id = id;
    }

    public CMLMolecule getReferenceGeometry() {
        return ref;
    }

    public Collection<CMLAtom> getAtoms() {
        return atoms;
    }

    public CMLBond getBond() {
        return bond;
    }

    public CMLMolecule asCMLMolecule() throws Exception {
        if (getReferenceGeometry() == null) {
            logger.error("No reference geometry has been defined!", new Exception("No reference geometry has been defined."));
        }

        if (getAtoms() == null || getAtoms().isEmpty()) {
            logger.error("No top has been defined!", new Exception("No top has been defined."));
        }

        // conversion of the given top into a CMLMolecule object        
        CMLMolecule top = new CMLMolecule(getReferenceGeometry());

        ArrayList<CMLAtom> invalidAtoms = new ArrayList<CMLAtom>();
        ArrayList<CMLBond> invalidBonds = new ArrayList<CMLBond>();

        // remove what is not required
        for (CMLAtom b : top.getAtoms()) {
            boolean invalid = true;
            for (CMLAtom a : getAtoms()) {
                if (b.getId().compareToIgnoreCase(a.getId()) == 0) {
                    invalid = false;
                    break;
                }
            }
            if (invalid) {

                // only top sided atom!
                //if ((b.getId().compareToIgnoreCase(getBond().getAtomId(0)) != 0) && (b.getId().compareToIgnoreCase(getBond().getAtomId(1)) != 0)) {
                    invalidAtoms.add(b);
                //} else {
                    // check if top sided
//                    if (!isTopSideAtom(b)) {
//                        invalidAtoms.add(b);
//                    }
                //}
            }
        }

        for (CMLBond b : top.getBonds()) {
            boolean invalid = true;
            for (CMLAtom a : getAtoms()) {
                if ((b.getAtom(0).getId().compareToIgnoreCase(a.getId()) == 0)
                        || (b.getAtom(1).getId().compareToIgnoreCase(a.getId()) == 0)) {
                    invalid = false;
                    break;
                }
            }
            if (invalid) {
                invalidBonds.add(b);
            }
        }

        for (int i = 0; i < invalidAtoms.size(); i++) {
            CMLAtom delAtom = top.getAtomById(invalidAtoms.get(i).getId());
            if (delAtom != null) {
                top.deleteAtom(delAtom);
            }
        }

        for (int i = 0; i < invalidBonds.size(); i++) {
            CMLBond delBond = top.getBondById(invalidBonds.get(i).getId());
            if (delBond != null) {
                top.deleteBond(delBond);
            }
        }

        // does not work
        //top.getAtoms().removeAll(invalidAtoms);
        //top.getBonds().removeAll(invalidBonds);

        //String emp = CompoundConverter.convertToEmpiricalFormulaFromCML(top);

        return top;
    }

    public CMLAtom getTopSideAtom() {
        // which side is the top?
        if (isTopSideAtom(getBond().getAtom(0))) {
            return getBond().getAtom(0);
        }
        if (isTopSideAtom(getBond().getAtom(1))) {
            return getBond().getAtom(1);
        }
        return null;
    }
    
    public void setId(String id) {
        this.id = id;
    }
    
    public String getId() {
        return id;
    }
    
    @Override
    public String toString() {
        return id;
    }
    
    protected boolean isTopSideAtom(CMLAtom topSide) {
        for (CMLBond b : getReferenceGeometry().getBonds()) {
            CMLAtom opponent = null;
            if (b.getAtom(0).getId().compareToIgnoreCase(topSide.getId()) == 0) {
                opponent = b.getAtom(1);
            }
            if (b.getAtom(1).getId().compareToIgnoreCase(topSide.getId()) == 0) {
                opponent = b.getAtom(0);
            }
            if (opponent != null) {
                // check if opponent is within top
                for (CMLAtom a : getAtoms()) {
                    if (opponent.getId().compareToIgnoreCase(a.getId()) == 0) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    
    public boolean isSimilar(Top top) {
        boolean equal = StructuralCompoundComparison.isEqual(getReferenceGeometry(), top.getReferenceGeometry()) &&
                StructuralCompoundComparison.isEqual(getBond(), top.getBond()) && getAtoms().size() == top.getAtoms().size();
        for (CMLAtom atom : getAtoms()) {
            boolean identified = false;
            for (CMLAtom cmpAtom : top.getAtoms()) {
                if (atom.getId().compareToIgnoreCase(cmpAtom.getId()) == 0)  {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                return false;
            }
        }
        return equal;
    }
}
