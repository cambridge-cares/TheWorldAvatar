/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class HCOTorsionalBondIdentifier {

    protected CMLMolecule mol = null;
    protected String[][] bondDescription = null;
    private Logger logger = Logger.getLogger(getClass().getName());

    public HCOTorsionalBondIdentifier() {
    }

    public HCOTorsionalBondIdentifier(CMLBond[] bond) throws Exception {
        bondDescription = new String[bond.length][2];
        for (int i = 0; i < bond.length; i++) {
            bondDescription[i][0] = bond[i].getAtom(0).getElementType();
            bondDescription[i][1] = bond[i].getAtom(1).getElementType();
        }
    }

    public HCOTorsionalBondIdentifier(CMLMolecule mol, CMLBond[] bond) throws Exception {
        bondDescription = new String[bond.length][2];
        for (int i = 0; i < bond.length; i++) {
            bondDescription[i][0] = bond[i].getAtom(0).getElementType();
            bondDescription[i][1] = bond[i].getAtom(1).getElementType();
        }
        this.mol = mol;
    }

    public HCOTorsionalBondIdentifier(String[][] bondDescription) {
        this.bondDescription = bondDescription;
    }

    public HCOTorsionalBondIdentifier(CMLMolecule mol, String[][] bondDescription) {
        this.bondDescription = bondDescription;
        this.mol = mol;
    }

    public CMLMolecule getCMLMolecule() {
        return mol;
    }

    public String[][] getBondDescriptions() {
        return bondDescription;
    }

    public void setCMLMolecule(CMLMolecule mol) {
        this.mol = mol;
    }

    public void setBondDescriptions(String[][] bondDescription) throws Exception {
        this.bondDescription = bondDescription;
    }

    public void setBonds(CMLBond[] bond) throws Exception {
        bondDescription = new String[bond.length][2];
        for (int i = 0; i < bond.length; i++) {
            bondDescription[i][0] = bond[i].getAtom(0).getElementType();
            bondDescription[i][1] = bond[i].getAtom(1).getElementType();
        }
    }

    public Collection<CMLBond> identify() throws Exception {
        return identify(getCMLMolecule(), getBondDescriptions());
    }

    public Collection<CMLBond> identify(CMLMolecule mol) throws Exception {
        return identify(mol, getBondDescriptions());
    }

    public Collection<CMLBond> identify(CMLBond[] bond) throws Exception {
        return identify(getCMLMolecule(), bond);
    }

    public Collection<CMLBond> identify(String[][] bondDescription) throws Exception {
        return identify(getCMLMolecule(), bondDescription);
    }

    public Collection<CMLBond> identify(CMLMolecule mol, CMLBond[] bond) throws Exception {
        String[][] d = new String[bond.length][2];
        for (int i = 0; i < bond.length; i++) {
            d[i][0] = bond[i].getAtom(0).getElementType();
            d[i][1] = bond[i].getAtom(1).getElementType();
        }
        return identify(mol, d);
    }

    public Collection<CMLBond> identify(CMLMolecule mol, String[][] bondDescription) throws Exception {
        Collection<CMLBond> identifiedTorsBonds = new ArrayList<CMLBond>();
        for (CMLBond b : mol.getBonds()) {
            try {
                if (b.getOrder().compareToIgnoreCase("s") != 0 && b.getOrder().compareToIgnoreCase("1") != 0) {
                    continue;
                }
                for (int i = 0; i < bondDescription.length; i++) {
                    if ((b.getAtom(0).getElementType().compareToIgnoreCase(bondDescription[i][0]) == 0 && b.getAtom(1).getElementType().compareToIgnoreCase(bondDescription[i][1]) == 0)
                            || (b.getAtom(0).getElementType().compareToIgnoreCase(bondDescription[i][1]) == 0 && b.getAtom(1).getElementType().compareToIgnoreCase(bondDescription[i][0]) == 0)) {
                        identifiedTorsBonds.add(b);
                        break;
                    }
                }
            } catch (Exception ex) {
                logger.error("Problems occurred during the identifiation of torsional bonds in molecule " + mol.getId(), ex);
            }
        }
        return identifiedTorsBonds;
    }
}
