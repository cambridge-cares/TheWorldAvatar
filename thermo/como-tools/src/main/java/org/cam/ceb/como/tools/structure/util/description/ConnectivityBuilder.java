/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util.description;

import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.Molecule;
import gigadot.chom.chem.structure.Structure;
import org.xmlcml.cml.element.CMLBond;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class ConnectivityBuilder implements CompoundDescriptionBuilderIntf {

    protected String toBondString(CMLBond bond) {
        final String representationTemplate = "[%s]{%s}[%s]";

        if (bond.getAtom(0).getElementType().compareTo(bond.getAtom(1).getElementType()) < 0) {
            return String.format(representationTemplate, bond.getAtom(0).getElementType(), bond.getOrder(), bond.getAtom(1).getElementType());
        } else {
            return String.format(representationTemplate, bond.getAtom(1).getElementType(), bond.getOrder(), bond.getAtom(0).getElementType());
        }
    }
    
    
    @Override
    public String build(CMLMolecule mol) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public String build(Structure mol) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public String build(Molecule mol) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public String build(Compound mol) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
