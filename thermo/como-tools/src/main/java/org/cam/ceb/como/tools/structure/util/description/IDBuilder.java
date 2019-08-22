/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util.description;

import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.Molecule;
import gigadot.chom.chem.structure.Structure;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class IDBuilder implements CompoundDescriptionBuilderIntf {

    @Override
    public String build(CMLMolecule mol) {
        return mol.getId();
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
        return mol.getInchi();
    }
    
}
