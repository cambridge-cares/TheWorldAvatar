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
public interface CompoundDescriptionBuilderIntf {
    
    public String build(CMLMolecule mol);
    
    public String build(Structure mol);
    
    public String build(Molecule mol);
    
    public String build(Compound mol);
    
}
