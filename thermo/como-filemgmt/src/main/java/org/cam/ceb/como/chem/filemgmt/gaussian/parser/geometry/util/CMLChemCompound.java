/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util;

import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class CMLChemCompound {
    protected CMLMolecule compound = null;
    protected String path = null;
    
    public void setCMLMolecule(CMLMolecule compound) {
        this.compound = compound;
    }
    
    public void setPath(String path) {
        this.path = path;
    }
    
    public CMLMolecule getCMLCompound(){
        return this.compound;
    }
    
    public String getPath() {
        return this.path;
    }
}
