/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.openbabel.cml.format;

import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public abstract class Format {
    
    protected CMLMolecule mol = null;
    
    public void setCMLMolecule(final CMLMolecule mol) {
        this.mol = mol;
    }
    
    public abstract String getString() throws Exception;
}
