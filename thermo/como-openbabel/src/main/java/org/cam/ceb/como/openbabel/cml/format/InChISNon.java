/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.openbabel.cml.format;

import org.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pb556
 */
public class InChISNon extends Format {
    
    @Override
    public String getString() throws Exception {
        if (this.mol == null) {
            Logger.getLogger(InChISNon.class.getName()).log(Level.SEVERE, 
                    "No CMLMolecule object is defined.", new IllegalArgumentException("Molecule is not defined."));
        }
        return OpenBabelCMLConverter.convert(this.mol.toXML(), "inchi", "--SNon");
    }
}
