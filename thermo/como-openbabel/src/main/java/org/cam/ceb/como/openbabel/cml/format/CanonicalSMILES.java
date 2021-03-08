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
public class CanonicalSMILES extends Format {

    @Override
    public String getString() throws Exception {
        if (this.mol == null) {
            Logger.getLogger(CanonicalSMILES.class.getName()).log(Level.SEVERE, 
                    "No CMLMolecule object is defined.", new IllegalArgumentException("Molecule is not defined."));
        }
        return OpenBabelCMLConverter.convert(this.mol.toXML(), "can", "-I --unique"); // "-h --unique"
        //return OpenBabelConverter.convert(this.mol, OpenBabelConverter.OBFormat.CanonicalSMILES, "-I --unique");//"-h --unique");
    }
}