/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.openbabel.cml.format;

import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pb556
 */
public class SMILES extends Format {

    @Override
    public String getString() throws Exception {
        if (this.mol == null) {
            Logger.getLogger(SMILES.class.getName()).log(Level.SEVERE, 
                    "No CMLMolecule object is defined.", new IllegalArgumentException("Molecule is not defined."));
        }
        return OpenBabelCMLConverter.convert(this.mol.toXML(), "smi", ""); // "-h --unique"

//.convert(this.mol, OpenBabelConverter.OBFormat.SMILES, "-I --unique");
    }
}
