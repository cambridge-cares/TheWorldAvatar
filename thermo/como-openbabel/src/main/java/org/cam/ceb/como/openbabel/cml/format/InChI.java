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
public class InChI extends Format {
    
    @Override
    public String getString() throws Exception {
        if (this.mol == null) {
            Logger.getLogger(InChI.class.getName()).log(Level.SEVERE, 
                    "No CMLMolecule object is defined.", new IllegalArgumentException("Molecule is not defined."));
        }
        
        //return InChIConverter.getAutoInChI(this.mol);
        return OpenBabelCMLConverter.convert(this.mol.toXML(), "inchi", "");
//                for (String line : ) {
//                    if (line.startsWith("InChI")) {
//                        // order and sort the molecule into the correct folder which might need to be created
//                        inchi = line.replace("/", "_").replace("\\", "_");
//                        break;
//                    }
//                }
//                
//                // check for each top????
//                if (inchi == null) {
//                    throw new Exception("Top cannot be described.");
//                }
//                return inchi;
    }
}