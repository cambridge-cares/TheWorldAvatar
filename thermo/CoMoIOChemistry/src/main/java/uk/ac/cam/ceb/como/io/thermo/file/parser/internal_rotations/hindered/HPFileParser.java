/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered;

import uk.ac.cam.ceb.como.compchem.CompChem;
import java.io.File;
import uk.ac.cam.ceb.como.io.chem.file.parser.ChemFileParser;

/**
 *
 * @author pb556
 */
public abstract class HPFileParser extends ChemFileParser<CompChem> {
    
    public HPFileParser() {
    }
    
    public HPFileParser(File file) throws Exception {
        super.set(file);
    }
}
