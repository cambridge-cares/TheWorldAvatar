/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.compact.g09;

import org.cam.ceb.como.chem.filemgmt.cml.parser.molecule.properties.CompChemParser;
import org.cam.ceb.como.chem.filemgmt.filter.G09FileFilter;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.FrequencyParser;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.compchem.CompChem;
import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class G09CompactCompoundReader {

    private static Logger logger = Logger.getLogger(G09CompactCompoundReader.class);
    
    public static Map<String, Compound> getCompounds(String path) throws Exception {
        G09FileFilter g09Filter = new G09FileFilter();

        // read the gaussian files
        Collection<File> g09Files = g09Filter.getValidFiles(path, true);
        FrequencyParser parser = new FrequencyParser();
        Map<String, Compound> compound = new HashMap<String, Compound>();
        for (File f : g09Files) {
            parser.set(f.getAbsolutePath());
            try {
                parser.parse();
                CompChem cc = (CompChem) parser.get();
                CompChemParser ccParser = new CompChemParser();
                ccParser.setCompChem(cc);
                ccParser.parse();
                compound.put(f.getAbsolutePath(), ccParser.get());
            } catch (Exception ex) {
                logger.warn("File " + f.getAbsolutePath() + " could not be read!");
                continue;
            }
        }
        return compound;
    }
}
