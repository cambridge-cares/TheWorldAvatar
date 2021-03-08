/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.filemgmt.gaussian.parser.compact.g09;

import com.cmclinnovations.io.file.filter.ls.ListingFileFilter;
import java.io.File;
import java.io.FileFilter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.log4j.Logger;
import org.junit.Test;

import uk.ac.cam.ceb.como.chem.structure.Compound;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.filter.extension.specific.G09FileFilter;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;

/**
 *
 * @author pb556
 */

public class G09CompactCompoundReader {

    private static Logger logger = Logger.getLogger(G09CompactCompoundReader.class);
    
    
    public static Map<String, Compound> getCompounds(String path) throws Exception {
    	
        ListingFileFilter g09Filter = new ListingFileFilter();

        // read the gaussian files
        Collection<File> g09Files = g09Filter.getValidFiles(path, true, new FileFilter[]{new G09FileFilter()});
        
        FrequencyParser parser = new FrequencyParser();
        
        Map<String, Compound> compound = new HashMap<String, Compound>();
        
        for (File f : g09Files) {
            parser.set(f.getAbsolutePath());
            try {
                parser.parse();
                CompChem cc = (CompChem) parser.get();
                CompChemParser ccParser = new CompChemParser();
                ccParser.parse(cc);
                compound.put(f.getAbsolutePath(), ccParser.get());
            } catch (Exception ex) {
                logger.warn("File " + f.getAbsolutePath() + " could not be read!");
                continue;
            }
        }
        return compound;
    }
}
