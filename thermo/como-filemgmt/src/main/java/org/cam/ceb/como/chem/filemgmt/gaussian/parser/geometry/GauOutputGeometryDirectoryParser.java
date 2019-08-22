/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util.ChemCompound;
import org.cam.ceb.como.chem.filemgmt.filter.FileFilter;
import org.cam.ceb.como.chem.filemgmt.filter.GauOutputFileFilter;
import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import java.io.File;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author pb556
 */
public class GauOutputGeometryDirectoryParser extends ChemFileParser {

    protected FileFilter fFilter = new GauOutputFileFilter();
    protected GauOutputGeometryParser parser = new GauOutputGeometryParser();
    
    protected List<ChemCompound> lObj = null;
    
    // set the filefilter and the file reader
    
    @Override
    public void parse() throws Exception {
        if (this.path == null) {
            throw new Exception("No file defined.");
        }
        Collection<File> files = this.fFilter.getValidFiles(this.path, true);
        if (files == null || files.isEmpty()) {
            throw new Exception("No files available.");
        }
        for (File f : files) {
            this.parser.set(f.getAbsolutePath());
            this.parser.parse();
            this.lObj.add((ChemCompound) this.parser.get());
        }
    }
    
    @Override
    public Object get() {
        return this.lObj;
    }
}