/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.filter;

import java.io.File;
import java.util.Collection;
import java.util.Map;

/**
 *
 * @author pb556
 */
public class GauOutputFileFilter extends FileFilter {
            
    public GauOutputFileFilter() {
        super();
    }
    
    public GauOutputFileFilter(boolean recursive) {
        super(recursive);
    }
    
    public GauOutputFileFilter(File source) {
        super(source);
    }
    
    public GauOutputFileFilter(File source, boolean recursive) {
        super(source, recursive);
    }
    
    @Override
    public String[] getValidExtensions() {
        return new String[] {"g03", "g09"};
    }

    @Override
    public Collection<File> getValidFiles(File source, Map properties) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
