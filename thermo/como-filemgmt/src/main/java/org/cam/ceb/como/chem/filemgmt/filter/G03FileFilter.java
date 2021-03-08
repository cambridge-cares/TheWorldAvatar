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
public class G03FileFilter extends FileFilter {
            
    public G03FileFilter() {
        super();
    }
    
    public G03FileFilter(boolean recursive) {
        super(recursive);
    }
    
    public G03FileFilter(File source) {
        super(source);
    }
    
    public G03FileFilter(File source, boolean recursive) {
        super(source, recursive);
    }
    
    @Override
    public String[] getValidExtensions() {
        return new String[] {"g03"};
    }

    @Override
    public Collection<File> getValidFiles(File source, Map properties) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
