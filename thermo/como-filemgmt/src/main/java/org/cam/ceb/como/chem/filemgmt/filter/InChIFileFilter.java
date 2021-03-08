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
public class InChIFileFilter extends FileFilter {
            
    public InChIFileFilter() {
        super();
    }
    
    public InChIFileFilter(boolean recursive) {
        super(recursive);
    }
    
    public InChIFileFilter(File source) {
        super(source);
    }
    
    public InChIFileFilter(File source, boolean recursive) {
        super(source, recursive);
    }
    
    @Override
    public String[] getValidExtensions() {
        return new String[] {"inchi"};
    }

    @Override
    public Collection<File> getValidFiles(File source, Map properties) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
