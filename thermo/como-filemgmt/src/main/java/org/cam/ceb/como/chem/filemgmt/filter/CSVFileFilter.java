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
public class CSVFileFilter extends FileFilter {
        
    public CSVFileFilter() {
        super();
    }
    
    public CSVFileFilter(boolean recursive) {
        super(recursive);
    }
    
    public CSVFileFilter(File source) {
        super(source);
    }
    
    public CSVFileFilter(File source, boolean recursive) {
        super(source, recursive);
    }
    
    @Override
    public String[] getValidExtensions() {
        return new String[]{"csv"};
    }

    @Override
    public Collection<File> getValidFiles(File source, Map properties) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}