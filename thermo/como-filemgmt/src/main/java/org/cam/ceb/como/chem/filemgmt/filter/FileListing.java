/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.filter;

import java.io.File;
import java.util.Collection;
import java.util.Map;
import org.cam.ceb.como.tools.util.FileOperations;

/**
 *
 * @author pb556
 */
public class FileListing extends FileFilter {
            
    public FileListing() {
        super();
    }
    
    public FileListing(boolean recursive) {
        super(recursive);
    }
    
    public FileListing(File source) {
        super(source);
    }
    
    public FileListing(File source, boolean recursive) {
        super(source, recursive);
    }
    
    @Override
    public Collection<File> getValidFiles(File source, boolean recursive) throws Exception {
        return FileOperations.ls(source, recursive);
    }
    
    @Override
    public Collection<File> getValidFiles(String source, boolean recursive) throws Exception {
        return getValidFiles(new File(source), recursive);
    }
    
    @Override
    public String[] getValidExtensions() {
        return new String[] {"*"};
    }

    @Override
    public Collection<File> getValidFiles(File source, Map properties) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
