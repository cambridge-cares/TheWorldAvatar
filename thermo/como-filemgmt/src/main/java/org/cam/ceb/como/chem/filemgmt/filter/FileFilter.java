/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */
package org.cam.ceb.como.chem.filemgmt.filter;

import java.io.File;
import java.util.Collection;
import java.util.Map;

import org.cam.ceb.como.tools.util.FileOperations;

/**
 *
 * @author pb556
 * 
 */
public abstract class FileFilter {
    
    protected File source = null;
    protected boolean recursive = false;
    
    public FileFilter() {
    }
    
    public FileFilter(boolean recursive) {
        this.recursive = recursive;
    }
    
    public FileFilter(File source) {
        this.source = source;
    }
    
    public FileFilter(File source, boolean recursive) {
        this.source = source;
        this.recursive = recursive;
    }
    
    public File getSource() {
        return source;
    }
    
    public boolean isRecursiveSearch() {
        return recursive;
    }
    
    public void setSource(File source) {
        this.source = source;
    }
    
    public void setRecursiveSearch(boolean recursive) {
        this.recursive = recursive;
    }
    
    public Collection<File> getValidFiles() throws Exception {
        return FileOperations.ls(getSource(), isRecursiveSearch(), getValidExtensions());
    }
    
    public Collection<File> getValidFiles(File source) throws Exception {
        return FileOperations.ls(source, isRecursiveSearch(), getValidExtensions());
    }
    
    public Collection<File> getValidFiles(boolean recursive) throws Exception {
        return FileOperations.ls(getSource(), recursive, getValidExtensions());
    }
    
    public Collection<File> getValidFiles(File source, boolean recursive) throws Exception {
        return FileOperations.ls(source, recursive, getValidExtensions());
    }
    
    public Collection<File> getValidFiles(String source, boolean recursive) throws Exception {
        return getValidFiles(new File(source), recursive);
    }
    
    public abstract String[] getValidExtensions();
    
    public abstract Collection<File> getValidFiles(File source, Map properties) throws Exception;
}
