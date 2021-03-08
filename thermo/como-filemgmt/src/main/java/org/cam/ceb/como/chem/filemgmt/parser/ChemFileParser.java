/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.parser;

import java.io.File;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public abstract class ChemFileParser<T> implements ParserIntf<T> {
    
    protected String path = null;
    protected T obj = null;
    
    protected Logger logger = Logger.getLogger(getClass());
    
    @Override
    public void set(File file) throws Exception {
        this.path = file.getAbsolutePath();
    }
    
    @Override
    public void set(String path) throws Exception {
        this.path = path;
    }
    
    @Override
    public File getFile() {
        return new File(path);
    }

    @Override
    public String getPath() {
        return path;
    }

    @Override
    public T get() {
        return this.obj;
    }
}
