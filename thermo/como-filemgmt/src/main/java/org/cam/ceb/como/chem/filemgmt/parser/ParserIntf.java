/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.parser;

import java.io.File;

/**
 *
 * @author pb556
 */
public interface ParserIntf<T> {
    public void set(File file) throws Exception;
    public void set(String path) throws Exception;
    public File getFile();
    public String getPath();
    public void parse() throws Exception;
    public T get();
}
