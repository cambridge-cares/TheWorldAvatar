/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.writer;

import java.io.File;

/**
 *
 * @author pb556
 */
public interface WriterIntf<T> {
    public void setPath(File dest) throws Exception;
    public void setPath(String dest) throws Exception;
    public void set(T obj) throws Exception;
    public void write() throws Exception;
}
