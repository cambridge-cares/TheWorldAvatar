/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.file.writer;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 *
 * @author pb556
 */
public abstract class Writer {
    
    protected String path = null;
    protected Object content = null;
    protected boolean overwrite = false;
    protected Charset encoding = StandardCharsets.UTF_8;
    
    public void setPath(String path) {
        this.path = path;
    }
    
    public void setContent(Object content) {
        this.content = content;
    }
    
    public void setOverwrite(boolean allowOverwrite) {
        this.overwrite = allowOverwrite;
    }
    
    public void setEncoding(Charset encoding) {
        this.encoding = encoding;
    }
    
    public abstract void write() throws Exception;
    
    public abstract void append() throws Exception;
    
    public abstract void insert(int line) throws Exception;
    
    public abstract void clear() throws Exception;
    
    public abstract void delete(int start, int end) throws Exception;
    
    public abstract void delete(int start) throws Exception;
}
