/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io;

import java.io.File;

/**
 *
 * @author pb556
 */
public abstract class Writer {

    protected File file = null;

    public Writer() {
    }

    public Writer(String path) {
        file = new File(path);
    }

    public Writer(File file) {
        this.file = file;
    }

    public void setPath(String path) {
        file = new File(path);
    }

    public void setPath(File file) {
        this.file = file;
    }
    
    public abstract void write();
    public abstract void append();
}