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
public abstract class ChemFileWriter<T> implements WriterIntf<T> {

    protected T obj = null;
    protected String path = null;
    protected String hdr = null;
    protected String rhdr = null;

    public void setHeader(String hdr) {
        this.hdr = hdr;
    }

    public void setRestrictedHeader(String hdr) {
        this.rhdr = hdr;
    }
    
    @Override
    public void setPath(File dest) throws Exception {
        this.path = dest.getAbsolutePath();
    }

    @Override
    public void setPath(String dest) throws Exception {
        this.path = dest;
    }

    @Override
    public void set(T obj) throws Exception {
        //if (obj instanceof ChemCompound) {
        this.obj = obj;
        //if (((ChemCompound) this.obj).getCompound() instanceof CMLMolecule) {
        //return;
        //}
        //}
        //throw new Exception("Invalid data type. ChemCompound is requried containing a CMLMolecule object.");
    }
}
