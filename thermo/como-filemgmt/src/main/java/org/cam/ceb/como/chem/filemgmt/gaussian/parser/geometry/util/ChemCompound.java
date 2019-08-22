/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util;

import java.io.Serializable;

/**
 *
 * @author pb556
 */
public class ChemCompound  implements Serializable {
    protected Object compound = null;
    protected Object pos = null; //HashMap<Atom, Point3>
    
    protected String path = null;
    protected String id = null;

    protected int multiplicity = 0;
    protected int formalCharge = 0;
    
    public void setCompound(Object compound) {
        this.compound = compound;
    }
    
    public void setPath(String path) {
        this.path = path;
    }
    
    public void setID(String id) {
        this.id = id;
    }
    
    public void setPosition(Object pos) {
        this.pos = pos;
    }
    
    public void setSpinMultiplicity(int multiplicity) {
        this.multiplicity = multiplicity;
    }
    
    public void setFormalCharge(int charge) {
        this.formalCharge = charge;
    }
    
    public Object getCompound(){
        return this.compound;
    }
    
    public String getPath() {
        return this.path;
    }
    
    public String getID() {
        return this.id;
    }
    
    public Object getPosition() {
        return this.pos;
    }
    
    public int getSpinMultiplicity() {
        return this.multiplicity;
    }
    
    public int getFormalCharge() {
        return this.formalCharge;
    }
}
