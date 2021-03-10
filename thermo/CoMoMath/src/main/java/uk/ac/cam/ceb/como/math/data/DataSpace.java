/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data;

import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class DataSpace extends ArrayList<DataPoint> {
    
    protected Object reference = null;
    
    public DataSpace() {}
    
    public DataSpace(Object reference) {
        this.reference = reference;
    }
    
    public void setReference(Object reference) {
        this.reference = reference;
    }
    
    public Object getReference() {
        return reference;
    }
}
