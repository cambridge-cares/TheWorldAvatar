/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.util;

import org.xmlcml.cml.element.CMLAtom;

/**
 *
 * @author pb556
 */
public class SimpleAtom {

    protected String id;
    protected String element;
    protected double x;
    protected double y;
    protected double z;

    public SimpleAtom(String id, String element, double x, double y, double z) {
        this.id = id;
        this.element = element;
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    public String getId() {
        return id;
    }
    
    public String getElement() {
        return element;
    }
    
    public double getX() {
        return x;
    }
    
    public double getY() {
        return y;
    }
    
    public double getZ() {
        return z;
    }
    
    public boolean equals(SimpleAtom atom) {
        double tolerance = 0.0001;
        return id.compareToIgnoreCase(atom.getId()) == 0 && 
                element.compareToIgnoreCase(atom.getElement()) == 0 && 
                Math.abs(x - atom.getX()) < tolerance &&
                Math.abs(y - atom.getY()) < tolerance &&
                Math.abs(z - atom.getZ()) < tolerance;
    }
    
    public boolean equals(CMLAtom atom) {
        double tolerance = 0.01;
        return element.compareToIgnoreCase(atom.getElementType()) == 0 && 
                Math.abs(x - atom.getXYZ3().getArray()[0]) < tolerance &&
                Math.abs(y - atom.getXYZ3().getArray()[1]) < tolerance &&
                Math.abs(z - atom.getXYZ3().getArray()[2]) < tolerance;
    }
}
