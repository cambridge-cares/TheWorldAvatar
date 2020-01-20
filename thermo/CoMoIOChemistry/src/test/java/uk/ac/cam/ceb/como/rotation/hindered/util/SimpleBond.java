/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.util;

import org.xmlcml.cml.element.CMLBond;

/**
 *
 * @author pb556
 */
public class SimpleBond {
    
    protected String id;
    protected SimpleAtom atomA;
    protected SimpleAtom atomB;
    protected String order;
    
    public SimpleBond(String id, String order, SimpleAtom atomA, SimpleAtom atomB) {
        this.id = id;
        this.order = order;
        this.atomA = atomA;
        this.atomB = atomB;
    }
    
    public String getId() {
        return id;
    }
    
    public String getOrder() {
        return order;
    }
    
    public SimpleAtom getAtomA() {
        return atomA;
    }
    
    public SimpleAtom getAtomB() {
        return atomB;
    }
    
    public boolean equals(SimpleBond bond) {
        if (atomA.equals(bond.getAtomA()) && atomB.equals(bond.getAtomB()) || 
                atomA.equals(bond.getAtomB()) && atomB.equals(bond.getAtomA())) {
            return id.compareToIgnoreCase(bond.getId()) == 0 && order.compareToIgnoreCase(bond.getOrder())== 0;
        }
        return false;
    }
    
    public boolean equals(CMLBond bond) {
        if (atomA.equals(bond.getAtom(0)) && atomB.equals(bond.getAtom(1)) || 
                atomA.equals(bond.getAtom(1)) && atomB.equals(bond.getAtom(0))) {
            return order.compareToIgnoreCase(bond.getOrder())== 0;
        }
        return false;
    }
}
