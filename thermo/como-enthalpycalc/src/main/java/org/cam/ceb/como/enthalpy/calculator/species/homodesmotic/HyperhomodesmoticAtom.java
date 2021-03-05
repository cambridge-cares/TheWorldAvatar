/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.species.homodesmotic;

import org.cam.ceb.como.tools.periodictable.Element;

/**
 *
 * @author pb556
 */
public class HyperhomodesmoticAtom {
    
    protected Element element;
    protected String id;
    
    public HyperhomodesmoticAtom(String id, Element element) {
        this.element = element;
        this.id = id;
    }
    
    public Element getElement(){
        return element;
    }
    
    public String getId() {
        return id;
    }
    
    public boolean equals(HyperhomodesmoticAtom atom) {
        return element.getSymbol().equals(atom.getElement().getSymbol()) && 
                id.equals(atom.getId());
    }
}
