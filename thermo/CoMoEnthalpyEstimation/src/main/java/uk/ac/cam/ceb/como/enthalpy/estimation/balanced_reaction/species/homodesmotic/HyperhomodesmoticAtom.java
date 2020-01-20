/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic;

import uk.ac.cam.ceb.como.chem.periodictable.Element;

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
