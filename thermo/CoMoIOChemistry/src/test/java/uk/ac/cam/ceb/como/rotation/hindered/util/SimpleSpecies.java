/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.util;

import java.util.List;

/**
 *
 * @author pb556
 */
public class SimpleSpecies {
    
    protected String id;
    protected List<SimpleAtom> atoms;
    protected List<SimpleBond> bonds;
    
    public SimpleSpecies(String id, List<SimpleAtom> atoms, List<SimpleBond> bonds) {
        this.id = id;
        this.atoms = atoms;
        this.bonds = bonds;
    }
    
    public String getId() {
        return id;
    }
    
    public List<SimpleAtom> getAtoms() {
        return atoms;
    }
    
    public List<SimpleBond> getBonds() {
        return bonds;
    }
}
