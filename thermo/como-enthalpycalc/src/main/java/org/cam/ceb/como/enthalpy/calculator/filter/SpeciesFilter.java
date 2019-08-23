/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import java.util.ArrayList;
import java.util.Collection;

import org.cam.ceb.como.enthalpy.calculator.species.Species;

/**
 *
 * @author pb556
 * 
 */
public abstract class SpeciesFilter {

    protected Collection<Species> species;
    
    protected ArrayList<Species> validSpecies = null;
    protected ArrayList<Species> invalidSpecies = null;
    
    public SpeciesFilter() {}
    
    public SpeciesFilter(Collection<Species> species) {
        this.species = species;
    }
    
    public void set(Collection<Species> species) {
        this.species = species;
    }
    
    public void setSpecies(Collection<Species> species) {
        this.species = species;
    }
    
    public Collection<Species> getValidSpecies() {
        return validSpecies;
    }
    
    public Collection<Species> getInvalidSpecies() {
        return invalidSpecies;
    }
    
    public abstract Collection<Species> filter();
    
    public abstract Collection<Species> filter(Collection<Species> species);    
}
