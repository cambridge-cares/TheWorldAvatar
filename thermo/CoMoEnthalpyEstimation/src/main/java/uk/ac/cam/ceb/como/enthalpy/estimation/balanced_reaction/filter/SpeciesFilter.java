/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author pb556
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
