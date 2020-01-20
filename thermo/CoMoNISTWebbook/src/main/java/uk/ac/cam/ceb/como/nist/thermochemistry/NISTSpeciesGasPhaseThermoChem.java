/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.thermochemistry;

import java.util.Collection;
import java.util.HashSet;

/**
 *
 * @author pb556
 */
public class NISTSpeciesGasPhaseThermoChem {

    protected Collection<NISTEnthalpy> enthalpies = new HashSet<NISTEnthalpy>();
    protected Collection<NISTEntropy> entropies = new HashSet<NISTEntropy>();
    protected Collection<NISTHeatCapacity> heatCapacities = new HashSet<NISTHeatCapacity>();
    
    public void setEnthalpies(Collection<NISTEnthalpy> enthalpies) {
        this.enthalpies = enthalpies;
    }
    
    public boolean addEnthalpy(NISTEnthalpy enthalpy) {
        return enthalpies.add(enthalpy);
    }
    
    public boolean addAllEnthalpies(Collection<NISTEnthalpy> enthalpies) {
        return this.enthalpies.addAll(enthalpies);
    }
    
    public boolean removeEnthalpy(NISTEnthalpy enthalpy) {
        return enthalpies.remove(enthalpy);
    }
    
    public boolean removeAllEnthalpies(Collection<NISTEnthalpy> enthalpies) {
        return this.enthalpies.removeAll(enthalpies);
    }
    
    public void clearEnthalpies() {
        enthalpies.clear();
    }
    
    public Collection<NISTEnthalpy> getEnthalpies() {
        return enthalpies;
    }
   
    public void setEntropies(Collection<NISTEntropy> entropies) {
        this.entropies = entropies;
    }
    
    public boolean addEntropy(NISTEntropy entropy) {
        return entropies.add(entropy);
    }
    
    public boolean addAllEntropies(Collection<NISTEntropy> entropies) {
        return this.entropies.addAll(entropies);
    }
    
    public boolean removeEntropy(NISTEntropy entropy) {
        return entropies.remove(entropy);
    }
    
    public boolean removeAllEntropies(Collection<NISTEntropy> entropies) {
        return this.entropies.removeAll(entropies);
    }
    
    public void clearEntropies() {
        entropies.clear();
    }
    
    public Collection<NISTEntropy> getEntropies() {
        return entropies;
    }
    
    public Collection<NISTHeatCapacity> getHeatCapacities() {
        return heatCapacities;
    }
    
    public void setHeatCapacities(Collection<NISTHeatCapacity> heatCapacities) {
        this.heatCapacities = heatCapacities;
    }
    
    public boolean addEnthalpy(NISTHeatCapacity heatCapacity) {
        return heatCapacities.add(heatCapacity);
    }
    
    public boolean addAllHeatCapacities(Collection<NISTHeatCapacity> heatCapacities) {
        return this.heatCapacities.addAll(heatCapacities);
    }
    
    public boolean removeEnthalpy(NISTHeatCapacity heatCapacity) {
        return heatCapacities.remove(heatCapacity);
    }
    
    public boolean removeAllHeatCapacities(Collection<NISTHeatCapacity> heatCapacities) {
        return this.heatCapacities.removeAll(heatCapacities);
    }
    
    public void clearHeatCapacities() {
        heatCapacities.clear();
    }
}
