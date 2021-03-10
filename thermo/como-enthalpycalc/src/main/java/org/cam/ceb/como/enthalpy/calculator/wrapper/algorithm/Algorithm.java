/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public abstract class Algorithm {
    
    protected Map<Species, ReactionList> results;
    
    public abstract ReactionList solve(Species targetSpecies) throws Exception;
    public abstract void reset();
    
    public Map<Species, ReactionList> solve(Collection<Species> targetSpecies) throws Exception {
        results = new HashMap<Species, ReactionList>();
        for (Species s : targetSpecies) {
            results.put(s, solve(s));
        }
        return results;
    }
    
    public Map<Species, ReactionList> get() {
        return results;
    }
}
