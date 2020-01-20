/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

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
