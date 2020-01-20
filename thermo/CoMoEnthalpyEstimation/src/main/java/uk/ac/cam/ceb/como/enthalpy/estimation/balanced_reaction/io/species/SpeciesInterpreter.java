/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.species;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */
public class SpeciesInterpreter {
    
    protected Collection<Species> pool;
    
    public SpeciesInterpreter(Collection<Species> pool) {
        this.pool = pool;
    }
    
    public Species get(String speciesId) {
        for (Species s : pool) {
            if (s.getRef().trim().compareToIgnoreCase(speciesId.trim()) == 0) {
                return s;
            }
        }
        return null;
    }
    
    public Collection<Species> get(Collection<String> speciesIds) {
        ArrayList<Species> coll = new ArrayList<Species>();
        for (String s : speciesIds) {
            coll.add(get(s));
        }
        return coll;
    }
    
    public Map<String, Species> getAsMap(Collection<String> speciesIds) {
        HashMap<String, Species> m = new HashMap<String, Species>();
        for (String s : speciesIds) {
            m.put(s, get(s));
        }
        return m;
    }
}
