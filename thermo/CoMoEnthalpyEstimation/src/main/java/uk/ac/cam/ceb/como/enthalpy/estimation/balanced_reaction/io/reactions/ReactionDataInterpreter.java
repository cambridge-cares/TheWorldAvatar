/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.species.SpeciesInterpreter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;

/**
 *
 * @author pb556
 */
public class ReactionDataInterpreter {
    
    private Logger logger = Logger.getLogger(getClass());
    //protected Collection<Species> species = null;
    protected SpeciesInterpreter spInterpreter;
    
    public ReactionDataInterpreter(Collection<Species> pool) {
        spInterpreter = new SpeciesInterpreter(pool);
    }
    
    public Reaction get(ReactionData data) {
        Map<Species, Double> reactants = get(data.getReactants());
        Map<Species, Double> products = get(data.getProducts());
        Species targetSpecies = get(data.getTargetSpecies());
        if (reactants == null || products == null || targetSpecies == null) {
            logger.error("Mapping process incomplete because of invalid reaction data...");
            return null;
        }
        Reaction r = new Reaction(targetSpecies);
        for (Species s : reactants.keySet()) {
            r.addReactant(s, reactants.get(s));
        }
        for (Species s : products.keySet()) {
            r.addProduct(s, products.get(s));
        }
        return r;
    }
    
    public Map<ReactionData, Reaction> get(Collection<ReactionData> data) {
        Map<ReactionData, Reaction> reactions = new HashMap<ReactionData, Reaction>();
        for (ReactionData d : data) {
            reactions.put(d, get(d));
        }
        return reactions;
    }
    
    public Species get(String ref) {
//        for (Species s : species) {
//            if (s.getRef().trim().compareToIgnoreCase(ref.trim()) == 0) {
//                return s;
//            }
//        }
//        return null;
        return spInterpreter.get(ref);
    }
    
    public Map<Species, Double> get(Map<String, Double> species) {
        Map<Species, Double> mappedSpecies = new HashMap<Species, Double>();
        for (String ref : species.keySet()) {
            Species s = get(ref);
            if (s == null) {
                return null;
            }
            mappedSpecies.put(s, species.get(ref));
        }
        return mappedSpecies;
    }
}
