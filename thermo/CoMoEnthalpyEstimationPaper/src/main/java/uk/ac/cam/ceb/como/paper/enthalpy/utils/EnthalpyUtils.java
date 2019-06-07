/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionData;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionDataInterpreter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionDataList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */
public class EnthalpyUtils {
    
    public static Set<Reaction> extendCombined(Set<Reaction> reactions, boolean displayInfo) {
        Map<Species, Collection<Reaction>> extendedSet = extend(reactions, displayInfo);
        Set<Reaction> fullSet = new HashSet<>();
        for (Collection<Reaction> c : extendedSet.values()) {
            fullSet.addAll(c);
        }
        return fullSet;
    }
    
    public static Map<Species, Collection<Reaction>> extend(Set<Reaction> reactions, boolean displayInfo) {
        Map<Species, Collection<Reaction>> extendedSet = new HashMap<>();
        int ctr = 0;
        for (Reaction r : reactions) {
            if (displayInfo) {
                System.out.println("Process reaction " + ctr + " / " + reactions.size());    
                ctr++;
            }
            if (r == null) {
                continue;
            }
            boolean add = true;
            if (extendedSet.containsKey(r.getSpecies())) {
                for (Reaction r2 : extendedSet.get(r.getSpecies())) {
                    if (r.equals(r2)) {
                        add = false;
                        break;
                    }
                }
            }
            if (add) {
                Set<Species> allSpecies = new HashSet<>();
                allSpecies.addAll(r.getProducts().keySet());
                allSpecies.addAll(r.getReactants().keySet());
                allSpecies.remove(r.getSpecies());
                Map<Species, Double> products = r.getProducts();
                Map<Species, Double> reactants = r.getReactants();
                for (Species rS : allSpecies) {
                    Reaction rNew = new Reaction(rS);
                    for (Species product : products.keySet()) {
                        rNew.addProduct(product, products.get(product));
                    }
                    for (Species reactant : reactants.keySet()) {
                        rNew.addReactant(reactant, reactants.get(reactant));
                    }
                    if (!extendedSet.containsKey(rNew.getSpecies())) {
                        extendedSet.put(rNew.getSpecies(), new HashSet<Reaction>());
                    }
                    extendedSet.get(rNew.getSpecies()).add(rNew);
                }

            }
        }
        return extendedSet;
    }
    
     public static Map<Species, Collection<ReactionList>> convert(List<Species> species, ReactionDataList data) {
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(species);
        Map<ReactionData, Reaction> map = interpreter.get(data);
        Map<Species, List<ReactionList>> results = new HashMap<>();
        for (Reaction r : map.values()) {
            if (!results.containsKey(r.getSpecies())) {
                results.put(r.getSpecies(), new ArrayList<ReactionList>());
                results.get(r.getSpecies()).add(new ReactionList());
            }
            results.get(r.getSpecies()).get(0).add(r);
        }
        Map<Species, Collection<ReactionList>> retVal = new HashMap<>();
        for (Map.Entry<Species, List<ReactionList>> e : results.entrySet()) {
            retVal.put(e.getKey(), e.getValue());
        }
        return retVal;
    }
    
    public static Set<Reaction> extractReactions(List<Species> species, ReactionDataList data) {
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(species);
        Map<ReactionData, Reaction> map = interpreter.get(data);
        return new HashSet<>(map.values());
    }   

    public static Map<String, Set<Reaction>> getOrganisedReactions(Set<Reaction> reactions) {
        Map<String, Set<Reaction>> m = new HashMap<>();
        for (Reaction r : reactions) {
            if (!m.containsKey(r.getSpecies().getRef())) {
                m.put(r.getSpecies().getRef(), new HashSet<Reaction>());
            }
            m.get(r.getSpecies().getRef()).add(r);
        }
        return m;
    }
    
    public static Set<Reaction> read(File f, List<Species> refSpecies) throws Exception {
        ReactionListParser parser = new ReactionListParser(f);
        parser.parse();
        ReactionDataList list = (ReactionDataList) parser.get();
        return extractReactions(refSpecies, list);
    }
    
    public static Set<Reaction> getReactions(int num, List<Species> refSpecies, Collection<Reaction> reactions) {
        List<Reaction> list = new ArrayList<>(reactions);
        Set<Integer> covered = new HashSet<>();
        Set<String> allValidRefs = new HashSet<>();
        for (Species s : refSpecies) {
            allValidRefs.add(s.getRef());
        }
        Set<Reaction> ret = new HashSet<>();
        while (ret.size() < num) {
            int index = ThreadLocalRandom.current().nextInt(list.size());
            Reaction r = list.get(index);
            covered.add(index);
            boolean valid = true;
            for (Species s : r.getReactants().keySet()) {
                if (!allValidRefs.contains(s.getRef())) {
                    valid = false;
                    break;
                }
            }
            for (Species s : r.getProducts().keySet()) {
                if (!allValidRefs.contains(s.getRef())) {
                    valid = false;
                    break;
                }
            }
            if (valid) {
                ret.add(r);
            }
            if (covered.size() >= reactions.size()) {
                break;
            }
        }
        return ret;
    }
}
