/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions;

import java.util.HashMap;

/**
 *
 * @author pb556
 */
public class ReactionData {

    private HashMap<String, Double> reactants = new HashMap<String, Double>();
    private HashMap<String, Double> products = new HashMap<String, Double>();
    private String targetSpecies = null;

    public ReactionData() {
    }

    public ReactionData(HashMap<String, Double> reactants, HashMap<String, Double> products, String targetSpecies) {
        this.reactants = reactants;
        this.products = products;
        this.targetSpecies = targetSpecies;
    }

    public void set(HashMap<String, Double> reactants, HashMap<String, Double> products, String targetSpecies) {
        this.reactants = reactants;
        this.products = products;
        this.targetSpecies = targetSpecies;
    }

    public HashMap<String, Double> getReactants() {
        return reactants;
    }

    public HashMap<String, Double> getProducts() {
        return products;
    }

    public String getTargetSpecies() {
        return targetSpecies;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof ReactionData) {
            ReactionData obj = (ReactionData) o;
            boolean equal = true;
            equal &= obj.getProducts().size() == products.size() 
                    && obj.getReactants().size() == reactants.size() 
                    && targetSpecies.compareToIgnoreCase(obj.getTargetSpecies()) == 0;
            for (String ref : reactants.keySet()) {
                boolean identified = false;
                String refObj = "";
                for (String ref2 : obj.getReactants().keySet())  {
                    if (ref.compareToIgnoreCase(ref2) == 0) {
                        refObj = ref2;
                        identified = true;
                        break;
                    }
                }
                equal &= identified;
                if (!equal) {
                    return false;
                }
                equal &= Math.abs(obj.getReactants().get(refObj) - reactants.get(ref)) < 0.0001;
            }
            for (String ref : products.keySet()) {
                boolean identified = false;
                String refObj = "";
                for (String ref2 : obj.getProducts().keySet())  {
                    if (ref.compareToIgnoreCase(ref2) == 0) {
                        refObj = ref2;
                        identified = true;
                        break;
                    }
                }
                equal &= identified;
                if (!equal) {
                    return false;
                }
                equal &= Math.abs(obj.getProducts().get(refObj) - products.get(ref)) < 0.0001;
            }
            return true;
        }
        return false;
    }
}
