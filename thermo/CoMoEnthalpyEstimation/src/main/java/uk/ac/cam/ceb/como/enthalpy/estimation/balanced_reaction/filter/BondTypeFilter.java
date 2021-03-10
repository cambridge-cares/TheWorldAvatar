/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class BondTypeFilter extends SpeciesFilter {

    protected Collection<String> bondTypes = null;
    protected RULE rule = RULE.SUBSET;
    private Logger logger = Logger.getLogger(getClass());
    
    public enum RULE {

        COMPLETE, // all of the listed elements have to be included
        SUBSET, // only a subset of the elements have to be included
        OTHERS_ALLOWED  // other elements are allowed as long as a subset of the given elements is included
    }

    public BondTypeFilter() {
        super();
    }
    
    public BondTypeFilter(Collection<String> bondTypes, RULE rule) {
        super();
        this.bondTypes = bondTypes;
        this.rule = rule;
    }

    public BondTypeFilter(Collection<String> bondTypes, Collection<Species> species, RULE rule) {
        super(species);
        this.bondTypes = bondTypes;
        this.rule = rule;
    }
    
    public void setFilterProperties(Collection<String> bondTypes, RULE rule) {
        this.bondTypes = bondTypes;
        this.rule = rule;
    }
    
    @Override
    public Collection<Species> filter() {
        switch (rule) {
            case COMPLETE:
                filterComplete();
                break;
            case SUBSET:
                filterSubset();
                break;
            case OTHERS_ALLOWED:
                filterOthersAllowed();
                break;
        }
        return validSpecies;
    }

    @Override
    public Collection<Species> filter(Collection<Species> species) {
        this.species = species;
        return filter();
    }
    
    protected void filterComplete() {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                boolean isValid = true;
                for (String bT : s.getBondTypeMultiset().elementSet()) {
                    if (!bondTypes.contains(bT)) {
                        isValid = false;
                        break;
                    }
                }
                if (isValid) {
                    validSpecies.add(s);
                } else {
                    invalidSpecies.add(s);
                }
            }
        } else {
            logger.error("No species are set!");
        }
    }
    
    protected void filterSubset() {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                boolean isValid = false;
                for (String bT : s.getBondTypeMultiset().elementSet()) {
                    if (bondTypes.contains(bT)) {
                        isValid = true;
                        break;
                    } else {
                        isValid = false;
                    }
                }
                if (isValid) {
                    validSpecies.add(s);
                } else {
                    invalidSpecies.add(s);
                }
            }
        } else {
            logger.error("No species are set!");
        }
    }
    
    protected void filterOthersAllowed() {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                boolean isValid = false;
                for (String bT : s.getBondTypeMultiset().elementSet()) {
                    if (bondTypes.contains(bT)) {
                        isValid = true;
                        break;
                    }
                }
                if (isValid) {
                    validSpecies.add(s);
                } else {
                    invalidSpecies.add(s);
                }
            }
        } else {
            logger.error("No species are set!");
        }
    }
}
