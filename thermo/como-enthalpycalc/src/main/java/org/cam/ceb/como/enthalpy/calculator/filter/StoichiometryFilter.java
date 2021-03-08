/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.tools.periodictable.Element;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class StoichiometryFilter extends SpeciesFilter {

    protected Map<Element, Integer> elements = null;
    protected RULE rule = RULE.EXACT;
    private Logger logger = Logger.getLogger(getClass());

    public enum RULE {

        EXACT, // all of the listed elements have to be included
        UPPER_THRESHOLD, // only a subset of the elements have to be included
        LOWER_THRESHOLD  // other elements are allowed as long as a subset of the given elements is included
    }
    
    public StoichiometryFilter() {
        super();
    }

    public StoichiometryFilter(Map<Element, Integer> elements, RULE rule) {
        super();
        this.elements = elements;
        this.rule = rule;
    }

    public StoichiometryFilter(Map<Element, Integer> elements, Collection<Species> species, RULE rule) {
        super(species);
        this.elements = elements;
        this.rule = rule;
    }
    
    public void setFilterProperties(Map<Element, Integer> elements, RULE rule) {
        this.elements = elements;
        this.rule = rule;
    }

    @Override
    public Collection<Species> filter() {
        switch (rule) {
            case EXACT:
                filterExact();
                break;
            case UPPER_THRESHOLD:
                filterThreshold(true);
                break;
            case LOWER_THRESHOLD:
                filterThreshold(false);
                break;
        }
        return validSpecies;
    }

    @Override
    public Collection<Species> filter(Collection<Species> species) {
        this.species = species;
        return filter();
    }
    
    protected void filterExact() {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                boolean isValid = true;
                for (Element element : s.getAtomMultiset().elementSet()) {
                    boolean validElement = false;
                    int eCtr = -1;
                    for (Element e1 : elements.keySet()) {
                        if (e1.getSymbol().compareToIgnoreCase(element.getSymbol()) == 0) {
                            validElement = true;
                            eCtr = elements.get(e1);
                            break;
                        }
                    }
                    if (validElement) {
                        if (eCtr != s.getAtomMultiset().count(element)) {
                            isValid = false;
                            break;
                        }
                    } else {
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
    
    protected void filterThreshold(boolean upper) {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                boolean isValid = true;
                for (Element element : s.getAtomMultiset().elementSet()) {
                    boolean validElement = false;
                    int eCtr = -1;
                    for (Element e1 : elements.keySet()) {
                        if (e1.getSymbol().compareToIgnoreCase(element.getSymbol()) == 0) {
                            validElement = true;
                            eCtr = elements.get(e1);
                            break;
                        }
                    }
                    if (validElement) {
                        if (upper) {
                            if (eCtr < s.getAtomMultiset().count(element)) {
                                isValid = false;
                                break;
                            }
                        } else {
                            if (eCtr > s.getAtomMultiset().count(element)) {
                                isValid = false;
                                break;
                            }
                        }
                    } else {
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
}
