/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.tools.periodictable.Element;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class ElementFilter extends SpeciesFilter {

    protected Collection<Element> elements = null;
    protected RULE rule = RULE.SUBSET;
    private Logger logger = Logger.getLogger(getClass());

    public enum RULE {

        COMPLETE, // all of the listed elements have to be included
        SUBSET, // only a subset of the elements have to be included
        OTHERS_ALLOWED  // other elements are allowed as long as a subset of the given elements is included
    }

    public ElementFilter() {
        super();
    }
    
    public ElementFilter(Collection<Element> elements, RULE rule) {
        super();
        this.elements = elements;
        this.rule = rule;
    }

    public ElementFilter(Collection<Element> elements, Collection<Species> species, RULE rule) {
        super(species);
        this.elements = elements;
        this.rule = rule;
    }
    
    public void setFilterProperties(Collection<Element> elements, RULE rule) {
        this.elements = elements;
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
                for (Element element : s.getAtomMultiset().elementSet()) {
                    if (!elements.contains(element)) {
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
                for (Element element : s.getAtomMultiset().elementSet()) {
                    if (elements.contains(element)) {
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
                for (Element element : s.getAtomMultiset().elementSet()) {
                    if (elements.contains(element)) {
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
