/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.MultiSelector;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.ReactionSelector;
import org.cam.ceb.como.enthalpy.calculator.species.Species;

/**
 *
 * @author pb556
 */
public class MultiSpeciesFilter extends SpeciesFilter {
    
    protected List<SpeciesFilter> filters = new ArrayList<SpeciesFilter>();
    private Logger logger = Logger.getLogger(MultiSelector.class);

    public MultiSpeciesFilter() {
        super();
    }
    
    public MultiSpeciesFilter(List<SpeciesFilter> filters) {
        super();
        this.filters = filters;
    }
    
    public void set(List<SpeciesFilter> filters) {
        this.filters = filters;
    }
    
    public boolean add(SpeciesFilter filter) {
        return filters.add(filter);
    }
    
    public void add(int index, SpeciesFilter filter) {
        filters.add(index, filter);
    }
    
    public boolean addAll(ArrayList<SpeciesFilter> filters) {
        return filters.addAll(filters);
    }
    
    public boolean addAll(int index, ArrayList<SpeciesFilter> filters) {
        return filters.addAll(index, filters);
    }
    
    public boolean remove(SpeciesFilter filter) {
        return filters.remove(filter);
    }
    
    public SpeciesFilter remove(int index) {
        return filters.remove(index);
    }
    
    public boolean removeAll(ArrayList<ReactionSelector> selectors) {
        return selectors.removeAll(selectors);
    }

    @Override
    public Collection<Species> filter() {
        return filter(this.species);
    }

    @Override
    public Collection<Species> filter(Collection<Species> species) {
        ArrayList<Species> selectedSpecies = new ArrayList<Species>();
        if (filters == null || filters.isEmpty()) {
            logger.warn("No filters are defined!");
        } else {
            selectedSpecies.addAll(species);
            for (SpeciesFilter filter : filters) {
                ArrayList<Species> buffer = new ArrayList<Species>();
                Collection<Species> valid = filter.filter(species);
                for (int i = 0; i < selectedSpecies.size(); i++) {
                    for (Species s : valid) {
                        if (selectedSpecies.get(i).equals(s, true)) {
                            buffer.add(selectedSpecies.get(i));
                            break;
                        }
                    }
                }
                selectedSpecies = new ArrayList<Species>(buffer);
            }
        }
        return selectedSpecies;
    }
}
