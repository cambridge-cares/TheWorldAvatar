/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Bond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 */

public class RadicalsFilter extends SpeciesFilter {

    protected int num = -1;
    protected int lowerThreshold = -1;
    protected int upperThreshold = -1;
    private Logger logger = Logger.getLogger(getClass());

    public RadicalsFilter() {
        super();
    }
    
    public RadicalsFilter(int num) {
        super();
        this.num = num;
    }

    public RadicalsFilter(int lowerThreshold, int upperThreshold) {
        super();
        this.lowerThreshold = lowerThreshold;
        this.upperThreshold = upperThreshold;
    }

    public RadicalsFilter(Collection<Species> species, int num) {
        super(species);
        this.num = num;
    }

    public RadicalsFilter(Collection<Species> species, int lowerThreshold, int upperThreshold) {
        super(species);
        this.lowerThreshold = lowerThreshold;
        this.upperThreshold = upperThreshold;
    }

    public void setFilterProperties(int lowerThreshold, int upperThreshold) {
        this.lowerThreshold = lowerThreshold;
        this.upperThreshold = upperThreshold;
    }
    
    public void setFilterProperties(int num) {
        this.num = num;
    }
    
    @Override
    public Collection<Species> filter() {
        if (num >= 0) {
            filter(species, num);
        } else if (lowerThreshold >= 0 && upperThreshold >= 0) {
            filter(species, lowerThreshold, true);
            filter(validSpecies, upperThreshold, false);
        } else if (lowerThreshold >= 0) {
            filter(species, lowerThreshold, true);
        } else if (upperThreshold >= 0) {
            filter(species, upperThreshold, false);
        }
        return validSpecies;
    }

    @Override
    public Collection<Species> filter(Collection<Species> species) {
        this.species = species;
        return filter();
    }

    protected void filter(Collection<Species> species, int num, boolean lowerThreshold) {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                if (lowerThreshold && num <= getNumRadicalSites(s)) {
                    validSpecies.add(s);
                } else if (!lowerThreshold && num >= getNumRadicalSites(s)) {
                    validSpecies.add(s);
                } else {
                    invalidSpecies.add(s);
                }
            }
        } else {
            logger.error("No species are set!");
        }
    }

    protected void filter(Collection<Species> species, int num) {
        if (species != null) {
            validSpecies = new ArrayList<Species>();
            invalidSpecies = new ArrayList<Species>();
            for (Species s : species) {
                if (num == getNumRadicalSites(s)) {
                    validSpecies.add(s);
                } else {
                    invalidSpecies.add(s);
                }
            }
        } else {
        	
            logger.error("No species are set!");
            
        }
    }

    protected int getNumRadicalSites(Species s) {
        int ctr = 0;
        for (String ref : s.getAtomMap().keySet()) {
            int maxNumBonds = PeriodicTable.getNumberOfPossibleBonds(PeriodicTable.getElementBySymbol(s.getAtomMap().get(ref)));
            int ctrBonds = 0;
            for (Bond b : s.getBondMap()) {
                if (b.contains(ref)) {
                    ctrBonds++;
                }
            }
            ctr += maxNumBonds - ctrBonds;
        }
        return ctr;
    }
}