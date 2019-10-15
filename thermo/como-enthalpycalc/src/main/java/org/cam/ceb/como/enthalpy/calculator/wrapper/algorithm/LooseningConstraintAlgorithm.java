/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm;

import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.BondTypeFilter;
import org.cam.ceb.como.enthalpy.calculator.filter.ElementFilter;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.filter.RadicalsFilter;
import org.cam.ceb.como.enthalpy.calculator.filter.StoichiometryFilter;
import org.cam.ceb.como.enthalpy.calculator.wrapper.Calculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.cam.ceb.como.tools.periodictable.Element;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.filter.MultiSpeciesFilter;

/**
 *
 * @author pb556
 */
public class LooseningConstraintAlgorithm {
    // species pool
    // filter are defined internally
    // some filter parameter would be nice
    // calculator
    // some additional filtering constraints that could be defined

    protected ObjectPool pool = null;
    protected Calculator calculator = null;
    protected SpeciesFilter addConstraints = null;

    public LooseningConstraintAlgorithm() {
    }

    public LooseningConstraintAlgorithm(ObjectPool pool, Calculator calculator) {
        this.pool = pool;
        this.calculator = calculator;
    }

    public LooseningConstraintAlgorithm(ObjectPool pool, Calculator calculator, SpeciesFilter additionalConstraints) {
        this.pool = pool;
        this.calculator = calculator;
        this.addConstraints = additionalConstraints;
    }

    // one species after the other
    public ReactionList solve(Species targetSpecies) {

        int maxThreshold = 10;
        int step = 2;
        int i = 0;
        boolean noResult = true;
        int maxRuns = 10;

        // iteration through the individual steps
        while (noResult) {
            // validate and invalidate the individual species
            this.pool.validateAll();
            // apply the individual filter
            // maximum level of stoichiometry coefficients
            if (i == maxRuns) {
                break;
            }

            SpeciesFilter filters;
            if (i < maxThreshold) {
                filters = getPhase1Filter(targetSpecies, i, step);
            } else if (i - maxThreshold + 1 == 1) {
                filters = getPhase2Filter(targetSpecies);
            } else if (i - maxThreshold + 1 == 2) {
                filters = getPhase3Filter(targetSpecies);
            } else {
                filters = getPhase4Filter(targetSpecies, i - maxThreshold - 1);
            }
            i++;

            calculator.setSpeciesFilter(filters);

            try {
                // NEEDS TO BE FIXED!!!
//                ReactionList reaction = calculator.calculate(targetSpecies);
//                if (!reaction.isEmpty()) {
//                    return reaction;
//                }

            } catch (Exception ex) {
                if (i == maxRuns) {
                    noResult = false;
                }
            }
        }
        return null;
    }

    private SpeciesFilter getPhase1Filter(Species s, int index, int stepSize) {
        RadicalsFilter rFilter = new RadicalsFilter();
        StoichiometryFilter sLowerFilter = new StoichiometryFilter();
        StoichiometryFilter sUpperFilter = new StoichiometryFilter();
        ElementFilter eFilter = new ElementFilter();
        BondTypeFilter cFilter = new BondTypeFilter();

        Map<Element, Integer> sData = analyseComposition(s);

        Map<Element, Integer> lower = new HashMap<Element, Integer>();
        Map<Element, Integer> upper = new HashMap<Element, Integer>();

        for (Element e : sData.keySet()) {
            lower.put(e, sData.get(e) - (index + 1) * stepSize);
            upper.put(e, sData.get(e) + (index + 1) * stepSize);
        }

        Map<String, Integer> bData = analyseBonds(s);

        // adjust filter properties
        rFilter.setFilterProperties(0, 0);

        eFilter.setFilterProperties(sData.keySet(), ElementFilter.RULE.SUBSET);
        cFilter.setFilterProperties(bData.keySet(), BondTypeFilter.RULE.SUBSET);

        sLowerFilter.setFilterProperties(sData, StoichiometryFilter.RULE.LOWER_THRESHOLD);
        sUpperFilter.setFilterProperties(sData, StoichiometryFilter.RULE.UPPER_THRESHOLD);

        MultiSpeciesFilter filters = new MultiSpeciesFilter();
        filters.add(rFilter);
        filters.add(eFilter);
        filters.add(sLowerFilter);
        filters.add(sUpperFilter);
        filters.add(cFilter);
        
        return filters;
    }
    
    private SpeciesFilter getPhase2Filter(Species s) {
        RadicalsFilter rFilter = new RadicalsFilter();
        ElementFilter eFilter = new ElementFilter();

        Map<Element, Integer> sData = analyseComposition(s);

        rFilter.setFilterProperties(0, 0);

        eFilter.setFilterProperties(sData.keySet(), ElementFilter.RULE.SUBSET);
        
        MultiSpeciesFilter filters = new MultiSpeciesFilter();
        filters.add(rFilter);
        filters.add(eFilter);
        
        return filters;
    }
    
    private SpeciesFilter getPhase3Filter(Species s) {
        RadicalsFilter rFilter = new RadicalsFilter();
        rFilter.setFilterProperties(0, 0);

        MultiSpeciesFilter filters = new MultiSpeciesFilter();
        filters.add(rFilter);
        
        return filters;
    }
    
    private SpeciesFilter getPhase4Filter(Species s, int maxRadicalSites) {
        RadicalsFilter rFilter = new RadicalsFilter();
        rFilter.setFilterProperties(0, maxRadicalSites);

        MultiSpeciesFilter filters = new MultiSpeciesFilter();
        filters.add(rFilter);
        
        return filters;
    }

    public Map<Species, ReactionList> solve(Collection<Species> targetSpecies) {
        HashMap<Species, ReactionList> data = new HashMap<Species, ReactionList>();
        if (targetSpecies != null && !targetSpecies.isEmpty()) {
            for (Species s : targetSpecies) {
                data.put(s, solve(s));
            }
        }
        return data;
    }

    protected Map<Element, Integer> analyseComposition(Species s) {
        HashMap<Element, Integer> sData = new HashMap<Element, Integer>();
        for (Element e : s.getAtomMultiset().elementSet()) {
            sData.put(e, s.getAtomMultiset().count(e));
        }
        return sData;
    }

    protected Map<String, Integer> analyseBonds(Species s) {
        HashMap<String, Integer> sData = new HashMap<String, Integer>();
        for (String e : s.getBondTypeMultiset().elementSet()) {
            sData.put(e, s.getBondTypeMultiset().count(e));
        }
        return sData;
    }

    public ObjectPool getCompleteObjectPool() {
        return pool;
    }

    public ObjectPool getCurrentObjectPool() {
        return null;
    }

    public Calculator getCalculator() {
        return calculator;
    }

    public void setObjectPool(ObjectPool pool) {
        this.pool = pool;
    }

    public void setCalculator(Calculator calculator) {
        this.calculator = calculator;
    }
}
