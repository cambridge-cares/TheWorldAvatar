/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 * 
 */
public class MultiRunCalculator extends ObjectPoolCalculator {

    protected ReactionSelector selector = null;
    protected ObjectPoolCalculator calculator = null;
    protected HashMap<Species, Collection<ReactionList>> result = null;
    protected int runs = 50;

    // use a ReactionSelector to keep consistency
    public MultiRunCalculator() {
        super();
    }

    public MultiRunCalculator(ObjectPoolCalculator calculator) {
        super(calculator.getSolver(), calculator.getFormat());
        result = new HashMap<Species, Collection<ReactionList>>();
        this.calculator = calculator;
        this.pool = this.calculator.getObjectPool();
    }

    public MultiRunCalculator(ObjectPoolCalculator calculator, SpeciesFilter filter) {
        super(calculator.getSolver(), calculator.getFormat(), filter);
        result = new HashMap<Species, Collection<ReactionList>>();
        this.calculator = calculator;
        this.pool = this.calculator.getObjectPool();
    }

    public MultiRunCalculator(ObjectPoolCalculator calculator, ReactionSelector selector) {
        super(calculator.getSolver(), calculator.getFormat());
        result = new HashMap<Species, Collection<ReactionList>>();
        this.calculator = calculator;
        this.pool = this.calculator.getObjectPool();
        this.selector = selector;
    }

    public MultiRunCalculator(ObjectPoolCalculator calculator, SpeciesFilter filter, ReactionSelector selector) {
        super(calculator.getSolver(), calculator.getFormat(), filter);
        result = new HashMap<Species, Collection<ReactionList>>();
        this.calculator = calculator;
        this.pool = this.calculator.getObjectPool();
        this.selector = selector;
    }

    public void setNumberOfRuns(int runs) {
        this.runs = Math.abs(runs);
    }

    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
    	
        if (pool == null) {
        	
            throw new Exception("No species pool has been set.");
        }
        
        HashMap<Species, Collection<ReactionList>> sol = new HashMap<Species, Collection<ReactionList>>();
        
        for (Species s : targetSpecies) {
        	
            ArrayList<ReactionList> rList = new ArrayList<ReactionList>();
            
            for (int i = 0; i < runs; i++) {
            	
                calculator.set(pool);
                
                calculator.calculate(s);
                
                HashMap<Species, ReactionList> res = (HashMap<Species, ReactionList>) calculator.get();
                
                for (Species sp : res.keySet()) {
                	
                    rList.add(res.get(sp));
                }
            }
            
            sol.put(s, rList);
        }
        
        result = sol;
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        HashSet<Species> set = new HashSet<Species>();
        set.add(targetSpecies);
        //System.out.println("Calculate " + targetSpecies.getRef());
        calculate(set);
    }

    @Override
    public void calculate() throws Exception {
    	
        calculate(pool.getInvalidatedObjects());
        
    }

    @Override
    public Object get() {
        
    	if (selector == null) {
    		
            return result;
        }
        
        HashMap<Species, Collection<ReactionList>> sol = new HashMap<Species, Collection<ReactionList>>();
        
        for (Species s : result.keySet()) {
        	
        sol.put(s, selector.select((List<ReactionList>) result.get(s)));
            
        }
        
        return sol;
    }

    public HashMap<Species, ReactionList> getCombinedResults() {
    	
        if (selector == null) {
        	
            //combine everything to a single ReactionList object
            HashMap<Species, ReactionList> sol = new HashMap<Species, ReactionList>();
            
            for (Species s : result.keySet()) {
            	
                ReactionList combined = new ReactionList();
                
                for (ReactionList rList : result.get(s)) {
                	
                    combined.addAll(rList);
                    
                }
                
                sol.put(s, combined);
            }
            
            return sol;
            
        }
        
        HashMap<Species, Collection<ReactionList>> sol = new HashMap<Species, Collection<ReactionList>>();
        for (Species s : result.keySet()) {
            sol.put(s, selector.select((List<ReactionList>) result.get(s)));
        }
        HashMap<Species, ReactionList> selectedSol = new HashMap<Species, ReactionList>();
        for (Species s : sol.keySet()) {
            ReactionList combined = new ReactionList();
            for (ReactionList rList : sol.get(s)) {
                combined.addAll(rList);
            }
            selectedSol.put(s, combined);
        }
        return selectedSol;
    }
}