/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.MultiRunCalculator;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 * @author pb556
 * 
 */
public class SpeciesPoolLeaveOneOutCrossValidation extends SpeciesPoolCrossValidation {
    
    public SpeciesPoolLeaveOneOutCrossValidation(ObjectPool<Species> pool, LPSolver solver, LPFormat format) {
        super(pool, solver, format);
    }

    @Override
    public void validate() throws Exception {
        
    	PoolModificationCalculator poolModCalc = new PoolModificationCalculator(numResults, solver, format);
        
        poolModCalc.setMaximumSearchDepth(maxDepth);
        
        MultiRunCalculator c =
                new MultiRunCalculator(
                poolModCalc);
        c.setNumberOfRuns(numRuns);
        
        results = new HashMap<Species, Collection<ReactionList>>();

        // calculation
        int ctr = 1;
        
        for (Species s : origPool.getValidatedObjects()) {
        	
            System.out.println("Processing species " + ctr + " / " + origPool.getValidatedObjects().size() + " (" + s.getRef() + ")");
            
            ctr++;
            
            c.set(getPool(s, true));
            
            try {
                c.calculate(s);
                
                Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) c.get();
                
                for (Species sR : r.keySet()) {
                	
                    results.put(s, r.get(sR));
                    
                }
                
            } catch (OutOfMemoryError e) {
            	
                System.gc();
            }
        }
        
      //Print reactants and products 
      for(Map.Entry<Species, Collection<ReactionList>> rTarget: results.entrySet()) {
       	 
      Species key = rTarget.getKey();
       	 
      System.out.println(" rTarget.getKey() - key.getRef(): " + key.getRef());
       	 
      Collection<ReactionList> rListValue = rTarget.getValue();
       	 
      for(ReactionList rList : rListValue) {
       		 
      Collection<Reaction> re = rList.getCollection();
       		 
      for(Reaction reac: re) {

       			 Map<Species, Double> reactants = reac.getReactants();
       			 
       			 for(Map.Entry<Species, Double> r1: reactants.entrySet()) {
       				 
       				 System.out.println(" r1.getKey().getRef(): " + r1.getKey().getRef() + " r1.getValue() : " + r1.getValue());
       			 }
       			 
       			 Map<Species, Double> products = reac.getProducts();
       			 
       			 for(Map.Entry<Species, Double> r2: products.entrySet()) {
       				 
       				 System.out.println(" r2.getKey().getRef(): " + r2.getKey().getRef() + " r2.getValue() : " + r2.getValue());
       			}
       		 }
       	 }
       } //map rTarget
        
    }

    @Override
    public ObjectPool<Species> getReducedSpeciesPool(double maxErr) throws Exception {
        validate();
        Map<Species, Integer[]> sum = validate(maxErr);
        Collection<Species> valid = new HashSet<Species>();
        for (Species s : sum.keySet()) {
            if (sum.get(s)[0] >= sum.get(s)[1]) {
                valid.add(s);
            }
        }
        return getPool(valid);
    }
}