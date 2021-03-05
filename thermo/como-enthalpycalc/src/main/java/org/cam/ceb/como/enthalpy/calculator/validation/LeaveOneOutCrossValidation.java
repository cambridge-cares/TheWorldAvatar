/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.ReactionSelector;
import org.cam.ceb.como.enthalpy.calculator.solver.SolverHelper;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.ObjectPoolCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 * @author pb556
 * 
 */

public class LeaveOneOutCrossValidation implements Runnable {

    protected ObjectPoolCalculator calculator = null;
    protected ReactionSelector selector = null;
    protected HashMap<Species, Double> results = null;
    protected HashMap<Species, Reaction> resultsDetailed = null;
    protected HashMap<Species, ReactionList> resultsDetailedComplete = null;
    protected ObjectPool<Species> pool = null;
    
    private Logger logger = Logger.getLogger(getClass());

    public LeaveOneOutCrossValidation(ObjectPool<Species> pool, ObjectPoolCalculator calculator, ReactionSelector selector) {
    	
        this.pool = pool;
        this.calculator = calculator;
        this.selector = selector;
        
    }

    public LeaveOneOutCrossValidation(ObjectPool<Species> pool, ObjectPoolCalculator calculator) {
    	
        this.pool = pool;
        this.calculator = calculator;
        
    }

    public void validate() {
    	
        this.run();
        
    }

    public Map<Species, Double> getValidationResults() {
    	
        return results;
        
    }

    public Map<Species, Reaction> getDetailedValidationResults() {
    	
        return resultsDetailed;
        
    }

    public Map<Species, ReactionList> getCompleteDetailedValidationResults() {
    	
        return resultsDetailedComplete;
        
    }

    @Override
    public void run() {
    	
        results = new HashMap<Species, Double>();
        
        resultsDetailed = new HashMap<Species, Reaction>();
        
        resultsDetailedComplete = new HashMap<Species, ReactionList>();
        
        ObjectPool<Species> orig = new ObjectPool<Species>();
        
        orig.addAll(pool.getValidatedObjects());
        
        HashMap<Species, Object> res = new HashMap<Species, Object>();
        
        int ctr = 0;
        
        System.out.println("orig.getValidatedObjects().size(): " + orig.getValidatedObjects().size());
        
        for (Object soi : orig.getValidatedObjects()) {
        	
            ctr++;
            
            ObjectPool<Species> clone = SolverHelper.clone(orig);
            
            Species selSOI = null;
            
            for (Object x : clone.getInvalidatedObjects()) {
            
            Species o = (Species) x;
                
            if (o.equals((Species) soi, true)) {
                
            selSOI = (Species) o;
                    
            break;
            
                }
            }
            
            if (selSOI == null) {
            
            	for (Object x : clone.getValidatedObjects()) {
                
            		Species o = (Species) x;
                    
            		if (o.equals((Species) soi, true)) {
                    
            			selSOI = (Species) o;
                        
            			break;
                    }
                }
            }
            
            if (selSOI != null) {
            	
                clone.invalidate(selSOI);
                
            } else {
            	
                clone.invalidate((Species) soi);
                
            }
            
            calculator.set(clone);
            
            try {
                
            	System.out.println("Process " + ctr + "/" + orig.getValidatedObjects().size());
            	
                calculator.calculate((Species) soi);
                
                HashMap<Species, ReactionList> sol = new HashMap<Species, ReactionList>();
                
                HashMap<Species, Object> buffer = (HashMap<Species, Object>) calculator.get();
                
                
                for (Species s : buffer.keySet()) {
                	
                if (buffer.get(s) instanceof ReactionList) {
                    	
                        sol.put(s, (ReactionList) buffer.get(s));
                        
                }
                    
                if (buffer.get(s) instanceof Collection) {
                    	
                        Collection c = (Collection) buffer.get(s);
                        
                        ReactionList combined = new ReactionList();
                        
                        for (Object o : c) {
                        	
                            if (o instanceof ReactionList) {
                            	
                                ReactionList b = (ReactionList) o;
                                
                                for (int i = 0; i < b.size(); i++) {
                                	
                                    combined.add(b.get(i));
                                }
                            }
                            
                            if (o instanceof Reaction) {
                            	
                                combined.add((Reaction) o);
                            }
                        }
                        
                        sol.put(s, combined);
                    }
                }
                
                res.put((Species) soi, calculator.get());
                
                for (Species s : sol.keySet()) {
                	
                    if (selector != null) {
                    	
                        try {
                            
                        	ReactionList valid = selector.select(sol.get(s));
                            
                        	results.put(s, s.getHf() - valid.get(0).calculateHf());
                            
                            resultsDetailed.put(s, valid.get(0));
                           
                            System.out.println(" valid.get(0).toString: " + valid.get(0).toString()+ " valid.get(0).calculateHf(): " + valid.get(0).calculateHf() );
                            
                        } catch (IndexOutOfBoundsException ioobe) {
                        	
                            logger.warn("No result obtained for species " + s.getRef());
                        }
                        
                    //System.out.println(s.getHf() - valid.get(0).calculateHf());
                        
                    } else {
                    	
                        try {
                        	
                            // use Median if ArrayList
                            results.put(s, s.getHf() - sol.get(s).get(0).calculateHf());
                            
                            resultsDetailed.put(s, sol.get(s).get(0));
                            
                            System.out.println("sol.get(s).get(0).toString(): " + sol.get(s).get(0).toString());
                            
                        } catch (IndexOutOfBoundsException ioobe) {
                        	
                            logger.warn("No result obtained for species " + s.getRef());
                            
                        }
                        
                    //System.out.println(s.getHf() - sol.get(s).get(0).calculateHf());
                    }
                    
                    resultsDetailedComplete.put(s, sol.get(s));

                }
                
                /**
                 * 
                 * @author nk510
                 * 
                 */
                
                for(Map.Entry<Species, ReactionList> rdc: resultsDetailedComplete.entrySet()) {
                	
                Species species = rdc.getKey();
                	
                System.out.println(" resultsDetailedComplete species name: " + species.getRef() + " species enthalpy taken from csv input file :  " + species.getHf());

//              System.out.println("rdc.getValue().get(0).calculateHf(): " + rdc.getValue().get(0).calculateHf() + " rdc.getValue().get(0).toString(): " + rdc.getValue().get(0).toString());
                	
//              System.out.println();
                	
                Collection<Reaction> rlc = rdc.getValue().getCollection();
                	
//              System.out.println("- - - - - - - -  - - - - - - - - - - - - - - - - - - - - - ");
                	
                int i =0;
                	
                	for(Reaction r: rlc) {
                		

                	
                	/**
                	 *  
                	 * Lists generated reaction and enthalpy estimated for that reaction.
                	 * 
                	 */
                		
                  	System.out.println("R_" +i+ ": " + rdc.getValue().get(i).toString() + "  enthalpy: " + rdc.getValue().get(i).calculateHf());
                  	
                  	i++;
                  	
                  	Map<Species, Double> reactants = r.getReactants();
                		
              		for(Map.Entry<Species, Double> react: reactants.entrySet()) {
                			
                			//System.out.println("reactant key " + react.getKey().getRef() + " reactant value: " + react.getValue());
               		}
                		
              		Map<Species, Double> products = r.getProducts();
                		
              		for(Map.Entry<Species, Double> prod: products.entrySet()) {
                			
                			//System.out.println("product key: " + prod.getKey().getRef() + "product value: " + prod.getValue());
                			
                		}
                	}
                }
                
            } catch (Exception ex) {
            	
                logger.error(ex.getMessage(), ex);
                
            }
        }
    }
    
//    create the k samples
//    what algorithm for the subsampling was chosen
//    protected Map<Integer, ObjectPool> createSubSets() {
//        pool.validateAll();
//        HashMap<Integer, ObjectPool> samples = new HashMap<Integer, ObjectPool>();
//        for (int i = 0; i < pool.getValidatedObjects().size(); i++) {
//            ObjectPool samplePool = new ObjectPool();
//            List l = (List) pool.getValidatedObjects();
//            for (int j = 0; j < pool.getValidatedObjects().size(); j++) {
//                if (i != j) {
//                    samplePool.add(l.get(j));
//                    samplePool.validateAll();
//                }
//            }
//            samples.put(i + 1, samplePool);
//        }
//        return samples;
//    }
//    protected Reaction getReaction(ReactionList list) {
//        double[] val = new double[list.size()];
//        Reaction[] r = new Reaction[list.size()];
//        for (int i = 0; i < list.size(); i++) {
//            val[i] = list.get(i).calculateHf();
//            r[i] = list.get(i);
//        }
//        // sort it
//        for (int i = 0; i < val.length; i++) {
//            for (int j = i + 1; j < val.length; j++) {
//                if (val[i] > val[j]) {
//                    // swap
//                    double m = val[i];
//                    Reaction rM = r[i];
//                    val[i] = val[j];
//                    r[i] = r[j];
//                    val[j] = m;
//                    r[j] = rM;
//                }
//            }
//        }
//
//        for (int i = 0; i < r.length; i++) {
//            System.out.println(r[i].calculateHf());
//        }
//        // get median (sort off)
//        int index = (int) (r.length / 2.0);
//        return r[index];
//    }
//    @Override
//    public void run() {       
//        ObjectPool orig = new ObjectPool();
//        orig.addAll(pool.getValidatedObjects());
//        //HashMap<Species, Model> results = new HashMap<Species, Model>();
//        for (Object soi : orig.getValidatedObjects()) {
//            ObjectPool clone = SolverHelper.clone(orig);
//            clone.invalidate((Species) soi);
//            try {
//                // create new instances
//                
//                // one calculator multiple jobs
//                
//                
//                
////                ObjectPoolCalculator calc = (ObjectPoolCalculator) calculator.getClass().newInstance();
////                LPSolver solver = calculator.getSolver().getClass().newInstance();
////                
////                // set properties
////                solver.setDirectory(calculator.getSolver().getDirectory());
////                solver.set(calculator.getSolver().isInputDeleted(), calculator.getSolver().isOutputDeleted());
////                
////                calc.setFormat(calculator.getFormat());
////                calc.setSolver(solver);
////                calc.setFilter(calculator.getFilter());
////                
////                calc.set(orig);
////                calc.calculate();
////                
////                results.put((Species) soi, (Model) calc.get());
//                
//            } catch (InstantiationException ex) {
//                java.util.logging.Logger.getLogger(LeaveOneOutCrossValidation.class.getName()).log(Level.SEVERE, null, ex);
//            } catch (IllegalAccessException ex) {
//                java.util.logging.Logger.getLogger(LeaveOneOutCrossValidation.class.getName()).log(Level.SEVERE, null, ex);
//            } catch (Exception ex) {
//                java.util.logging.Logger.getLogger(LeaveOneOutCrossValidation.class.getName()).log(Level.SEVERE, null, ex);
//            }
//        }
//    }
}