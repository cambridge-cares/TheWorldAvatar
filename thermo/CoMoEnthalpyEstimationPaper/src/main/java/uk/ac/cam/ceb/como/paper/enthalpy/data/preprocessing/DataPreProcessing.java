package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
*  
* @author nk510 ( caresssd@hermes.cam.ac.uk )
* @author am2145( am2145@cam.ac.uk )
* 
* - This code does pre- processing step in cross validation for selected Ti-based species.
* - In this code inputs are: 
*    - A set of Ti-based species (Gaussian files) given as reference list. 
*    - A list of 25 target species  for which enthalpies are given. This list is given as csv file. Comments: List of target and reference species should be the same.
*    - Maximum error that is compared with error for each generated reaction. If error  for each reaction is smaller that maximum error then that reaction is valid. Otherwise, that reaction is invalid. 
*    - Number of runs is set to one, and number of allowed reactions that will be generated.
*    - ISG reaction type
* - Calculates EBRs for reference set of species (srcRefPool) by using Gaussian files of the same set of species.
* - Calculates difference  (error) between estimated enthalpy of formation for each species in each generated reaction and reference enthalpy of selected species.
* - Calculates error bar for each species by using errors for each generated reaction.
* - Generates list of valid reactions.
* - Generates list of invalid reactions.
* - Generates an initial list of valid species
* - Generates an initial list of invalid species.
* - Reports a species name and its maximum error bar.
* 
*/

public class DataPreProcessing {
	
	Collection<Species> invalids = new HashSet<>();
    
    Map<Species, Integer> spinMultiplicity = new HashMap<>();
    
    Set<Species> validSpecies = new HashSet<Species>();
	
	Set<Species> invalidSpecies = new HashSet<Species>();

	Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();
	
	Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();
	
    public void getPreProcessingCorssValidation(int timeout, int maxErr, String destRList, int[] ctrRadicals, int[] ctrRuns,  int[] ctrRes, List<Species> refSpecies,  LPSolver solver ) throws Exception {
    	
    	for (int z = 0; z < ctrRadicals.length; z++) {
        	
            int maxRadical = ctrRadicals[z];
            
             timeout = 1500; // remove this when call this method
        
        for (int i = 0; i < ctrRuns.length; i++) {
        
        	for (int k = 0; k < ctrRes.length; k++) {
            
        		String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                
        		System.out.println("Process configuration " + config);
                
                if (new File(destRList + "\\" + config + ".txt").exists()) {
                	
                    System.out.println("Skipping " + destRList + "\\" + config);
                    
                    continue;
                }
                
                Collections.shuffle(refSpecies);
                
                int ctr = 1; //added int 
                
                for (Species target : refSpecies) {

                	
                    Map<Species, Collection<ReactionList>> results = new HashMap<>();
                    
                    System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + refSpecies.size() + ")");
                
                    ctr++;

                    if (new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
                    
                    	continue;
                    }

                    List<Species> refPool = new ArrayList<>();
                    
                    refPool.addAll(refSpecies);
                    
                    refPool.remove(target);
                    
                    // filter for radicals
                    for (Species sSpin : spinMultiplicity.keySet()) {
                    	
                        try {
                        	
                            if (spinMultiplicity.get(sSpin) != null && spinMultiplicity.get(sSpin) - 1 > maxRadical) {
                            	
                                refPool.remove(sSpin);
                                
                            }
                            
                        } catch (NullPointerException ex) {
                        	
                        }
                    }
                    
                    Collections.shuffle(refPool);
                    
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    
                    PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISGReactionType(true)));
                    
                    poolModCalc.setMaximumSearchDepth(50);
                    
                    MultiRunCalculator c = new MultiRunCalculator(poolModCalc);
                    
                    c.setNumberOfRuns(ctrRuns[i]);
                    
                    EnthalpyEstimationThread t = new EnthalpyEstimationThread(c, target, EvaluationUtils.getPool(refPool, true));
                    
                    Future<Map<Species, Collection<ReactionList>>> future = executor.submit(t);
                    
                    try {
                    
                    	try {
                        
                    		Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) future.get(timeout, TimeUnit.SECONDS);
                            
                    		if (r != null) {
                            
                    			for (Species sR : r.keySet()) {
                                
                    				results.put(target, r.get(sR));
                                }
                    			
                            } else {
                            	    
                                r = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                                
                                if (r != null) {
                                	
                                    for (Species sR : r.keySet()) {
                                    	
                                        results.put(target, r.get(sR));
                                    }
                                    
                                } else {
                                	
                                    results.put(target, null);
                                    
                                }
                            }
                    		
                        } catch (TimeoutException | InterruptedException | ExecutionException e) {
                        	
                            System.out.println("Terminated!");
                            
                            Map<Species, Collection<ReactionList>> re = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                            
                            if (re != null) {
                            	
                                for (Species sR : re.keySet()) {
                                	
                                    results.put(target, re.get(sR));
                                    
                                }
                                
                            } else {
                            	
                                results.put(target, null);
                                
                            }
                        }

                        ReactionList completeRList = new ReactionList();
                        
                        Collection<Species> ttipSpecies = new HashSet<>();
                        
                        for (Species s : results.keySet()) {
                        
                        	System.out.println("Ref species name: " + s.getRef()+ "  ref species Hf: " + s.getHf());
                        	
                        	try {
                        		
                                ReactionList rList = new ReactionList();
                                
                                for (ReactionList l : results.get(s)) {
                                	
                                    rList.addAll(l);
                                }
                                
                              completeRList.addAll(rList);
                              
                              ReactionSelector selector = new MedianReactionSelector();
                              
                              List<Reaction> reactionList = selector.select(rList);
                              
                              System.out.println("reactionList.size(): " + reactionList.size());
                              
                              for(Reaction r : reactionList) {
                            	  
                            	  System.out.println("Species name for reaction list : " + r.getSpecies().getRef());
                              }
                              
                              ttipSpecies.add(s);
                                
                            } catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
                            
                            	System.out.println("No data were calculated for " + s.getRef());
                                
                            }
                        }

                        if(!new File(destRList + "\\" + target.getRef() + "\\").exists()) {
                        	
                        new File(destRList + "\\" + target.getRef() + "\\").mkdirs();
                        
                        }

                        ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));                            
                        
                        /**
                         * 
                         * Added ctr in species pool median name.
                         * 
                         */
                        
                        SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_species-pool_median_"+ctr+".csv"));
                        
                        
                        if (!completeRList.isEmpty()) {
                        	
                        	
                            System.out.println("Writting complete reaction list...");
                            
                            rListWriter.set(completeRList);
                            
                            rListWriter.overwrite(true);
                            
                            rListWriter.write();
                            
                            for(int ri=0; ri<completeRList.size();ri++) {
                             
                            double error = Math.abs(completeRList.get(ri).getSpecies().getHf()-completeRList.get(ri).calculateHf());
                            
                        	System.out.println( " Reaction("+ri+"): " + completeRList.get(ri).toString() + " Species (ref) enthalpy: " + completeRList.get(ri).getSpecies().getHf() + " Calculated Hf for reaction("+ri+"): " + completeRList.get(ri).calculateHf() );
                           
                        	if(error<maxErr) {
                            	
                            validReaction.put(completeRList.get(ri), error);
                            	
                            if(!validSpecies.contains(completeRList.get(ri).getSpecies())){
                            		
                            validSpecies.add(completeRList.get(ri).getSpecies());
                            
                            }
                            
                            }else {
                            	
                            	invalidReaction.put(completeRList.get(ri), error);
                            	
                            	if(!invalidSpecies.contains(completeRList.get(ri).getSpecies())) {
                            	
                            		invalidSpecies.add(completeRList.get(ri).getSpecies());
                            	}
                            }
                            
                            }                         
                        }
                        
                        if(!ttipSpecies.isEmpty()){
                        	
                        	for(Species sp: ttipSpecies) {
                                
                            
                        		/**
                            	 * 
                            	 * Median enthalpy and species name.
                            	 * 
                            	 */
                            	
                            System.out.println("[Species name: " + sp.getRef() + "  Species (reference) enthalpy: " + sp.getHf() + " ]" );
                            
                            }
                        	
                            System.out.println("Writting species list...");
                            
                            spWriter.set(ttipSpecies, false);
                            
                            spWriter.write();
                            
                        }
                        
                    } catch (OutOfMemoryError e) {
                    	
                        System.gc();
                    }
                }

                try {
                	
                    StringWriter writer = new StringWriter();
                    
                    writer.setContent("completed!");
                    
                    writer.overwrite(true);
                    
                    writer.set(destRList + "\\" + config + ".txt");
                    
                    writer.write();
                    
                } catch (Exception e) {
                
                	
                }
            }
        }
    }
    
    }

}