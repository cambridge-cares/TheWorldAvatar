package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Bond;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSolver;
import uk.ac.cam.ceb.como.paper.enthalpy.io.LoadSpecies;
import uk.ac.cam.ceb.como.paper.enthalpy.reduction.list_calculator.ErrorBarCalculation;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.FolderUtils;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;
import uk.ac.cam.ceb.paper.sort.Sort;

/**
*  
* @author nk510 ( caresssd@hermes.cam.ac.uk )
* @author am2145( am2145@cam.ac.uk )
* 
* - This code does pre- processing step in cross validation for selected reference set of species.
* 
* - In this code inputs are:
*    - A set of species (Gaussian files) given as reference list.
*    - A list of target species  for which enthalpies are given. This list is given as csv file. Comments: List of target and reference species should be the same.
*    - Maximum error that is compared with error for each generated reaction. If error  for each reaction is smaller that maximum error then that reaction is valid. Otherwise, that reaction is invalid. 
*    - Number of runs is set to one, and number of allowed reactions that will be generated.
*    - ISG reaction type
*    
*   Output is:
* - Calculates EBRs for reference set of species (srcRefPool) by using Gaussian files of that set of species.
* - Calculates difference  (error) between estimated enthalpy of formation for each species in each generated reaction and reference enthalpy of selected species. Reference enthalpy is given in csv file.
* - Calculates error bar for each species by using errors for each generated reaction. Error is difference between reference enthalpy of formation and calculated enthalpy of formation for selected species.
* - Generates an initial list of valid reactions.
* - Generates an initial list of invalid reactions.
* - Generates an initial list of valid species
* - Generates an initial list of invalid species.
* 
*/

public class DataPreProcessing {	
	
    /**
     * @author nk510 ( caresssd@hermes.cam.ac.uk )
     * @param timeout Time limited
     * @param maxErr Maximum error that is compared with the error for each generated reaction. 
     * @param destRList The destination folder where information about reactions, enthalpies, valid species, invalid species, valid reactions, invalid reaction are saved.
     * @param ctrRadicals The number of raducals. 
     * @param ctrRuns The number of runs.
     * @param ctrRes The number of reaction that will be generated.
     * @param refSpecies The species
     * @param spinMultiplicity The spin multiplicity for each species.
     * @param solver The LP solver
     * @param validSpecies The set of valid species that is generated in pre-processing step.
     * @param invalidSpecies The set of invalid species  that is generated in pre-processing step.
     * @param validReaction  The set of valid reactions that is generated in pre-processing step.
     * @param invalidReaction The set of invalid reactions that is generated in pre-processing step.
     * 
     * @throws Exception the exception.
     * 
     */
	
    public void getPreProcessingCorssValidation(ReactionType reactionType, int timeout, int maxErr, String destRList, int[] ctrRadicals, int[] ctrRuns,  int[] ctrRes, List<Species> refSpecies,  Map<Species, Integer> spinMultiplicity, LPSolver solver, LinkedHashSet<Species> validSpecies,  LinkedHashSet<Species> invalidSpecies, Map<Reaction, Double> validReaction, Map<Reaction, Double> invalidReaction,  BufferedWriter printedResultsFile) throws Exception {
    	
    for (int z = 0; z < ctrRadicals.length; z++) {
        	
    int maxRadical = ctrRadicals[z];
            
//          timeout = 1500; // remove this when call this method            
//          Stopping run the code condition
            
    int stop = 1;
            
    for (int i = 0; i < ctrRuns.length; i++) {
        
    for (int k = 0; k < ctrRes.length; k++) {
            
        		String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                
        		System.out.println("Process configuration " + config);
                
        		printedResultsFile.write("Process configuration " + config);
        		
        		printedResultsFile.write("\n");
        		
                	  
                	  /**
                	   * @author nk510 ( caresssd@hermes.cam.ac.uk )
                 	   * Folder path settings on HPC machine
                	   */

          if(new File(destRList + "/data-pre-processing" + "/" + config + ".txt").exists()) {

      	  /**
      	   * @author nk510 ( caresssd@hermes.cam.ac.uk )
       	   * Folder path settings on PC machine
      	   */
    		
//if (new File(destRList + "\\"+"data-pre-processing" + "\\" + config + ".txt").exists()) {

	 
          	  /**
          	   * @author nk510 ( caresssd@hermes.cam.ac.uk )
           	   * Folder path settings on PC machine
          	   */
          	
//System.out.println("Skipping " + destRList+"\\"  + "data-pre-processing" + "\\" + config);
             /**
               * @author nk510 ( caresssd@hermes.cam.ac.uk )
            	 * Folder path settings on HPC machine
            	 */  
  		System.out.println("Skipping " + destRList  + "/" +"data-pre-processing" + "/" + config);
  		
  		printedResultsFile.write("Skipping " + destRList  + "/" +"data-pre-processing" + "/" + config);
        
//  	printedResultsFile.write("Skipping " + destRList+"\\"  + "\\" +"data-pre-processing" + "\\" + config);
  		
          
  		printedResultsFile.write("\n");
  		
              continue;
          }
                
        		  /**
        		   * @author nk510 ( caresssd@hermes.cam.ac.uk )
        		   * Commented line of code below is used in original code.
        		   *  
        		   */
//              Collections.shuffle(refSpecies);  
                
                int ctr = 1;
                
                System.out.println("refSpecies.isEmpty()(getPreProcessingCorssValidation method i class DataPreProcessing) : " + refSpecies.isEmpty());
                
                printedResultsFile.write("refSpecies.isEmpty()(getPreProcessingCorssValidation method i class DataPreProcessing) : " + refSpecies.isEmpty());
                
                printedResultsFile.write("\n");
                
                for(Species target : refSpecies) {
                	
                	/**
                	 * @author nk510 ( caresssd@hermes.cam.ac.uk )
                	 * Added new HashMap<>() -> new HashMap<Species, Collection<ReactionList>>()
                	 */
                    Map<Species, Collection<ReactionList>> results = new HashMap<Species, Collection<ReactionList>>();
                    
                    System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + refSpecies.size() + ")");
                        
                    printedResultsFile.write("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + refSpecies.size() + ")");
                    
                    printedResultsFile.write("\n");
                    
                    ctr++;
                    
                    /**
                     * @author nk510 ( caresssd@hermes.cam.ac.uk )
                     * Folder path settings on PC machine
                     * 
                     */
                  
//                      if (new File(destRList+"\\"  + "data-pre-processing" + "\\"+ target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
                        /**
                         * @author nk510 ( caresssd@hermes.cam.ac.uk )
                      	 * Folder path settings on HPC machine
                      	 * 
                      	 */
                      if (new File(destRList  +"/"+ "data-pre-processing" + "/"+ target.getRef() + "/" + config + "_reaction-list.rct").exists()) {
                          
                          continue;
                          
                          }
                  /**
                   * 
                   * @author nk510 ( caresssd@hermes.cam.ac.uk )
                   * Added: new ArrayList<>() -> new ArrayList<Species>();
                   * 
                   */
                    List<Species> refPool = new ArrayList<Species>();
                    
//                  System.out.println("RefPool before adding refSpecies:");
                    for(Species refP: refPool) {
                    	
//                    	System.out.println(refP.getRef() + " " + refP.getHf() + " " + refP.getAtomMultiset() + " " + refP.getAtomMap() + " " + refP.getBondMap());
                    	
//                    	System.out.println("Atom multiset in refPool");
                    	for(Element refpms : refP.getAtomMultiset()){
                    		
//                    		System.out.println("name: " + refpms.getName() + " atom number: " + refpms.getAtomicNumber()+ " group: " + refpms.getGroup() + " mass number: " + refpms.getMassNumber());
                    	}
                    	
//                    	System.out.println("Atom map in refPool:");
                    	for(Map.Entry<String, String> atomM : refP.getAtomMap().entrySet()){
                    		
//                    		System.out.println(atomM.getKey() + " " + atomM.getValue());
           
                    	}
                    	
//                    	System.out.println("Bond map in refPool:");
                    	for(Bond bondM : refP.getBondMap()){
                    		
//                    	System.out.println("bond type value: " + bondM.getBondType().getValue() + " atom A: " + bondM.getRefAtomA().toString() + " atom B: " + bondM.getRefAtomB().toString());
                    		
                    	}
                    }
                    
                    refPool.addAll(refSpecies);
                    
//                      System.out.println("RefPool after adding refSpecies:");
//                      System.out.println("refPool.isEmpty() (getPreProcessingCorssValidation method i class DataPreProcessing) : " +refPool.isEmpty());

                        for(Species refP: refPool) {
                    	
//                    	System.out.println(refP.getRef() + " " + refP.getHf() + " " + refP.getAtomMultiset() + " " + refP.getAtomMap() + " " + refP.getBondMap());
                    	
//                    	System.out.println("Atom multiset in refPool");
                    	for(Element refpms : refP.getAtomMultiset()){
                    		
//                    	System.out.println("name: " + refpms.getName() + " atom number: " + refpms.getAtomicNumber()+ " group: " + refpms.getGroup() + " mass number: " + refpms.getMassNumber());
                    		
                    	}
                    	
//                    	System.out.println("Atom map in refPool:");
                    	for(Map.Entry<String, String> atomM : refP.getAtomMap().entrySet()){
                    		
//                    	System.out.println(atomM.getKey() + " " + atomM.getValue());
           
                    	}
                    	
//                    	System.out.println("Bond map in refPool:");
                    	for(Bond bondM : refP.getBondMap()){
                    	
//                    	System.out.println("bond type value: " + bondM.getBondType().getValue() + " atom A: " + bondM.getRefAtomA().toString() + " atom B: " + bondM.getRefAtomB().toString());

                    	}
                        
                        }
                    
                        refPool.remove(target);
                    
//                      System.out.println("RefPool after removing target species:");
                        for(Species refP: refPool) {
                    	
//                    	System.out.println(refP.getRef() + " " + refP.getHf() + " " + refP.getAtomMultiset() + " " + refP.getAtomMap() + " " + refP.getBondMap());
//                    	System.out.println("Atom multiset in refPool");
                    	for(Element refpms : refP.getAtomMultiset()){
                    		
//                    	System.out.println("name: " + refpms.getName() + " atom number: " + refpms.getAtomicNumber()+ " group: " + refpms.getGroup() + " mass number: " + refpms.getMassNumber());
                    	
                    	}
                    	
//                    	System.out.println("Atom map in refPool:");
                    	for(Map.Entry<String, String> atomM : refP.getAtomMap().entrySet()){
                    		
//                    	System.out.println(atomM.getKey() + " " + atomM.getValue());
           
                    	}
                    	
//                    	System.out.println("Bond map in refPool:");
                    	for(Bond bondM : refP.getBondMap()){
                    		
//                    	System.out.println("bond type value: " + bondM.getBondType().getValue() + " atom A: " + bondM.getRefAtomA().toString() + " atom B: " + bondM.getRefAtomB().toString());

                    	}
                    }
                    
                    // filter for radicals
                    for (Species sSpin : spinMultiplicity.keySet()) {
                    	
                        try {
                        	
                            if (spinMultiplicity.get(sSpin) != null && spinMultiplicity.get(sSpin) - 1 > maxRadical) {
                            	
                                refPool.remove(sSpin);
                            
                            }
                            
                        } catch (NullPointerException ex) {
                        	
                        ex.printStackTrace();	
                        }
                    }
                    
//                  System.out.println("refPool after removing species from spinMultiplicity:");

                    for(Species refP: refPool) {
                    	
//                    	System.out.println(refP.getRef() + " " + refP.getHf() + " " + refP.getAtomMultiset() + " " + refP.getAtomMap() + " " + refP.getBondMap());
                    	
//                    	System.out.println("Atom multiset in refPool");
                    	for(Element refpms : refP.getAtomMultiset()){
                    	
//                    	System.out.println("name: " + refpms.getName() + " atom number: " + refpms.getAtomicNumber()+ " group: " + refpms.getGroup() + " mass number: " + refpms.getMassNumber());
                    	
                    	}
                    	
//                    	System.out.println("Atom map in refPool:");
                    	for(Map.Entry<String, String> atomM : refP.getAtomMap().entrySet()){
                    		
//                    	System.out.println(atomM.getKey() + " " + atomM.getValue());
           
                    	}
                    	
//                    	System.out.println("Bond map in refPool:");
                    	for(Bond bondM : refP.getBondMap()){
                    		
//                    	System.out.println("bond type value: " + bondM.getBondType().getValue() + " atom A: " + bondM.getRefAtomA().toString() + " atom B: " + bondM.getRefAtomB().toString());
                    	
                    	}
                    }

                    /**
                     * 
                     * @author nk510 ( caresssd@hermes.cam.ac.uk )
                     * Commented line of code below 1is used in original code
                     * 
                     */
                    
//                  Collections.shuffle(refPool);
                    
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    
//                  PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new HHDReactionType()));
                    
                    PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, reactionType));
                    
                    poolModCalc.setMaximumSearchDepth(50); //50
                    
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
                            
                            printedResultsFile.write("Terminated!");
                            
                            printedResultsFile.write("\n");
                            
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
                        	
                        	printedResultsFile.write("Ref species name: " + s.getRef()+ "  ref species Hf: " + s.getHf());
                        	
                        	printedResultsFile.write("\n");
                        	
                        	try {
                        		
                                ReactionList rList = new ReactionList();
                                
                                for (ReactionList l : results.get(s)) {
                                	
                                rList.addAll(l);
                                
                                }
                                
                              completeRList.addAll(rList);
                              
                              ReactionSelector selector = new MedianReactionSelector();
                              
                              List<Reaction> reactionList = selector.select(rList);
                              
                              System.out.println("reactionList.size(): " + reactionList.size());
                              
                              printedResultsFile.write("reactionList.size(): " + reactionList.size());
                              
                              printedResultsFile.write("\n");
                              
                              for(Reaction r : reactionList) {
                            	  
                            	  System.out.println("Species name for reaction list : " + r.getSpecies().getRef());
                            	  
                                  printedResultsFile.write("Species name for reaction list : " + r.getSpecies().getRef());
                                  
                                  printedResultsFile.write("\n");
                                  
                              }
                              
                              ttipSpecies.add(s);
                                
                            } catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
                            
                            	System.out.println("No data were calculated for " + s.getRef());
                            	
                                printedResultsFile.write("No data were calculated for " + s.getRef());
                                
                                printedResultsFile.write("\n");
                                
                            }
                        }
                        
                        /**
                         * 
                         * @author nk510 ( caresssd@hermes.cam.ac.uk )
                         * PC machine settings.
                         * 
                         */
                        
//                        	 if(!new File(destRList+"\\"  + "data-pre-processing" + "\\"+ target.getRef() + "\\").exists()) {
                                 /**
                                  * 
                                  * @author nk510 ( caresssd@hermes.cam.ac.uk )
                             	 * HPC settings
                             	 * 
                             	 */
                              if(!new File(destRList  + "/"+"data-pre-processing" + "/"+ target.getRef() + "/").exists()) {
                                 	
                                 	/**
                                 	 * 
                                 	 * @author nk510 ( caresssd@hermes.cam.ac.uk )
                                 	 * PC machine settings.
                                 	 * 
                                 	 */
//                            new File(destRList+"\\"  + "data-pre-processing" + "\\"+ target.getRef() + "\\").mkdirs();
                                 /**
                                  * 
                                  * @author nk510 ( caresssd@hermes.cam.ac.uk )
                             	 * HPC settings
                             	 * 
                             	 */
                               new File(destRList  + "/" + "data-pre-processing" + "/"+ target.getRef() + "/").mkdirs();
                                 
                                 }
                     /**
                      * 
                      * @author nk510 ( caresssd@hermes.cam.ac.uk )
                      * PC machine settings.
                      * 
                      */
                        ReactionListWriter rListWriter;
                            /**
                             * 
                             * @author nk510 ( caresssd@hermes.cam.ac.uk )
                          	 * HPC settings
                          	 * 
                          	 */
                             rListWriter = new ReactionListWriter(new File(destRList  + "/" + "data-pre-processing" + "/"+ target.getRef() + "/" + config + "_reaction-list.rct"));
                        /**
                         * 
                         * @author nk510 (caresssd@hermes.cam.ac.uk)
                         * Added ctr in species pool median name.
                         * 
                         */
                    
                     
                    
                    /**
                     * 
                     * @author nk510 ( caresssd@hermes.cam.ac.uk )
                  	 * HPC settings.
                  	 * 
                  	 */
                  SpeciesPoolWriter spWriter;
               	  spWriter = new SpeciesPoolWriter(new File(destRList + "/" +"data-pre-processing" + "/"+ target.getRef() + "/" + config + "_species-pool_median_"+ctr+".csv"));
                  BufferedWriter printedJsonFileMedianEnthalpySpeciesPreProcessingAnalysis;
               	  printedJsonFileMedianEnthalpySpeciesPreProcessingAnalysis = new BufferedWriter(new FileWriter(destRList+ "/" +"data-pre-processing" + "/"+ target.getRef() + "/" + config + "_species-pool_median_"+ctr+".json", true));	   
                  JSONArray medianEnthalpyReactionJsonList = new JSONArray();
                  
                  if (!completeRList.isEmpty()) {
                        	
                            System.out.println("Writting complete reaction list...");
                            
                            printedResultsFile.write("Writting complete reaction list...");
                            
                            printedResultsFile.write("\n");
                            
                            rListWriter.set(completeRList);
                            
                            rListWriter.overwrite(true);
                            
                            rListWriter.write();
                            
                            for(int ri=0; ri<completeRList.size();ri++) {
                             
                            	/**
                            	 * 
                            	 * @author nk510 (caresssd@hermes.cam.ac.uk)
                            	 * Calculates error that is difference between calculated enthalpy of formation (Hf) for currently analyzed species and species reference enthalpy.
                            	 * 
                            	 * 
                            	 */
                            	
                            double error = Math.abs(completeRList.get(ri).getSpecies().getHf()-completeRList.get(ri).calculateHf());
                            
                            /**
                             * 
                             * @author nk510 (caresssd@hermes.cam.ac.uk)
                             * Commented line below: double variable 'calculatedEnthalpy' does not save correctly calculated enthalpy of formation (Hf). It has error in decimals.
                             *   
                             */
//                          double calculatedEnthalpy = completeRList.get(ri).calculateHf();
                            
                        	System.out.println(" Reaction("+ri+"): " + completeRList.get(ri).toString() + " Species (ref) enthalpy: " + completeRList.get(ri).getSpecies().getHf() + " Calculated Hf for reaction("+ri+"): " + completeRList.get(ri).calculateHf() );
                            
                        	printedResultsFile.write(" Reaction("+ri+"): " + completeRList.get(ri).toString() + " Species (ref) enthalpy: " + completeRList.get(ri).getSpecies().getHf() + " Calculated Hf for reaction("+ri+"): " + completeRList.get(ri).calculateHf());
                        	
                        	printedResultsFile.write("\n");
                        	
                        	/**
                        	 * 
                        	 * @author nk510 (caresssd@hermes.cam.ac.uk)
                             * Species is added into set of valid species and reaction is added into valid set of reactions if the error is lower than maximum error for currently analyzed species. Otherwise the species is added into the set of invalid species and the reaction is added into invalid set of reactions.
                             * 
                             */
                        	
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
                            
                            stop++;
                            
                            /**
                             * 
                             * @author NK510 (caresssd@hermes.cam.ac.uk)
                             * Selects median reaction and median enthalpy in preprocessing step.
                             * 
                             */
                            ReactionSelector medianSelector = new MedianReactionSelector();
                            
                            Reaction medianReaction = medianSelector.select(completeRList).get(0);
                            
                            System.out.println();
                            
                            System.out.println("Median Reaction: " + medianReaction.toString() + " medianReaction species Hf: " + medianReaction.getSpecies().getHf() + " : medianReaction.calculateHf(): " + medianReaction.calculateHf());
                            
                            printedResultsFile.write("Median Reaction: " + medianReaction.toString() + " medianReaction species Hf: " + medianReaction.getSpecies().getHf() + " : medianReaction.calculateHf(): " + medianReaction.calculateHf());
                            
                            
                            JSONObject jsonEnthalpySpecies = new JSONObject();
                     		
                     	    JSONObject jsonAllEnthalpySpecies = new JSONObject();	    
                     	    
                     	    /**
                     	     * 
                     	     * @author NK510 (caresssd@hermes.cam.ac.uk)
                     		 * Store information about median enthalpy for species in JSON file.
                     		 * 
                     		 */
                     	    
                     	   jsonEnthalpySpecies.put("speciesName",medianReaction.getSpecies().getRef().toString());
                     		
                       	   jsonEnthalpySpecies.put("medianEnthalpy",medianReaction.calculateHf());
                     		
                     	   jsonAllEnthalpySpecies.put("species", jsonEnthalpySpecies);
                     		
                     	  medianEnthalpyReactionJsonList.add(jsonAllEnthalpySpecies);
                     	   
                            
                        }
                  
                  printedJsonFileMedianEnthalpySpeciesPreProcessingAnalysis.write(medianEnthalpyReactionJsonList.toJSONString());
                  printedJsonFileMedianEnthalpySpeciesPreProcessingAnalysis.close();
                  
                  
                        if(!ttipSpecies.isEmpty()){
                        	
                        for(Species sp: ttipSpecies) {
                        		
                        		/**
                        		 * 
                            	 * @author nk510 (caresssd@hermes.cam.ac.uk)
                            	 * Species reference enthalpy and species name.
                            	 * 
                            	 */
                            	
                        System.out.println("[ Species name: " + sp.getRef() + "  Species (reference) enthalpy: " + sp.getHf() + " ]" );
                            
                        printedResultsFile.write("[ Species name: " + sp.getRef() + "  Species (reference) enthalpy: " + sp.getHf() + " ]" );
                        
                        printedResultsFile.write("\n");
                        
                        }
                        	
                        System.out.println("Writting species list...");
                        
                        printedResultsFile.write("Writting species list...");
                        
                        printedResultsFile.write("\n");
                            
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
                
             
                    
                /**
                 * @author nk510 ( caresssd@hermes.cam.ac.uk )
              	 * HPC settings
              	 * 
              	 */
            writer.set(destRList  + "/"+ "data-pre-processing" + "/"+ config + ".txt");

                 writer.write();
                    
                } catch (Exception e) {
                
                	
                }
            }
         }
      }    
    }
 
}