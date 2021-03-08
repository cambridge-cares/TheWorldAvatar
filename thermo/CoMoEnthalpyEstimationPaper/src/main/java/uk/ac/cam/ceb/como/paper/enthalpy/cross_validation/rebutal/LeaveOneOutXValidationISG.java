/*
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */
package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.rebutal;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
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

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
 *
 * @author pb556
 * 
 * @author nk510 ( caresssd@hermes.cam.ac.uk )
 * @author am2145( am2145@cam.ac.uk )
 * 
 * - This code does pre- processing step in cross validation for selected Ti-based species.
 * - In this code inputs are: 
 *    - A set of Ti-based species (Gaussian files) given as reference list. 
 *    - A list of 25 target species  for which enthalpies are given. This list is given as csv file. Comments: List of target and reference species should be the same.
 *    - Maximum error that is compared with error for each generated reaction. If error  for each reaction is smaller that maximum error then that reaction is valid. Otherwise, that reaction is invalid. 
 *    - Number of runs is set to one, and number of allowed reactions is set to 5.
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

public class LeaveOneOutXValidationISG {
	
    public static void main(String[] args) throws Exception {

    	double maxErr = 20; //20 was in original code
    	
    	Set<Species> validSpecies = new HashSet<Species>();
    	
    	Set<Species> invalidSpecies = new HashSet<Species>();
    	
    	Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();
    	
    	Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();
    	
        Map<String, Integer[]> mapElPairing = new HashMap<>();
        
//      String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\";
//      String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv"; //ref_scaled_kJperMols_v8-5-6c.csv";
//      String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\";
        
        String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\esc\\g09\\";
        String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\ref-enthalpy_scaled_kJperMol.csv";
        String destRList = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\hco_hd\\";
        
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        Collection<Species> ref = refParser.getRefSpecies();
        
        Collection<Species> invalids = new HashSet<>();
        
        Map<Species, Integer> spinMultiplicity = new HashMap<>();
        
        int ctr = 1;
        
        for (Species s : ref) {
            
        System.out.println("REF: Processing " + ctr + " / " + ref.size());
        
        ctr++;
        
        File f = new File(srcCompoundsRef + s.getRef().replace(".g09", "") + ".g09");
            
        if (f.exists()) {
                
            	System.out.print(f.getName() + ": ");
                
                Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                
                if (e != null) {
                    
                	spinMultiplicity.put(s, e[1] + 1);
                
                    mapElPairing.put(s.getRef(), e);
                    
                } else {
                    
                	System.out.println("REF: e- pairing could not be determined for " + s.getRef());
                    
                    invalids.add(s);
                }
                
            } else {
            	
                System.out.println("REF: No file found for " + s.getRef());
                
                invalids.add(s);
            }
        }
        
        ref.removeAll(invalids);
        
        List<Species> refSpecies = new ArrayList<>(ref);

        SolverHelper.add(mapElPairing);

        LPSolver solver = new TerminalGLPKSolver(15000, false, true);
        
        solver.setDirectory(new File("D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\"));

        int[] ctrRuns = new int[]{1};
        
        int[] ctrRes = new int[]{1}; //5 was in original code // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10
        
        int[] ctrRadicals = new int[]{0}; //100 was in original code // 0, 1, 2, 3, 4, 5

        for (int z = 0; z < ctrRadicals.length; z++) {
        	
            int maxRadical = ctrRadicals[z];
            
            int timeout = 1500; //1500 was in original code
            
            for (int i = 0; i < ctrRuns.length; i++) {
            
            	for (int k = 0; k < ctrRes.length; k++) {
                
            		String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    
            		System.out.println("Process configuration " + config);
                    
                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                    	
                        System.out.println("Skipping " + destRList + "\\" + config);
                        
                        continue;
                    }
                    
                    Collections.shuffle(refSpecies);
                    
                    ctr = 1;
                    
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
                        
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new HDReactionType()));
                        
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
                                  

//                                Reaction r = selector.select(rList).get(0);
//                                    
//                                System.out.println("Median Reaction: " + r.toString() + " species Hf: " + r.getSpecies().getHf() + " : r.calculateHf(): " + r.calculateHf());
//                                    
//                                    /**
//                                     * 
//                                     * @nk510
//                                     * Absolute difference between enthalpy of formation for target species and estimated enthalpy of formation for target species.
//                                     * 
//                                     */
//                                    
//                                double  d = Math.abs(r.getSpecies().getHf() - r.calculateHf());
//                                    
//                                if(d>maxErr) {
//                                    
//                                invalidReaction.put(r, d);
//                                    	
//                                }else {
//                                    	
//                                validReaction.put(r, d);
//                                    
//                                }
//                                    
//                                  s.setHf(r.calculateHf());                                    
//                                
//                                  System.out.println("Reaction("+cri+"):_ " + rList.get(cri).toString() + "  Reaction enthalpy from rList: " + rList.get(cri).calculateHf() + "  Median reaction enthalpy from r after calling selector : " + r.calculateHf());
                                    
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
        }//for (int z = 0; z < ctrRadicals.length; z++)
        
        System.out.println();
        
        System.out.println("Valid reactions: ");
        
        BufferedWriter validReactionFile = new BufferedWriter(new FileWriter(destRList + "\\" + "valid_reactions" + ".txt", true));
        
        for(Map.Entry<Reaction, Double> v: validReaction.entrySet()) {
        	
        	System.out.println("Reaction: " + v.getKey().toString() + " Calc enthalpy: " + v.getKey().calculateHf() + " Ref enthalpy: " + v.getKey().getSpecies().getHf() + " (error: " + v.getValue()+ " )");
        	
        	validReactionFile.write("Reaction: " + v.getKey().toString() + " Calc enthalpy: " + v.getKey().calculateHf() + " Ref enthalpy: " + v.getKey().getSpecies().getHf() + " (error: " + v.getValue()+ " )");
        	
        	validReactionFile.write("\n");
        	
        }
        
        validReactionFile.close();
        
        BufferedWriter invalidReactionFile = new BufferedWriter(new FileWriter(destRList + "\\" + "invalid_reactions" + ".txt", true)); 
        
        System.out.println();
        
        System.out.println("Invalid reactions: ");
        
        for(Map.Entry<Reaction, Double> inv: invalidReaction.entrySet()) {
        	
        System.out.println("Reaction: " + inv.getKey().toString() + " Calc enthalpy: " + inv.getKey().calculateHf()  + " Ref enthalpy: " + inv.getKey().getSpecies().getHf() + " (error: " + inv.getValue()+ " )");
        
    	invalidReactionFile.write("Reaction: " + inv.getKey().toString() + " Calc enthalpy: " + inv.getKey().calculateHf()  + " Ref enthalpy: " + inv.getKey().getSpecies().getHf() + " (error: " + inv.getValue()+ " )");
    	
    	invalidReactionFile.write("\n");
    	
        }
        
        invalidReactionFile.close();
       
        System.out.println();
       
        System.out.println("Valid Species names: ");
       
        for(Species vs: validSpecies) {
    	   
        System.out.println(vs.getRef());
       
       }
       
       System.out.println();
       
       System.out.println("Invalid Species names: ");
       
       for(Species invs: invalidSpecies) {
          
       if(!validSpecies.contains(invs)) {
       
       System.out.println(invs.getRef());
       
       }
       }
       
       /**
        * 
        * Calculation error bar for each invalid species.
        * 
        */
     Map<Species,Double> speciesErrorBarMap = new HashMap<Species,Double>();
       
//     System.out.println("- - -  - -  - - - - - - - - - -  - - - - -  - - - - - -");       
//       for(Species invspecies: invalidSpecies) {
//    	   
//    	   if(!validSpecies.contains(invspecies)) {
//    		   
//        	   double errorSum = 0.0;
//        	   
//        	   int errorCount = 0;
//        	   
//        	   double errorBar = 0.0;    		   
//     
//    	   for(Map.Entry<Reaction, Double> invr: invalidReaction.entrySet()) {
//    		   
//    		   if(invr.getKey().getSpecies().getRef().toString().equalsIgnoreCase(invspecies.getRef())){
//    			   
//    		    errorSum = errorSum + invr.getValue();
//    			   
//    			errorCount++;
//
//    		   }
//    	   }
//    	   
//    	   errorBar =errorSum/errorCount;
//    	   
//    	   System.out.println("Species name: " + invspecies.getRef() + " , Error bar is: " + errorBar);
//    	   
//    	   speciesErrorBarMap.put(invspecies, errorBar);
//    	   
//    	   }
//    	   
//       }  

       /**
        * 
        * Calculation error bar for each species in invalid set of reactions.
        * 
        */
     Set<Species> uniqueinvSetOfSpecies  = new HashSet<Species>();
     
     for(Map.Entry<Reaction, Double> invspm: invalidReaction.entrySet()) {
    	 
     if(!uniqueinvSetOfSpecies.contains(invspm.getKey().getSpecies())) {
    	
    	uniqueinvSetOfSpecies.add(invspm.getKey().getSpecies());
    
     }
     
     }
    
     System.out.println("- - - -  - - - Unique species names from invalid reactions:  - - - - - -  - - - - - - ");
     
     for(Species s : uniqueinvSetOfSpecies) {
    	 
    	 System.out.println(s.getRef());    
     }
     
     System.out.println("- - - -  - - -  Species names from invalid reactions and their error bars: - - - - - -  - - - - - - ");
     
     for(Species usp: uniqueinvSetOfSpecies ) {

         double errorSum = 0.0;
         int errorCount=0;
    	 double errorBar = 0.0;
    	 
    	 for(Map.Entry<Reaction, Double> m: invalidReaction.entrySet()) {
    		 
    		 if(m.getKey().getSpecies().getRef().equals(usp.getRef())) {
    			 
    			 errorSum = errorSum + m.getValue();
    			 
    			 errorCount++;
    		 }    		 
    	 }
    	 
     errorBar=errorSum/errorCount;
     
     System.out.println("Species name: " + usp.getRef() + " , error bar: "  + errorBar );
    	 
     speciesErrorBarMap.put(usp, errorBar);
     
     }
     
     Map<Species,Double> invalidSpeciesErrorBarMap = new HashMap<Species,Double>();
     
     for(Map.Entry<Species,Double> speciesMap : speciesErrorBarMap.entrySet()) {
    	 
    	 if(invalidSpecies.contains(speciesMap.getKey()) && (!validSpecies.contains(speciesMap.getKey()))) {
    	 
    	 invalidSpeciesErrorBarMap.put(speciesMap.getKey(), speciesMap.getValue());
    	 
          }
     }
     
     System.out.println();
     
     System.out.println("-----------------Species from invalid (rejected) list with error bar: ");
     
     for(Map.Entry<Species, Double> invmap: invalidSpeciesErrorBarMap.entrySet()) {
    	 
    	 System.out.println(invmap.getKey().getRef()+ " " + invmap.getValue()) ;
     }
     
     System.out.println();
     
     System.out.println("- - - -  - - -  - - - - - -  - - - - - -");
     
     double maxErrorBar = Collections.max(invalidSpeciesErrorBarMap.values());
     
     for(Map.Entry<Species,Double> invspeciesMap : invalidSpeciesErrorBarMap.entrySet()) {
    
    	 if(invspeciesMap.getValue()==maxErrorBar) {
    		 
   		 System.out.println("Species name : " + invspeciesMap.getKey().getRef() + " error bar from map : " +  invspeciesMap.getValue() + " max error bar: " + maxErrorBar);
    		 
    	 break;
    	 }
    }
    
    
    
    
    }
}