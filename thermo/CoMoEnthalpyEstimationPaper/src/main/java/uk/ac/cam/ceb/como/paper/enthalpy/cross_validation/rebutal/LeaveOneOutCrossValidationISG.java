/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
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
 */

public class LeaveOneOutCrossValidationISG {
	
    public static void main(String[] args) throws Exception {

    	double maxErr = 20;
    	
    	Map<Reaction, Double> validReaction = new HashMap<Reaction, Double>();
    	
    	Map<Reaction, Double> invalidReaction = new HashMap<Reaction, Double>();
    	
        Map<String, Integer[]> mapElPairing = new HashMap<>();

//      String srcCompoundsRef = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\esc\\";
//      String srcRefPool = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\calc-enthalpy_scaled_kJperMol.csv";
//      String destRList = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\reactions\\x-validation\\hco_isg\\";

        String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\esc\\g09\\";
        String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\ref-enthalpy_scaled_kJperMol.csv";
        String destRList = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\hco_isg\\";

        
//        String srcCompoundsRef = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\g09\\";
//        String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";
//        String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ti_isg\\";

        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        Collection<Species> ref = refParser.getRefSpecies();

//        for(Species r:ref) {
//        	
//        System.out.println("Ref species name: " + r.getRef() + " Ref species enthalpy: " + r.getHf());
//        	
//        }
        
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
        
        int[] ctrRes = new int[]{25}; // 1, 5, 15, 25 //25,50 // 1,2,3,4,5,6,7,8,9,10
        
        int[] ctrRadicals = new int[]{0,100}; // 0, 1, 2, 3, 4, 5

        for (int z = 0; z < ctrRadicals.length; z++) {
        	
            int maxRadical = ctrRadicals[z];
            
            int timeout = 1500;
            
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
                            
                            	System.out.println("s. name: " + s.getRef()+ "  s. Hf: " + s.getHf());
                            	
                            	try {
                            		
                                    ReactionList rList = new ReactionList();
                                    
                                    for (ReactionList l : results.get(s)) {
                                    
                                        rList.addAll(l);
                                    }
                                    
                                    completeRList.addAll(rList);
                                    
                                    ReactionSelector selector = new MedianReactionSelector();
                                    
                                    Reaction r = selector.select(rList).get(0);
                                    
                                    System.out.println( "Median Reaction: " + r.toString() + " species Hf: " + r.getSpecies().getHf() + " : r.calculateHf(): " + r.calculateHf());
                                    
                                    /**
                                     * 
                                     * @nk510
                                     * Absolute difference between enthalpy of formation for target species and estimated enthalpy of formation for target species.
                                     *  
                                     */
                                    
                                    double  d = Math.abs(r.getSpecies().getHf() - r.calculateHf());
                                    
                                    if(d>maxErr) {
                                    	
                                    	invalidReaction.put(r, d);
                                    	
                                    }else {
                                    	
                                    	validReaction.put(r, d);
                                    
                                    }
                                    
                                    s.setHf(r.calculateHf());
                                    
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

//                              System.out.println("Median_reaction: " + completeRList.get(0).toString()+ " Hf: " + completeRList.get(0).getSpecies().getHf());
                                
                                for(int ri=0; ri<completeRList.size();ri++) {
                                
                                System.out.println("Reaction("+ri+")_" + completeRList.get(ri).toString() + " Calculated Hf for reaction("+ri+")_ " + completeRList.get(ri).calculateHf() + " Species (median) enthalpy: " + completeRList.get(ri).getSpecies().getHf());
                                
                                }
                            }

                            if(!ttipSpecies.isEmpty()){
                            	
                            	for(Species sp: ttipSpecies) {
                                    
                                	/**
                                	 * 
                                	 * Median enthalpy and species name.
                                	 * 
                                	 */
                                	
                                System.out.println("[Species name: " + sp.getRef() + "  Species enthalpy (median?): " + sp.getHf() + " ]" );
                                
                                
                                
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
        
        System.out.println();
        
        System.out.println("Valid reactions: ");
        
        BufferedWriter validReactionFile = new BufferedWriter(new FileWriter(destRList + "\\" + "valid_reactions" + ".txt", true)); 
        
        for(Map.Entry<Reaction, Double> v: validReaction.entrySet()) {
        	
        	System.out.println("Reaction: " + v.getKey().toString() + " Median enthalpy: " + v.getKey().calculateHf() + " ( " + v.getValue()+ " )");
        	
        	validReactionFile.write("Reaction: " + v.getKey().toString() + " Median enthalpy: " + v.getKey().calculateHf() + " ( " + v.getValue()+ " )");
        	
        	validReactionFile.write("\n");
        	
        }
        
        validReactionFile.close();
        
        BufferedWriter invalidReactionFile = new BufferedWriter(new FileWriter(destRList + "\\" + "invalid_reactions" + ".txt", true)); 
        
        System.out.println();
        
        System.out.println("Invalid reactions: ");
        
        for(Map.Entry<Reaction, Double> inv: invalidReaction.entrySet()) {
        	
        System.out.println("Reaction: " + inv.getKey().toString() + " Median enthalpy: " + inv.getKey().calculateHf() + " ( " + inv.getValue()+ " )");
        
    	invalidReactionFile.write("Reaction: " + inv.getKey().toString() + " Median enthalpy: " + inv.getKey().calculateHf() + " ( " + inv.getValue()+ " )");
    	
    	invalidReactionFile.write("\n");
    	
        }
        
        invalidReactionFile.close();
        
    }
}