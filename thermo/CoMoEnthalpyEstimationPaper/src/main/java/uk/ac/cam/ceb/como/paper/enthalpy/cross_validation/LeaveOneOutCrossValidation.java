/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

import com.cmclinnovations.data.collections.ObjectPool;
import java.io.File;
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

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

 /**
 *
 * @author pb556
 */

public class LeaveOneOutCrossValidation {

    public static void main(String[] args) throws Exception {

    	//Java code from previous version that is commented
    	
        /* String srcRefPool = "C:\\Users\\pb556\\workspace\\methodology\\results\\thermal_scaled_kJperMol.csv";
        String destRList = "C:\\Users\\pb556\\workspace\\methodology\\results\\leave-one-out-x-validation\\hco\\";
        */
       
    	/**
    	 * @author nk510
    	 */
    	
    	/*Testing main method by using recovered file:  thermal_scaled_kJperMol.csv (date stamp: date stamp 19/03/16. @19.48) and new file path*/
    	
    	String srcRefPool = "D:\\LeaveOneOutCrossValidation_results\\thermal_scaled_kJperMol.csv";
        String destRList = "D:\\LeaveOneOutCrossValidation_results\\leave-one-out-x-validation\\hco\\";
           
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        List<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());
        
        LPSolver solver = new TerminalGLPKSolver(15000, true, true);
      
        /*Java code from previous version that is commented.*/
        /* solver.setDirectory(new File("C:\\Users\\pb556\\temp2\\")); */

        /**
         * @author nk510
         */
        
        /*testing main method by using recovered file:  thermal_scaled_kJperMol.csv and new file path */
        
        solver.setDirectory(new File("D:\\LeaveOneOutCrossValidation_temp2\\"));

        int[] ctrRuns = new int[]{1};
        int[] ctrRes = new int[]{25}; // 1, 5, 15, 25
        int[] ctrRadicals = new int[]{100, 0, 1, 2}; // 0, 1, 2, 3, 4, 5

        for (int z = 0; z < ctrRadicals.length; z++) {
            
        	int maxRadical = ctrRadicals[z];
            
            int timeout = 600;
            
            RadicalsFilter filter = new RadicalsFilter(0, maxRadical);

            for (int i = 0; i < ctrRuns.length; i++) {
                
            	for (int k = 0; k < ctrRes.length; k++) {
                    
                	String config = "isd_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    
                    System.out.println("Process configuration " + config);
                    
                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                        System.out.println("Skipping " + destRList + "\\" + config);
                        continue;
                    }

                    Collections.shuffle(refSpecies);
                    
                    int ctr = 1;
                    
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
                        Collections.shuffle(refPool);
                        ExecutorService executor = Executors.newSingleThreadExecutor();
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISDReactionType()));
                        poolModCalc.setMaximumSearchDepth(50);
                        MultiRunCalculator c =
                                new MultiRunCalculator(
                                poolModCalc);
                        c.setNumberOfRuns(ctrRuns[i]);
                        EnthalpyEstimationThread t = new EnthalpyEstimationThread(c, target, getPool(filter.filter(refPool), true));
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
                                try {
                                    ReactionList rList = new ReactionList();
                                    for (ReactionList l : results.get(s)) {
                                        rList.addAll(l);
                                    }
                                    completeRList.addAll(rList);
                                    ReactionSelector selector = new MedianReactionSelector();
                                    Reaction r = selector.select(rList).get(0);
                                    s.setHf(r.calculateHf());
                                    ttipSpecies.add(s);
                                } catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
                                    System.out.println("No data were calculated for " + s.getRef());
                                }
                            }

                            if (!new File(destRList + "\\" + target.getRef() + "\\").exists()) {
                                new File(destRList + "\\" + target.getRef() + "\\").mkdirs();
                            }

                            ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));
                            SpeciesPoolWriter spWriter = new SpeciesPoolWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_species-pool_median.csv"));

                            if (!completeRList.isEmpty()) {
                                System.out.println("Writting complete reaction list...");
                                rListWriter.set(completeRList);
                                rListWriter.overwrite(true);
                                rListWriter.write();
                            }

                            if (!ttipSpecies.isEmpty()) {
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

    protected static ObjectPool<Species> getPool(Collection<Species> pool, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<>();
        for (Species s : pool) {
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }

    protected static ObjectPool<Species> getPool(Collection<Species> pool, Species targetSpecies, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<>();
        for (Species s : pool) {
            if (s.equals(targetSpecies, true)) {
                continue;
            }
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }
}