/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation.rebutal;

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
import uk.ac.cam.ceb.como.paper.enthalpy.utils.EvaluationUtils;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
 *
 * @author pb556
 */

public class CalculationISD {

    public static void main(String[] args) throws Exception {

        String srcCompoundsRef = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\esc\\";
        String srcRefPool = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\calc-enthalpy_scaled_kJperMol.csv";
        String destRList = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\reactions\\calculation\\hco_isd\\";
        String srcSoiPool = "C:\\Users\\pb556\\preprints\\methodology\\cnf_initial-submission\\rebuttal\\additional-calculations\\data\\geom_calc-enthalpy_scaled_kJperMol.csv";
        
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        refParser.parse();
        List<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());
        
        SpeciesPoolParser soiParser = new SpeciesPoolParser(new File(srcSoiPool));
        soiParser.parse();
        List<Species> soiSpecies = new ArrayList<>(soiParser.getAllSpecies());
        
        Collection<Species> invalids = new HashSet<>();
        Map<Species, Integer> spinMultiplicity = new HashMap<>();
        int ctr = 1;
        
        Set<Species> all = new HashSet<>();
        all.addAll(soiSpecies);
        all.addAll(refSpecies);

        refSpecies.removeAll(invalids);
        all.removeAll(invalids);
        soiSpecies.removeAll(invalids);

        LPSolver solver = new TerminalGLPKSolver(15000, false, true);
        solver.setDirectory(new File("C:\\Users\\pb556\\temp2\\"));

        int[] ctrRuns = new int[]{1};
        int[] ctrRes = new int[]{1,2,3,4,5,6,7,8,9,7,5,8,7,9,10,2,3,4,10,20,30,40,50,60,70,80,90,100}; // 1, 5, 15, 25
        int[] ctrRadicals = new int[]{100}; // 0, 1, 2, 3, 4, 5

        for (int z = 0; z < ctrRadicals.length; z++) {
            int maxRadical = ctrRadicals[z];
            int timeout = 2*1500;
            for (int i = 0; i < ctrRuns.length; i++) {
                for (int k = 0; k < ctrRes.length; k++) {
                    String config = "isd_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    System.out.println("Process configuration " + config);

                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                        System.out.println("Skipping " + destRList + "\\" + config);
                        continue;
                    }
                    Collections.shuffle(refSpecies);
                    Collections.shuffle(soiSpecies);
                    ctr = 1;
                    for (Species target : soiSpecies) {

                        Map<Species, Collection<ReactionList>> results = new HashMap<>();
                        System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + soiSpecies.size() + ")");
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
                        
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISDReactionType()));
                        
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
}
