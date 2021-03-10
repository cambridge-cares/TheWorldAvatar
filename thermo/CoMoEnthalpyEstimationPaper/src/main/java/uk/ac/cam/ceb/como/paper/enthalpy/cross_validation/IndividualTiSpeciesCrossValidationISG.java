/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.cross_validation;

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

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
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
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

/**
 *
 * @author pb556
 * 
 */

public class IndividualTiSpeciesCrossValidationISG {

    private static String srcCompoundsRefTi = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\TiCl4\\g09\\" ; //"C:\\Users\\pb556\\workspace\\ttip_thermo\\calculations\\dHf\\g09\\ti\\";
    
    private static String srcCompoundsRefHCO = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\esc\\g09\\"; //"C:\\Users\\pb556\\workspace\\ttip_thermo\\calculations\\dHf\\g09\\hco\\";
    
    public static void main(String[] args) throws Exception {

        Map<String, Integer[]> mapElPairing = new HashMap<String, Integer[]>();
        
        String srcRefHCOPool = "C:\\Users\\NK\\Documents\\philipp\\171-pb556\\ref-enthalpy_scaled_kJperMol.csv";//"C:\\Users\\pb556\\workspace\\ttip_thermo\\calculations\\dHf\\hco_scaled_kJperMols.csv"; // maybe this is output of LeaveOneOutCrossValidation.java calculations 
        
        String srcRefTiPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";//"C:\\Users\\pb556\\workspace\\ttip_thermo\\calculations\\dHf\\ti_scaled_kJperMols_no-TiO.csv";
        
        String destRList = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\xvalidation\\";//"C:\\Users\\pb556\\workspace\\ttip_thermo\\calculations\\dHf-x-validation\\dHf-x-validation-isg\\dHf_rct_isg_x-validation-refined-noTiO\\";
        
        int[] ctrRuns = new int[]{1};
        int[] ctrRes = new int[]{5}; // 1, 5, 15, 25
        int[] ctrRadicals = new int[]{100, 1, 2, 3, 4, 5}; // 0, 1, 2, 3, 4, 5

        System.out.println("REF-Ti: Processing Ti-reference data");
        
        SpeciesPoolParser refTiParser = new SpeciesPoolParser(new File(srcRefTiPool));

        refTiParser.parse();
        
        Collection<Species> refTi = refTiParser.getRefSpecies();
        Collection<Species> invalidTi = new HashSet<Species>();
        
        int ctr = 1;
        
        for (Species s : refTi) {
            System.out.println("REF-Ti: Processing " + ctr + " / " + refTi.size());
            ctr++;
            File f = new File(srcCompoundsRefTi + s.getRef().replace(".g09", "") + ".g09");
            System.out.println(f.getName());
            if (f.exists()) {
                Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                if (e != null) {
                    mapElPairing.put(s.getRef(), e);
                } else {
                    System.out.println("REF-Ti: e- pairing could not be determined for " + s.getRef());
                    invalidTi.add(s);
                }
            } else {
                System.out.println("REF-Ti: No file found for " + s.getRef());
                invalidTi.add(s);
            }
        }

        refTi.removeAll(invalidTi);

        System.out.println("REF-HCO: Processing HCO-reference data");
        
        SpeciesPoolParser refHCOParser = new SpeciesPoolParser(new File(srcRefHCOPool));
        
        refHCOParser.parse();
        
        Collection<Species> refHCO = refHCOParser.getRefSpecies();
        
        Collection<Species> invalidHCO = new HashSet<Species>();
        
        ctr = 1;
        
        for (Species s : refHCO) {
            System.out.println("REF-HCO: Processing " + ctr + " / " + refHCO.size());
            ctr++;
            File f = new File(srcCompoundsRefHCO + s.getRef().replace(".g09", "") + ".g09");
            if (f.exists()) {
                Integer[] e = HfSpeciesConverter.getNumberOfElectrons(HfSpeciesConverter.parse(f));
                if (e != null) {
                    mapElPairing.put(s.getRef(), e);
                } else {
                    System.out.println("REF-HCO: e- pairing could not be determined for " + s.getRef());
                    invalidHCO.add(s);
                }
            } else {
                System.out.println("REF-HCO: No file found for " + s.getRef());
                invalidHCO.add(s);
            }
        }
        
        refHCO.removeAll(invalidHCO);

        SolverHelper.add(mapElPairing);
        
        LPSolver solver = new TerminalGLPKSolver(15000, true, true);
        
//      solver.setDirectory(new File("C:\\Users\\pb556\\temp2\\"));
        
        solver.setDirectory(new File("D:\\Data-Philip\\LeaveOneOutCrossValidation_temp"));

        for (int z = 0; z < ctrRadicals.length; z++) {
            int maxRadical = ctrRadicals[z];
            int timeout = 1500;
            RadicalsFilter filter = new RadicalsFilter(0, maxRadical);

            for (int i = 0; i < ctrRuns.length; i++) {
                for (int k = 0; k < ctrRes.length; k++) {
                    String config = "isg_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    System.out.println("Process configuration " + config);
                    
                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                        System.out.println("Skipping " + destRList + "\\" + config);
                        continue;
                    }

                    ctr = 1;
                    for (Species target : refTi) {

                        Map<Species, Collection<ReactionList>> results = new HashMap<Species, Collection<ReactionList>>();
                        System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + refTi.size() + ")");
                        ctr++;

                        if (new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
                            continue;
                        }

                        List<Species> refPool = new ArrayList<Species>();
                        refPool.addAll(refHCO);
                        for (Species s : refTi) {
                            if (s.getRef().compareTo(target.getRef()) == 0) {
                                continue;
                            }
                            refPool.add(s);
                        }
                        Collections.shuffle(refPool);
                        ExecutorService executor = Executors.newSingleThreadExecutor();
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISGReactionType(true)));
                        poolModCalc.setMaximumSearchDepth(50);
                        MultiRunCalculator c =
                                new MultiRunCalculator(
                                poolModCalc);
                        c.setNumberOfRuns(ctrRuns[i]);
                        Collections.shuffle(refPool);
                        
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
                            } catch (TimeoutException e) {
                                System.out.println("Terminated!");
                                Map<Species, Collection<ReactionList>> re = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                                if (re != null) {
                                    for (Species sR : re.keySet()) {
                                        results.put(target, re.get(sR));
                                    }
                                } else {
                                    results.put(target, null);
                                }
                            } catch (InterruptedException ex) {
                                System.out.println("Terminated!");
                                Map<Species, Collection<ReactionList>> re = (Map<Species, Collection<ReactionList>>) t.getCalculator().get();
                                if (re != null) {
                                    for (Species sR : re.keySet()) {
                                        results.put(target, re.get(sR));
                                    }
                                } else {
                                    results.put(target, null);
                                }
                            } catch (ExecutionException ex) {
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
                            
                            Collection<Species> ttipSpecies = new HashSet<Species>();
                            
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
                                } catch (ArrayIndexOutOfBoundsException aioobe) {
                                    System.out.println("No data were calculated for " + s.getRef());
                                } catch (NullPointerException npe) {
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
        ObjectPool<Species> newPool = new ObjectPool<Species>();
        for (Species s : pool) {
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }

    protected static ObjectPool<Species> getPool(Collection<Species> pool, Species targetSpecies, boolean validatedSpeciesOnly) {
        ObjectPool<Species> newPool = new ObjectPool<Species>();
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
