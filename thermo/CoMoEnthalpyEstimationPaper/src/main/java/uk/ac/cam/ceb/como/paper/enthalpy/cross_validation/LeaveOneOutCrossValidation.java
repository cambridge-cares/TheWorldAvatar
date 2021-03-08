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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.paper.enthalpy.threading.EnthalpyEstimationThread;
import uk.ac.cam.ceb.como.tools.file.writer.StringWriter;

 /**
 *
 * @author pb556
 */

/**
 * 
 * @author nk510@cam.ac.uk
 * @version 1.0
 * @since 2018-04-2
 * 
 * Input data: 
 * Cross-validation algorithm (drawn in Figure 6.9 [1], page.149) requires reference data set that is given in file thermal_scaled_kJperMol.csv (data stamp: March-19th-2016,  @07.48pm).
 * 
 * Calculation/Iterations: 
 * - There are at least two iterations of calculations. It takes a few days (more than 5 days) to complete calculations.
 *   
 * Output of calculations:
 * - Estimates dHf (298.15K) for selected 920 reference species.
 * - Generates complete reaction (rst file) list and species list (csv file) in folders named as same as species id's name. Species's id (CAS Registry Number) is given in first column of input 'csv' file. 
 * - Generates files in '.temp' folder. The folder has more than 66GB of data.  
 * - Based on [1] (page 149), it should return 'a set of consistent (accepted) and a set of inconsistent (rejected) reference data' [1]. 
 *   Note:  I did not find in this class the implementation of methods for checking automatically the consistency of target species.
 *   
 *   References used in documentation:
 *   
 *   [1] Philipp Buerger, First-Principles Investigation of Titanium Dioxide Gas-Phase Precursor Chemistry, PhD thesis, St Edmund’s College, February 7, 2017.
 *   [2] GNU Linear Programming Kit (GLPK), Version 4.58. URL http://www.gnu.org/software/glpk/glpk.html,  2016.
 *   [3] lp_solve. Version 5.5.2.0. URL http://lpsolve.sourceforge.net/, 2016.
 * 
 */

public class LeaveOneOutCrossValidation {

    public static void main(String[] args) throws Exception {

    	/**
    	 * 
    	 * @author nk510@cam.ac.uk
    	 * @version 1.0
         * @since 2018-04-20
         * 
    	 */
    	
        /*
         * (non-Javadoc)
         * Commented Java code from previous version:
         *  
         * String srcRefPool = "C:\\Users\\pb556\\workspace\\methodology\\results\\thermal_scaled_kJperMol.csv";
         * String destRList = "C:\\Users\\pb556\\workspace\\methodology\\results\\leave-one-out-x-validation\\hco\\";
         * 
        */
    	
    	/**
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         * 
    	 * srcRefPool: Testing main method by using recovered file: thermal_scaled_kJperMol.csv (date stamp: date stamp 19/03/16. @19.48) and created new file path.   	
    	 * Input file contains information about 920 species
    	 * 
         */    	 
    	
//    	String srcRefPool = "D:\\LeaveOneOutCrossValidation_results\\thermal_scaled_kJperMol.csv";
    	
//    	String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\179-pb556-DetailedMethodology\\calc-enthalpy_scaled_kJperMol.csv";
    	
    	String srcRefPool = "C:\\Users\\NK\\Documents\\philipp\\180-pb556\\ref_scaled_kJperMols_v8.csv";
    	    	
//    	String srcRefPool ="C:\\Users\\NK\\Documents\\philipp\\171-pb556\\ref-enthalpy_scaled_kJperMol3.csv";
    	
    	/**
    	 * @author nk510
         * @version 1.0
         * @since 2018-04-20
         * 
         * destRList: Output folder that contains folders named by species-id (using 'ref' attribute in Java class 'Species')
    	 * Each folder could be empty (no calculation output) such as 2409-55-4, or contains one or more 'rct', and 'csv' files. 
    	 * The folder is created ones.
    	 * 
         */
    	
        String destRList = "D:\\Data-Philip\\LeaveOneOutCrossValidation_results\\";
        
        /**
         * 
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         *
         *  refParser: Creates object variable of  SpeciesPoolParser by using input csv file.
         *  SpeciesPoolParser class implements methods for parsing input csv file. It extends CSVParser (extends FileParser<Set<Species>>) class.
         *  
         */
        
        SpeciesPoolParser refParser = new SpeciesPoolParser(new File(srcRefPool));
        
        refParser.parse();
        
        /**
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         * 
         * refSpecies:
         * Creates reference species list. See 'Overview of the method used to estimate the enthalpy of formation' in Figure 6.7, page 144 [1].  
         * Creates Java data structure ArrayList of object variables of Species class by using the object variable 'refParser' of SpeciesPoolParser class. 
         *  
         */
        
        List<Species> refSpecies = new ArrayList<>(refParser.getRefSpecies());

        
        /**
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         *
         *solver:
         * Object variable 'solver' is created as a instance of class LPSolver, over TerminalGLPKSolver class constructor.   
         * The class TerminalGLPKSolver calls The GNU Linear Programming Kit (GLPK) [2] and 
         * lp_solve [3] linear programming solver which were used in preliminary tests [1] (page 145.) 
         * 
         */
        
        LPSolver solver = new TerminalGLPKSolver(15000, true, true); 
        
        /**
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         */
        
        /*
         * (non-Javadoc)
         * Commented Java code from previous version.
         * solver.setDirectory(new File("C:\\Users\\pb556\\temp2\\")); 
         * 
         */

        /**
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         *          
         * Generates files in '.temp' folder. These files contain information about Karush-Kuhn-Tucker optimality 
         * conditions (https://en.wikipedia.org/wiki/Karush_Kuhn_Tucker_conditions), as well as other information about calculations for species 
         * such as Problem (example: 3170-83-0), Rows (example: 1806), Columns (example: 1790), Non-zeros (example: 8565), Status (example: UNDEFINED), 
         * Objective (example: R0 = 0 (MINimum)), etc.
         * 
         * The '.temp' file has more than 66GB of data.
         * 
         * */

        solver.setDirectory(new File("D:\\Data-Philip\\LeaveOneOutCrossValidation_temp\\"));

        /**
         * 
         * @author nk510
         * @version 1.0
         * @since 2018-04-20
         *          
         * I can not understand what is the meaning of integer arrays defined below, used as a maximum number of loops in calculations. They are used in calculation loop as a lower/upper bounds like in matrix dimensions [crtRuns,crtRes].
         * I did not find in PhD thesis [1] term 'radicals' used in pseudo code of Chapter 6 [1]. Term 'radicals' appears in Chapter 2 [1].
         * 
         */
        
        int[] ctrRuns = new int[]{1};
        int[] ctrRes = new int[]{5}; // 1, 5, 15, 25  // first version: int[] ctrRes = new int[]{25};
        int[] ctrRadicals = new int[]{100}; // 100, 1, 2, 3, 4, 5 |  0, 1, 2, 3, 4, 5  //first version: int[] ctrRadicals = new int[]{100, 0, 1, 2};
        
        for (int z = 0; z < ctrRadicals.length; z++) {
            
        	int maxRadical = ctrRadicals[z];
            
//            int timeout = 600;
      
        	  int timeout = 1500;
            
            RadicalsFilter filter = new RadicalsFilter(0, maxRadical);

            for (int i = 0; i < ctrRuns.length; i++) {
                
            	for (int k = 0; k < ctrRes.length; k++) {
                    
                	String config = "isd_runs" + ctrRuns[i] + "_res" + ctrRes[k] + "_radicals" + maxRadical + "_" + timeout + "s";
                    
                    System.out.println("Process configuration " + config);
                    
                    /**
                     * @author nk510
                     * @version 1.0
                     * @since 2018-04-20
                     *
                     * Skips creating output folder twice.
                     */
                    
                    if (new File(destRList + "\\" + config + ".txt").exists()) {
                        System.out.println("Skipping " + destRList + "\\" + config);
                        continue;
                    }
                    
                    Collections.shuffle(refSpecies);
                    
                    int ctr = 1;
                    
                    /**
                     * @author nk510
                     * @version 1.0
                     * @since 2018-04-20
                     *                      
                     * Iterates over parsed species reference list (ArrayList data structure)  
                     */
                    
                    for (Species target : refSpecies) {

                    	/**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                    	 * Results of calculations are stored in Java Map data structure. 
                    	 * For Key-Set of this Java Map, Philipp used {@link Species#getRef()} attribute defined in Species class as a 'private String'.  
                    	 * 
                    	 */
                    	
                        Map<Species, Collection<ReactionList>> results = new HashMap<>();
                        
                        System.out.println("Estimating dHf(298.15K) for species " + target.getRef() + " (" + ctr + " / " + refSpecies.size() + ")");
                        
                        ctr++;

                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                         * Skips existing (already created) reaction list files.
                         * 
                         */
                        
                        if (new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct").exists()) {
                            continue;
                        }

                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                         * refPool: Creates reference species pool (see page page 102, 3rd paragraph  [1]) as a Java List data structure. 
                         * 
                         */
                        
                        List<Species> refPool = new ArrayList<>();
                        
                        refPool.addAll(refSpecies);
                        
                        refPool.remove(target);
                        
                        Collections.shuffle(refPool);
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                         * ExecutorService's object variable is responsible for  termination of algorithm implemented in main method of Java class. 
                         * For more @see <a href="https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ExecutorService.html">ExecutorService</a> 
                         * 
                         */
                        ExecutorService executor = Executors.newSingleThreadExecutor();
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                         * PoolModificationCalculator's object variable 'poolModCalc' remembers (get/set methods) number of results, 
                         * maximum search depth, maximum number of attempts. 
                         *                          
                         */
                        
                        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new ISGReactionType(true)));
                        
//                      PoolModificationCalculator poolModCalc = new PoolModificationCalculator(ctrRes[k], solver, new MPSFormat(false, new HDReactionType()));
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *  
                         * Maximum search depth set to 50. For more information about maximum search depth, please see Figure 6.8 on page 146 [1].
                         *                          
                         */
                        poolModCalc.setMaximumSearchDepth(50);
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *  
                         * Object variable 'c' remembers number of maximum search depth, and the number of runs  (absolute value of given integer). 
                         *                          
                         */
                        
                        MultiRunCalculator c = new MultiRunCalculator(poolModCalc);
                        
                        c.setNumberOfRuns(ctrRuns[i]);
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *  
                         * Object variable {@link EnthalpyEstimationThread#t} of EnthalpyEstimationThread class is created. 
                         * EnthalpyEstimationThread implements <a href="https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Callable.html">Callable</a> interface by using 
                         * Species and collection of ReactionList object variables. Implemented {@link Callable#call()} method returns results or may throw an exception.                         
                         */
                        EnthalpyEstimationThread t = new EnthalpyEstimationThread(c, target, getPool(filter.filter(refPool), true));
                        
                        Future<Map<Species, Collection<ReactionList>>> future = executor.submit(t);
                        
                        /**
                    	 * @author nk510
                         * @version 1.0
                         * @since 2018-04-20
                         *
                         *  Try - catch block stores result of calculations (species list and reaction list) in Java Map object variable 'results'.   
                         *                                                     
                         */
                        
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
                           
                            /**
                        	 * @author nk510
                             * @version 1.0
                             * @since 2018-04-20
                             *
                             *  Object variable {@link #ttipSpecies} remembers data about species taken from results of calculations by using {@link ReactionSelector#selector} object variable.   
                             *  Calculate the enthalpy of formation of a target species.
                             *                                                     
                             */
                            
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
                                    
                                    /**
                                	 * @author nk510
                                     * @version 1.0
                                     * @since 2018-04-20
                                     *
                                     *  Calculates the enthalpy of formation of a target species, if species is not part of the isodesmic reaction.
                                     *  Set the result of calculation to ({@link Species#getHf()} variable as attribute of Species class.
                                     *                                                     
                                     */
                                    
                                    s.setHf(r.calculateHf());
                                    
                                    /**
                                	 * @author nk510
                                     * @version 1.0
                                     * @since 2018-04-20
                                     *
                                     * Adds data about species into the collection.
                                     *                                                     
                                     */
                                    
                                    ttipSpecies.add(s);
                                    
                                } catch (ArrayIndexOutOfBoundsException | NullPointerException aioobe) {
                                    System.out.println("No data were calculated for " + s.getRef());
                                }
                            }
                            
                            if (!new File(destRList + "\\" + target.getRef() + "\\").exists()) {
                                new File(destRList + "\\" + target.getRef() + "\\").mkdirs();
                            }

                            /**
                        	 * @author nk510
                             * @version 1.0
                             * @since 2018-04-20
                             *
                             * Generates (writes) target species reaction list file stored in folder named by value of first column of input csv file.
                             * To do that, here it is used {@link Species#getRef()} variable.
                             *                                                     
                             */
                            
                            ReactionListWriter rListWriter = new ReactionListWriter(new File(destRList + "\\" + target.getRef() + "\\" + config + "_reaction-list.rct"));
                            
                            /**
                        	 * @author nk510
                             * @version 1.0
                             * @since 2018-04-20
                             *
                             * Generates (writes) calculated pool median for a species. Species reference is given by using {@link Species#ref} variable.
                             *                                                     
                             */
                            
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