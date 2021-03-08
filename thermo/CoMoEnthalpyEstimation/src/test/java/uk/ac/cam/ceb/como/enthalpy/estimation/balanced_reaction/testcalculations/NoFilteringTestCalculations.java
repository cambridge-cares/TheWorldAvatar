/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.testcalculations;

import java.io.File;
import java.io.FileNotFoundException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.AbsolutValueFeasibilityReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.FeasibleReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MeanReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MultiSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.RangeBasedReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;
import com.cmclinnovations.data.collections.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class NoFilteringTestCalculations {
    
    protected String poolSubSet = "230";
    protected String threshold = "25";
    protected int runs = 25;
    protected int numMean = 10;
    protected int radicals = 1;
    protected int numResults = 25;

    @Test
    public void ISD_MedianReactionSelector() throws FileNotFoundException, Exception, Exception {
        CSVParser parserPool = new SpeciesPoolParser(new File("test_data/analysis/pools/radical-" + radicals + "/pool_" + poolSubSet + "_radicals_" + radicals + ".csv"));
        parserPool.parse();

        Collection<Species> ref = parserPool.getRefSpecies();

        MultiSelector selector = new MultiSelector();
        //selector.add(new RangeBasedReactionSelector(10, 1));
        //selector.add(new FeasibleReactionSelector(-2500.0, 2500.0));
        //selector.add(new MedianReactionSelector());
        selector.add(new AbsolutValueFeasibilityReactionSelector());
        
        run(runs, "test_data/isd_nofiltering25_50runs", ref, selector, new MPSFormat(false, new ISDReactionType()));
    }
    
    @Test
    @Ignore
    public void noFiltering3459_83_4Test() throws FileNotFoundException, Exception, Exception {
        CSVParser parserPool = new SpeciesPoolParser(new File("test_data/analysis/pools/radical-" + radicals + "/pool_" + poolSubSet + "_radicals_" + radicals + ".csv"));
        parserPool.parse();
        
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/analysis/filtered/isd_subset_" + threshold + "_exclusion" + ".csv"));
        parserRef.parse();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        
        Species targetSpecies = null;
        for (Species s : parserPool.getRefSpecies()) {
            if (s.getRef().trim().compareToIgnoreCase("3459-83-4") == 0) {
                targetSpecies = s;
                break;
            }
        }

        if (targetSpecies != null) {
            Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
            MultiRunCalculator calculator = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, new MPSFormat(true, new ISDReactionType()), getPool(targetSpecies, parserPool.getRefSpecies())));
            calculator.calculate(targetSpecies);
            Map<Species, ReactionList> sol = (Map<Species, ReactionList>) calculator.get();
            for (Species s : sol.keySet()) {
                for (int i = 0; i < sol.get(s).size(); i++) {
                    Reaction r = sol.get(s).get(i);
                    System.out.println(r.toString() + " = " + r.calculateHf() + " / " + (s.getHf() - r.calculateHf()));
                }
                ReactionSelector selector = new MedianReactionSelector();
                ReactionList l = selector.select(sol.get(s));
                System.out.println(l.get(0).toString());
                System.out.println(l.get(0).calculateHf());
                System.out.println(s.getHf());
            }
        }
    }
    
    protected ObjectPool getPool(Collection<Species> ref) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (ref != null && !ref.isEmpty()) {
            for (Species s : ref) {
                pool.addValidated(s);
            }
        }
        return pool;
    }

    protected ObjectPool getPool(Species exclude, Collection<Species> ref) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (ref != null && !ref.isEmpty()) {
            for (Species s : ref) {
                if (!exclude.equals(s, true)) {
                    pool.addValidated(s);
                }
            }
        }
        return pool;
    }
    
    protected void run(int runs, String output, Collection<Species> ref, ReactionSelector selector, LPFormat format) throws Exception {
        run(runs, output, ref, ref, selector, format);
    }
    
    protected void run(int runs, String output, Collection<Species> soi, Collection<Species> ref, ReactionSelector selector, LPFormat format) throws Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Map<Species, Reaction> sol = new HashMap<Species, Reaction>();
        Map<Species, ReactionList> detSol = new HashMap<Species, ReactionList>();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
                System.out.println("Calculating enthalpy of formation for species " + s.getRef() + "...");
                
                ReactionList rList = new ReactionList();

                MultiRunCalculator mrCalc = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, format, getPool(s, ref)), selector);
                mrCalc.setNumberOfRuns(runs);
                mrCalc.calculate(s);

                
                //sdfasdfasdfasdf PROBLEM sdfjlasdjfoiasdjfioasj
                HashMap<Species, ReactionList> list = (HashMap<Species, ReactionList>) mrCalc.getCombinedResults();

                if (list.size() > 1) {
                    System.out.println("ERROR!");
                }
                
                for (Species sp : list.keySet()) {
                    rList = list.get(sp);
                }
                
//                for (Species sp : list.keySet()) {
//                    try {
//                        for (Reaction r : list.get(sp)) {
//                            for (int i = 0; i < l.size(); i++) {
//                                System.out.println(l.get(i).calculateHf());
//                            }
//                            Reaction r = selector.select(l).get(0);
//                            rList.add(r);
//                        }
//                    } catch (IndexOutOfBoundsException ioobe) {
//                        System.out.println(s.getRef() + " - reaction could not be selected");
//                    }
//                }

                try {
                    MedianReactionSelector m = new MedianReactionSelector();
                    Reaction r = m.select(rList).get(0);
//                    Reaction r = selector.select(rList).get(0);
                    sol.put(s, r);
                    detSol.put(s, rList);
                    System.out.println(s.getRef() + "(" + Math.abs(r.calculateHf() - s.getHf()) + "): " + r.calculateHf());
                } catch (IndexOutOfBoundsException ioobe2) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            } catch (LpSolverException lpse) {
                System.out.println(s.getRef() + " - " + lpse.getMessage());
            }

            //deleteFiles("test_data/.temp");
        }

        // save the results!!!
        ReactionListWriter writer = new ReactionListWriter();
        writer.set(new File(output + "_sol"));

        ReactionList solList = new ReactionList();
        for (Species s : sol.keySet()) {
            solList.add(sol.get(s));
        }

        writer.set(solList);
        writer.overwrite(true);
        writer.write();

        writer.set(new File(output + "_det"));

        solList = new ReactionList();
        for (Species s : detSol.keySet()) {
            solList.addAll(detSol.get(s));
        }

        writer.set(solList);
        writer.overwrite(true);
        writer.write();
    }

    protected void runMeanReactionSelector(int runs, String output, Collection<Species> ref, LPFormat format, int num) throws Exception {
        runMeanReactionSelector(runs, output, ref, ref, format, num);
    }
    
    protected void runMeanReactionSelector(int runs, String output, Collection<Species> soi, Collection<Species> ref, LPFormat format, int num) throws Exception {
        MeanReactionSelector selector = new MeanReactionSelector(num);
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Map<Species, Double> sol = new HashMap<Species, Double>();
        Map<Species, ReactionList> detSol = new HashMap<Species, ReactionList>();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
                System.out.println("Calculating enthalpy of formation for species " + s.getRef() + "...");
                
                ReactionList rList = new ReactionList();

                MultiRunCalculator mrCalc = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, format, getPool(s, ref)));
                mrCalc.setNumberOfRuns(runs);
                mrCalc.calculate(s);

                HashMap<Species, Collection<ReactionList>> list = (HashMap<Species, Collection<ReactionList>>) mrCalc.get();

                for (Species sp : list.keySet()) {
                    try {
                        for (ReactionList l : list.get(sp)) {
                            Reaction r = selector.select(l).get(0);
                            rList.add(r);
                        }
                    } catch (IndexOutOfBoundsException ioobe) {
                        System.out.println(s.getRef() + " - reaction could not be selected");
                    }
                }

                try {
                    ReactionList rMean = selector.select(rList);
                    double sum = 0.0;
                    for (Reaction r : rMean) {
                        sum += r.calculateHf();
                    }
                    double result = sum / rMean.size();
                    sol.put(s, result);
                    detSol.put(s, rList);
                    System.out.println(s.getRef() + "(" + Math.abs(result - s.getHf()) + "): " + result);
                } catch (IndexOutOfBoundsException ioobe2) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            } catch (LpSolverException lpse) {
                System.out.println(s.getRef() + " - " + lpse.getMessage());
            }

            //deleteFiles("test_data/.temp");
        }

        // save the results!!!
        StringListWriter strWriter = new StringListWriter();
        strWriter.set(output + "_sol");

        ArrayList<String> strList = new ArrayList<String>();
        for (Species s : sol.keySet()) {
            strList.add(s.getRef() + "," + Double.toString(sol.get(s)));
        }

        strWriter.overwrite(true);
        strWriter.setContent(strList);
        strWriter.write();

        ReactionListWriter writer = new ReactionListWriter();
        writer.set(new File(output + "_det"));

        ReactionList solList = new ReactionList();
        solList = new ReactionList();
        for (Species s : detSol.keySet()) {
            solList.addAll(detSol.get(s));
        }

        writer.set(solList);
        writer.overwrite(true);
        writer.write();
    }
}
