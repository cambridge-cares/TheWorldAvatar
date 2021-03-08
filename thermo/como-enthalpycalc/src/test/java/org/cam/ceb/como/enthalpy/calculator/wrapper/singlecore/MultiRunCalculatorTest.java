/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.reaction.DistinctReactionList;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.MedianReactionSelector;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.ReactionListValidation;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.RemoveFlaggedReactions;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.NoFeasibleSolutionException;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.MPSFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 * 
 */
public class MultiRunCalculatorTest {

    protected int numResults = 25;
    
    @Test
    public void simpleTest() {
    	
    }

    @Test
    public void extensiveTest() {
    	
    }

    @Test
    @Ignore
    public void multiRunTest3() throws FileNotFoundException, IOException, Exception {
        
    	LPSolver solver = new TerminalGLPKSolver(true, true);
        
        solver.setDirectory(new File("test_data/"));
        
        // CSVParser parserRef = new ISDParser(new File("test_data/csv/pool_hydrocarbons.csv"));
        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_no_radicals_no_marking.csv"));
        // test_data/csv/subset_25.csv

        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_3_629-11-8.csv"));
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/csv/subset_25_marked.csv"));
        CSVParser parserSoi = new SpeciesPoolParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3.csv"));
        //CSVParser parserSoi = new ISDParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3_629-11-8.csv"));

        parserRef.parse();
        parserSoi.parse();

        ArrayList<String> flag = new ArrayList<String>();
        flag.add("1");
        flag.add("2");

        //Collection<Species> ref = parserRef.getRefSpecies(flag);

        Collection<Species> ref = parserRef.getRefSpecies(flag);
        //Collection<Species> soi = parserSoi.getSpeciesOfInterest();
        Collection<Species> soi = parserSoi.getRefSpecies();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
                ReactionList rList = new ReactionList();

                MultiRunCalculator mrCalc = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), getPool(s, ref)));
                mrCalc.calculate(s);
                HashMap<Species, Collection<ReactionList>> list = (HashMap<Species, Collection<ReactionList>>) mrCalc.get();

                MedianReactionSelector selector = new MedianReactionSelector();

                for (Species sp : list.keySet()) {
                    try {
                        for (ReactionList l : list.get(sp)) {
                            DistinctReactionList dList = new DistinctReactionList(l);
                            Reaction r = selector.select(dList.getDistinctList()).get(0);
                            //System.out.println(r.calculateHf());
                            rList.add(r);
                            //System.out.println(r.calculateHf());
                        }
                    } catch (IndexOutOfBoundsException ioobe) {
                        System.out.println(s.getRef() + " - reaction could not be selected");
                    }

                }

                try {
                    // remove flagged species
                    RemoveFlaggedReactions rem = new RemoveFlaggedReactions(parserRef.getFlags(), "2");
                    ReactionList newRList = rem.select(rList);
                    Reaction r1 = selector.select(rList).get(0);
                    System.out.println("***SSS***" + s.getRef() + "(" + Math.abs(r1.calculateHf() - s.getHf()) + "): " + r1.calculateHf());
                    if (!newRList.isEmpty()) {
                        Reaction r = selector.select(newRList).get(0);
                        System.out.println(s.getRef() + "(" + Math.abs(r.calculateHf() - s.getHf()) + "): " + r.calculateHf());
                    }
                } catch (IndexOutOfBoundsException ioobe2) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            }
        }
    }

    @Test
    @Ignore
    public void multiRunTest2() throws FileNotFoundException, IOException, Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        // CSVParser parserRef = new ISDParser(new File("test_data/csv/pool_hydrocarbons.csv"));
        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_no_radicals_no_marking.csv"));
        // test_data/csv/subset_25.csv

        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_3_629-11-8.csv"));
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/csv/subset_25_3.csv"));
        CSVParser parserSoi = new SpeciesPoolParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3.csv"));
        //CSVParser parserSoi = new ISDParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3_629-11-8.csv"));

        parserRef.parse();
        parserSoi.parse();

        Collection<Species> ref = parserRef.getRefSpecies();
        //Collection<Species> soi = parserSoi.getSpeciesOfInterest();
        Collection<Species> soi = parserSoi.getRefSpecies();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
                ReactionList rList = new ReactionList();

                MultiRunCalculator mrCalc = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), getPool(s, ref)));
                mrCalc.calculate(s);
                HashMap<Species, Collection<ReactionList>> list = (HashMap<Species, Collection<ReactionList>>) mrCalc.get();

                MedianReactionSelector selector = new MedianReactionSelector();

                for (Species sp : list.keySet()) {
                    try {
                        for (ReactionList l : list.get(sp)) {
                            DistinctReactionList dList = new DistinctReactionList(l);
                            Reaction r = selector.select(dList.getDistinctList()).get(0);
                            //System.out.println(r.calculateHf());
                            rList.add(r);
                            //System.out.println(r.calculateHf());
                        }
                    } catch (IndexOutOfBoundsException ioobe) {
                        System.out.println(s.getRef() + " - reaction could not be selected");
                    }

                }

                try {
                    //DistinctReactionList dList = new DistinctReactionList(rList);
                    ReactionListValidation val = new ReactionListValidation(rList);
                    if (val.isValidDistribution(20 * 20)) {
                        Reaction r = selector.select(rList).get(0);
                        System.out.println(s.getRef() + "(" + Math.abs(r.calculateHf() - s.getHf()) + "): " + r.calculateHf());
                    } else {
                        Reaction r = selector.select(rList).get(0);
                        System.out.println("***" + s.getRef() + "(" + Math.abs(r.calculateHf() - s.getHf()) + "): " + r.calculateHf());
                    }
                } catch (IndexOutOfBoundsException ioobe2) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            }
        }
    }

    @Test
    @Ignore
    public void multiRunTest() throws FileNotFoundException, IOException, Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        // CSVParser parserRef = new ISDParser(new File("test_data/csv/pool_hydrocarbons.csv"));
        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_no_radicals_no_marking.csv"));
        // test_data/csv/subset_25.csv

        //CSVParser parserRef = new ISDParser(new File("test_data/csv/subset_25_3_629-11-8.csv"));
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/csv/subset_25_3.csv"));
        CSVParser parserSoi = new SpeciesPoolParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3.csv"));
        //CSVParser parserSoi = new ISDParser(new File("W:\\Data\\TTIP\\NIST\\isd\\pool_hydrocarbons3_629-11-8.csv"));

        parserRef.parse();
        parserSoi.parse();

        Collection<Species> ref = parserRef.getRefSpecies();
        //Collection<Species> soi = parserSoi.getSpeciesOfInterest();
        Collection<Species> soi = parserSoi.getRefSpecies();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
                ReactionList rList = new ReactionList();

                MultiRunCalculator mrCalc = new MultiRunCalculator(new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), getPool(s, ref)));
                mrCalc.calculate(s);
                HashMap<Species, Collection<ReactionList>> list = (HashMap<Species, Collection<ReactionList>>) mrCalc.get();

                MedianReactionSelector selector = new MedianReactionSelector();

                double sum = 0.0;
                double ctr = 0;

                for (Species sp : list.keySet()) {
                    try {
                        for (ReactionList l : list.get(sp)) {
                            DistinctReactionList dList = new DistinctReactionList(l);
                            Reaction r = selector.select(dList.getDistinctList()).get(0);
                            System.out.println(r.calculateHf());
                            sum += r.calculateHf();
                            ctr = ctr + 1;
                            rList.add(r);
                            //System.out.println(r.calculateHf());
                        }
                    } catch (IndexOutOfBoundsException ioobe) {
                        System.out.println(s.getRef() + " - reaction could not be selected");
                    }

                }

                try {
                    DistinctReactionList dList = new DistinctReactionList(rList);
                    Reaction r = selector.select(dList.getDistinctList()).get(0);
                    System.out.println(s.getRef() + "(" + Math.abs(r.calculateHf() - s.getHf()) + "/" + Math.abs(sum / ctr - s.getHf()) + "): " + r.calculateHf() + " vs " + sum / ctr);
                } catch (IndexOutOfBoundsException ioobe2) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            }
        }
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
}
