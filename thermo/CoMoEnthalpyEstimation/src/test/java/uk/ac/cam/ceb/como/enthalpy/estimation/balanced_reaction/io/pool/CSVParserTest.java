/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import java.io.File;
import java.io.FileNotFoundException;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CSVParserTest {
    
    @Test
    public void parseCSVTest() throws FileNotFoundException, Exception, NoFeasibleSolutionException, LpSolverException, Exception {
        
        LPSolver solver = new TerminalGLPKSolver(false, false);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(false, new ISDReactionType()), new VariableFactory("v"));
        
        //Parser parser = new ISDParser(new File("test_data/csv/pool_test_err.csv"));
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/pool_reaction.csv"));
        parser.parse();
        for (Species s : parser.getRefSpecies()) {
            iss.addReferenceSpecies(s);
        }
        Species t = null;
        for (Species s : parser.getSpeciesOfInterest()) {
            t = s;
        }

        Reaction r = iss.solve(t);
        System.out.println(r.calculateHf());
    }
    
    @Test
    public void parseCSVTest1() throws FileNotFoundException, Exception, NoFeasibleSolutionException, LpSolverException {
        
//        LPSolver solver = new TerminalLPSolveSolver(false, false);
//        solver.setDirectory(new File("test_data/"));
//        Solver iss = new Solver(solver, new ISDLpSolveFormat(), new VariableFactory("v"));
//        
//        //Parser parser = new ISDParser(new File("test_data/csv/pool_test_err.csv"));
//        Parser parser = new ISDParser(new File("test_data/csv/pool_reaction.csv"));
//        parser.parse();
//        assert (parser.getRefSpecies().size() == 14);
//        assert (parser.getSpeciesOfInterest().size() == 1);
//        for (Species s : parser.getRefSpecies()) {
//            iss.addReferenceSpecies(s);
//            System.out.println(s.getRef() + ": " + s.getBondTypeMultiset().count("[C]{1}[C]") + ", " + s.getBondTypeMultiset().count("[C]{1}[H]"));
//        }
//        Species t = null;
//        for (Species s : parser.getSpeciesOfInterest()) {
//            t = s;
//            System.out.println(s.getRef() + ": " + s.getBondTypeMultiset().count("[C]{1}[C]") + ", " + s.getBondTypeMultiset().count("[C]{1}[H]"));
//        }
//        
//        ISDLpSolveFormat f = new ISDLpSolveFormat();
//        System.out.println(f.getInputString(t, parser.getRefSpecies(), new VariableSet(new VariableFactory("v"))));
//        
//        Reaction r = iss.solve(t);
//        System.out.println(r.calculateHf());
//        
//        int cc = 0;
//        int ch = 0;
//        for (Species s : r.getProducts().keySet()) {
//            cc += r.getProducts().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[C]");
//            ch += r.getProducts().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[H]");
//            System.out.println(s.getRef() + " x " + r.getProducts().get(s).doubleValue());
//        }
//        System.out.println("Products: " + cc + ", " + ch);
//        
//        cc = 0;
//        ch = 0;
//        for (Species s : r.getReactants().keySet()) {
//            cc += r.getReactants().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[C]");
//            ch += r.getReactants().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[H]");
//            System.out.println(s.getRef() + " x " + r.getReactants().get(s).doubleValue());
//        }
//        System.out.println("Reactants: " + cc + ", " + ch);
//       
//        try {
//            System.out.println(t.getRef() + " x " + r.getProducts().get(t).doubleValue() + "(Product)");
//        } catch (Exception e) {
//            System.out.println(t.getRef() + " x " + r.getReactants().get(t).doubleValue() + "(Reactant)");
//        }
    }
    
    @Test
    public void parseCSVTest2() throws FileNotFoundException, Exception, NoFeasibleSolutionException, LpSolverException, Exception {
        
        LPSolver solver = new TerminalGLPKSolver(false, false);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(false, new ISDReactionType()), new VariableFactory("v"));
        
        CSVParser parser = new SpeciesPoolParser(new File("test_data/csv/pool_test_err.csv"));
        parser.parse();
        assert (parser.getRefSpecies().size() == 3);
        assert (parser.getSpeciesOfInterest().size() == 1);
        for (Species s : parser.getRefSpecies()) {
            iss.addReferenceSpecies(s);
            System.out.println(s.getRef() + ": " + s.getBondTypeMultiset().count("[C]{1}[C]") + ", " + s.getBondTypeMultiset().count("[C]{1}[H]"));
        }
        Species t = null;
        for (Species s : parser.getSpeciesOfInterest()) {
            t = s;
            System.out.println(s.getRef() + ": " + s.getBondTypeMultiset().count("[C]{1}[C]") + ", " + s.getBondTypeMultiset().count("[C]{1}[H]"));
        }
        
        MPSFormat f = new MPSFormat(false, new ISDReactionType());
        System.out.println(f.getInputString(t, parser.getRefSpecies(), new VariableSet(new VariableFactory("v"))));
        
        Reaction r = iss.solve(t);
        System.out.println(r.calculateHf());
        
        int cc = 0;
        int ch = 0;
        for (Species s : r.getProducts().keySet()) {
            cc += r.getProducts().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[C]");
            ch += r.getProducts().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[H]");
            System.out.println(s.getRef() + " x " + r.getProducts().get(s).doubleValue());
        }
        System.out.println("Products: " + cc + ", " + ch);
        
        cc = 0;
        ch = 0;
        for (Species s : r.getReactants().keySet()) {
            cc += r.getReactants().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[C]");
            ch += r.getReactants().get(s).doubleValue() * s.getBondTypeMultiset().count("[C]{1}[H]");
            System.out.println(s.getRef() + " x " + r.getReactants().get(s).doubleValue());
        }
        System.out.println("Reactants: " + cc + ", " + ch);
       
        try {
            System.out.println(t.getRef() + " x " + r.getProducts().get(t).doubleValue() + "(Product)");
        } catch (Exception e) {
            System.out.println(t.getRef() + " x " + r.getReactants().get(t).doubleValue() + "(Reactant)");
        }
    }
}
