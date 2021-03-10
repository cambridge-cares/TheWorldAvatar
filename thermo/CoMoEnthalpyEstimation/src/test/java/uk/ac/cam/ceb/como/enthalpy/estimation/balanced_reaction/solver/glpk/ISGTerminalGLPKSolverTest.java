/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import java.io.File;
import java.util.HashSet;
import java.util.LinkedHashSet;

import org.junit.Ignore;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;

/**
 *
 * @author pb556
 * 
 */
public class ISGTerminalGLPKSolverTest {
    
    protected double maxErr = 50;
    
    @Test
    @Ignore
    public void solveDoubleAdvancedTest()  throws Exception, NoFeasibleSolutionException, LpSolverException {
        // read species pool
        SpeciesPoolParser parser = new SpeciesPoolParser(new File("test_data/test_set.csv"));
        parser.parse();
        
        /**
         * @author nk510 (caresssd@hermes.ca.ac.uk)
         * Two lines below are added. We made changes: 
         * 1. HashSet<Species> allSpecies - > ArrayList<Species> allSpecies
         * 2. HashSet<Species> soi -> ArrayList<Species> soi
         * Commneted because of usage LinkedHashSet<Species> as data structure for the same objects (allSpecies and soi).
         */
//      ArrayList<Species> allSpecies = (ArrayList<Species>)parser.getRefSpecies();
//      ArrayList<Species> soi = (ArrayList<Species>) parser.getSpeciesOfInterest();
        
        /**
         * @author nk510 (caresssd@hermes.cam.ac.uk)
         * Two lines below are commented from original source code.
         */
        LinkedHashSet<Species> allSpecies = (LinkedHashSet<Species>) parser.getRefSpecies();
        LinkedHashSet<Species> soi = (LinkedHashSet<Species>) parser.getSpeciesOfInterest();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        
        for (Species s : allSpecies) {
            iss.addReferenceSpecies(s);
        }
        
        for (Species s : soi) {
            Reaction reaction = iss.solve(s);
            System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        }
    }
    
    @Test
    public void solveDoubleAdvanced_1634_04_4_Test()  throws Exception, NoFeasibleSolutionException, LpSolverException {
        // read species pool
        SpeciesPoolParser parser = new SpeciesPoolParser(new File("test_data/test_set_1634-04-4.csv"));
        parser.parse();
        HashSet<Species> allSpecies = (HashSet<Species>) parser.getAllSpecies();
        
        Species sp_540 = null;
        Species sp_3249 = null;
        Species sp_1634 = null;
        
        for (Species s : allSpecies) {
            if (s.getRef().contains("540-67-0")) {
                sp_540 = s;
            }
            if (s.getRef().contains("3249-47-6")) {
                sp_3249 = s;
            }
            if (s.getRef().contains("1634-04-4")) {
                sp_1634 = s;
            }
        }
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(sp_540);
        iss.addReferenceSpecies(sp_3249);

        Reaction reaction = iss.solve(sp_1634);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        //assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
    }
    
    @Test
    @Ignore
    public void solveIntegerExactTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H4O);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }

    @Test
    @Ignore
    public void solveIntegerTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        //iss.addReferenceSpecies(C2H4);
        iss.addReferenceSpecies(C2H4O);
        iss.addReferenceSpecies(C2H6);
        iss.addReferenceSpecies(C2H6O);
        iss.addReferenceSpecies(C3H6);
        iss.addReferenceSpecies(C3H8);
        iss.addReferenceSpecies(CH4);
        iss.addReferenceSpecies(CH4O);

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4);
        iss.removeReferenceSpecies(C2H4O);
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4O);
        iss.removeReferenceSpecies(C2H6);
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6);
        iss.removeReferenceSpecies(C2H6O);
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        //assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6O);
        iss.removeReferenceSpecies(C3H6);
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H6);
        iss.removeReferenceSpecies(C3H8);
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H8);
        iss.removeReferenceSpecies(CH4);
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(CH4);
        iss.removeReferenceSpecies(CH4O);
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }
    
    @Test
    @Ignore
    public void solveDoubleExactTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }

    @Test
    @Ignore
    public void solveDoubleTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, new ISGReactionType(true)), new VariableFactory("v"));
        //iss.addReferenceSpecies(C2H4);
        iss.addReferenceSpecies(C2H4O);
        iss.addReferenceSpecies(C2H6);
        iss.addReferenceSpecies(C2H6O);
        iss.addReferenceSpecies(C3H6);
        iss.addReferenceSpecies(C3H8);
        iss.addReferenceSpecies(CH4);
        iss.addReferenceSpecies(CH4O);

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4);
        iss.removeReferenceSpecies(C2H4O);
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4O);
        iss.removeReferenceSpecies(C2H6);
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6);
        iss.removeReferenceSpecies(C2H6O);
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6O);
        iss.removeReferenceSpecies(C3H6);
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H6);
        iss.removeReferenceSpecies(C3H8);
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H8);
        iss.removeReferenceSpecies(CH4);
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(CH4);
        iss.removeReferenceSpecies(CH4O);
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }

    @Test
    @Ignore
    public void solveIntegerExactExclUnpairedTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(false)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H4O);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6);
        System.out.println(reaction.toString() + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }

    @Test
    @Ignore
    public void solveIntegerExclUnpairedTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(false)), new VariableFactory("v"));
        //iss.addReferenceSpecies(C2H4);
        iss.addReferenceSpecies(C2H4O);
        iss.addReferenceSpecies(C2H6);
        iss.addReferenceSpecies(C2H6O);
        iss.addReferenceSpecies(C3H6);
        iss.addReferenceSpecies(C3H8);
        iss.addReferenceSpecies(CH4);
        iss.addReferenceSpecies(CH4O);

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4);
        iss.removeReferenceSpecies(C2H4O);
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4O);
        iss.removeReferenceSpecies(C2H6);
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6);
        iss.removeReferenceSpecies(C2H6O);
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6O);
        iss.removeReferenceSpecies(C3H6);
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H6);
        iss.removeReferenceSpecies(C3H8);
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H8);
        iss.removeReferenceSpecies(CH4);
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(CH4);
        iss.removeReferenceSpecies(CH4O);
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }
    
    @Test
    @Ignore
    public void solveDoubleExactExclUnpairedTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, new ISGReactionType(false)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }

    @Test
    @Ignore
    public void solveDoubleExclUnpairedTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4 = MockSpecies.getCH4();
        Species CH4O = MockSpecies.getCH4O();
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, new ISGReactionType(false)), new VariableFactory("v"));
        //iss.addReferenceSpecies(C2H4);
        iss.addReferenceSpecies(C2H4O);
        iss.addReferenceSpecies(C2H6);
        iss.addReferenceSpecies(C2H6O);
        iss.addReferenceSpecies(C3H6);
        iss.addReferenceSpecies(C3H8);
        iss.addReferenceSpecies(CH4);
        iss.addReferenceSpecies(CH4O);

        Reaction reaction = iss.solve(C2H4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4);
        iss.removeReferenceSpecies(C2H4O);
        reaction = iss.solve(C2H4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H4O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H4O);
        iss.removeReferenceSpecies(C2H6);
        reaction = iss.solve(C2H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6);
        iss.removeReferenceSpecies(C2H6O);
        reaction = iss.solve(C2H6O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C2H6O.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C2H6O);
        iss.removeReferenceSpecies(C3H6);
        reaction = iss.solve(C3H6);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H6.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H6);
        iss.removeReferenceSpecies(C3H8);
        reaction = iss.solve(C3H8);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(C3H8.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(C3H8);
        iss.removeReferenceSpecies(CH4);
        reaction = iss.solve(CH4);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4.getHf() - reaction.calculateHf()) <= maxErr);
        
        iss.addReferenceSpecies(CH4);
        iss.removeReferenceSpecies(CH4O);
        reaction = iss.solve(CH4O);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        assert (Math.abs(CH4O.getHf() - reaction.calculateHf()) <= maxErr);
    }
}