/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.glpk;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.NoFeasibleSolutionException;
import org.cam.ceb.como.enthalpy.calculator.solver.Solver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISGReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ISGTerminalGLPKSolverTest {
    
    protected double maxErr = 50;
    
    @Test
    //@Ignore
    public void solveDoubleAdvancedTest()  throws IOException, NoFeasibleSolutionException, LpSolverException {
        // read species pool
        SpeciesPoolParser parser = new SpeciesPoolParser(new File("test_data/test_set.csv"));
        parser.parse();
        HashSet<Species> allSpecies = (HashSet<Species>) parser.getRefSpecies();
        HashSet<Species> soi = (HashSet<Species>) parser.getSpeciesOfInterest();
        
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
    public void solveDoubleAdvanced_1634_04_4_Test()  throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    public void solveIntegerExactTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    public void solveIntegerTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    public void solveDoubleExactTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    public void solveDoubleTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    //@Ignore
    public void solveIntegerExactExclUnpairedTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    //@Ignore
    public void solveIntegerExclUnpairedTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    //@Ignore
    public void solveDoubleExactExclUnpairedTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
    //@Ignore
    public void solveDoubleExclUnpairedTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
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
