/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.glpk;

import java.io.File;
import java.io.IOException;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.NoFeasibleSolutionException;
import org.cam.ceb.como.enthalpy.calculator.solver.Solver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ISDTerminalGLPKSolverTest {
    
    protected double maxErr = 15;
    
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

        Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
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

        Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
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

        Solver iss = new Solver(solver, new MPSFormat(false, new ISDReactionType()), new VariableFactory("v"));
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

        Solver iss = new Solver(solver, new MPSFormat(false, new ISDReactionType()), new VariableFactory("v"));
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
    
    
    
    
//    @Test
//    public void solveTest1() throws IOException, NoFeasibleSolutionException, LpSolverException {
//        Species C2H4 = MockSpecies.getC2H4();
//
////        HashSet<ISDSpecies> set = new HashSet<ISDSpecies>();
////        set.add(MockISDSpecies.getC2H4());
////        set.add(MockISDSpecies.getC2H4O());
////        set.add(MockISDSpecies.getC2H6());
////        set.add(MockISDSpecies.getC2H6O());
////        set.add(MockISDSpecies.getC3H6());
////        set.add(MockISDSpecies.getC3H8());
////        set.add(MockISDSpecies.getCH4());
////        set.add(MockISDSpecies.getCH4O());
//
//        LPSolver solver = new TerminalGLPKSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//
//        Solver iss = new Solver(solver, new MPSFormat(false, new ISDReactionType()), new VariableFactory("v"));
//        iss.addReferenceSpecies(MockSpecies.getC2H4());
//        iss.addReferenceSpecies(MockSpecies.getC2H4O());
//        iss.addReferenceSpecies(MockSpecies.getC2H6());
//        iss.addReferenceSpecies(MockSpecies.getC2H6O());
//        iss.addReferenceSpecies(MockSpecies.getC3H6());
//        iss.addReferenceSpecies(MockSpecies.getC3H8());
//        iss.addReferenceSpecies(MockSpecies.getCH4());
//        iss.addReferenceSpecies(MockSpecies.getCH4O());
//
//        Reaction reaction = iss.solve(C2H4);
//        //System.out.println(reaction + " -> " + reaction.calculateHf());
//
//        assert (reaction.calculateHf() == 52.47);
//    }
//
//    @Test
//    public void solveTest2() throws IOException, NoFeasibleSolutionException, LpSolverException {
//        Species C2H4 = MockSpecies.getC2H4();
//
//        LPSolver solver = new TerminalGLPKSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
//        //iss.addReferenceSpecies(MockISDSpecies.getC2H4());
//        //iss.addReferenceSpecies(MockISDSpecies.getC3H6());
//        
//        
//        iss.addReferenceSpecies(MockSpecies.getCH4O());
//        iss.addReferenceSpecies(MockSpecies.getC2H6O());
//        //iss.addReferenceSpecies(MockISDSpecies.getCH4());
//        //iss.addReferenceSpecies(MockISDSpecies.getC2H6());
//        iss.addReferenceSpecies(MockSpecies.getC3H8());
//        iss.addReferenceSpecies(MockSpecies.getC2H4O());
//
//        Reaction reaction = iss.solve(C2H4);
//        //System.out.println(reaction + " -> " + reaction.calculateHf());
//        
//        String strReaction = "";
//        for (Species s : reaction.getReactants().keySet()) {
//            strReaction += reaction.getReactants().get(s) + " " + s.getRef() + " ";
//        }
//        strReaction += "<-> ";
//        for (Species s : reaction.getProducts().keySet()) {
//            strReaction += reaction.getProducts().get(s) + " " + s.getRef() + " ";
//        }
//        System.out.println(strReaction);
//
//        //assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
//    }
//
//    @Test
//    public void solveTest3() throws IOException, NoFeasibleSolutionException, LpSolverException {
//        Species C2H4 = MockSpecies.getC2H4();
//
//        LPSolver solver = new TerminalGLPKSolver(true, true);
//        solver.setDirectory(new File("test_data/"));
//        Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
//        //iss.addReferenceSpecies(MockISDSpecies.getC2H4());
//        iss.addReferenceSpecies(MockSpecies.getC2H4O());
//        iss.addReferenceSpecies(MockSpecies.getC2H6());
//        iss.addReferenceSpecies(MockSpecies.getC2H6O());
//        iss.addReferenceSpecies(MockSpecies.getC3H6());
//        iss.addReferenceSpecies(MockSpecies.getC3H8());
//        iss.addReferenceSpecies(MockSpecies.getCH4());
//        iss.addReferenceSpecies(MockSpecies.getCH4O());
//
//        Reaction reaction = iss.solve(C2H4);
//        //System.out.println(reaction + " -> " + reaction.calculateHf());
//
//        assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
//    }
//
//    @Test
//    public void outputParserTest() throws LpSolverException {
//        TerminalGLPKSolver solver = new TerminalGLPKSolver(true, false);
//        Map<String, Number> data = solver.parseLpSolveOutput(null, new File("test_data/glpk/output/simple_test.out"));
//        assert (data.get("absV[0]").intValue() == 1);
//        assert (data.get("absV[1]").intValue() == 1);
//        assert (data.get("absV[2]").intValue() == 1);
//        assert (data.get("absV[3]").intValue() == 1);
//        assert (data.get("v[0]").intValue() == 1);
//        assert (data.get("v[1]").intValue() == 1);
//        assert (data.get("v[2]").intValue() == -1);
//        assert (data.get("v[3]").intValue() == -1);
//    }
}
