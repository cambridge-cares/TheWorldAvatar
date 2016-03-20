/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.lpsolve;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.lpsolve.TerminalLPSolveSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.lpsolve.LpSolveFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import java.io.File;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HHDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ISDLpSolveSolverTest {
    
    @Test
    @Ignore
    public void solveTest1() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(false, false);
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
        //System.out.println(reaction + " -> " + reaction.calculateHf());
        
        assert(reaction.calculateHf() == 52.47);
    }
    
    @Test
    @Ignore
    public void solveTest2() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(false, true);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new LpSolveFormat(true, new ISDReactionType()), new VariableFactory("v"));
        //iss.addReferenceSpecies(MockISDSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());
        
        Reaction reaction = iss.solve(C2H4);
        
        String strReaction = "";
        for (Species s : reaction.getReactants().keySet()) {
            strReaction += reaction.getReactants().get(s) + " " + s.getRef() + " ";
        }
        strReaction += "<-> ";
        for (Species s : reaction.getProducts().keySet()) {
            strReaction += reaction.getProducts().get(s) + " " + s.getRef() + " ";
        }
        System.out.println(strReaction);
        //System.out.println(reaction + " -> " + reaction.calculateHf());
        
        assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
    }
    
    @Test
    @Ignore
    public void solveTest2a() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C3H8 = MockSpecies.getC3H8();

        LPSolver solver = new TerminalGLPKSolver(false, true);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        //iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());
        
        Reaction reaction = iss.solve(C3H8);
        
        String strReaction = "";
        for (Species s : reaction.getReactants().keySet()) {
            strReaction += reaction.getReactants().get(s) + " " + s.getRef() + " ";
        }
        strReaction += "<-> ";
        for (Species s : reaction.getProducts().keySet()) {
            strReaction += reaction.getProducts().get(s) + " " + s.getRef() + " ";
        }
        System.out.println(strReaction);
        System.out.println(reaction + " -> " + reaction.calculateHf());
        
        //assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
    }
    
    @Test
    @Ignore
    public void solveTest3() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISDReactionType()), new VariableFactory("v"));
        //iss.addReferenceSpecies(MockISDSpecies.getC2H4());
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        iss.addReferenceSpecies(MockSpecies.getCH4O());
        
        Reaction reaction = iss.solve(C2H4);
        //System.out.println(reaction + " -> " + reaction.calculateHf());
        
        assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
    }
}
