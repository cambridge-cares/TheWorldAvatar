/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.lpsolve;

import java.io.File;

import org.junit.Ignore;
import org.junit.Test;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpeciesConnectivity;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;

/**
 *
 * @author pb556
 */
public class ISGLpSolveSolverTest {
    @Test
    @Ignore
    public void solveTest1() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species CH4O = MockSpecies.getCH4O();

        LPSolver solver = new TerminalLPSolveSolver(false, false);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpeciesConnectivity.getCH3());
        iss.addReferenceSpecies(MockSpeciesConnectivity.getOH());
        iss.addReferenceSpecies(MockSpeciesConnectivity.getH2O());
        iss.addReferenceSpecies(MockSpeciesConnectivity.getH());
        
        Reaction reaction = iss.solve(CH4O);
        String strReaction = "";
        for (Species s : reaction.getReactants().keySet()) {
            strReaction += reaction.getReactants().get(s) + " " + s.getRef() + " ";
        }
        strReaction += "<-> ";
        for (Species s : reaction.getProducts().keySet()) {
            strReaction += reaction.getProducts().get(s) + " " + s.getRef() + " ";
        }
        
        //System.out.println(reaction + " -> " + reaction.calculateHf());
        System.out.println(strReaction);
        System.out.println(reaction.calculateHf());
    }
    
    @Test
    @Ignore
    public void solveTest11() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species CH4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(false, false);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        iss.addReferenceSpecies(MockSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        
        //iss.addReferenceSpecies(MockISDSpecies.getCH4O());
                
        Reaction reaction = iss.solve(CH4);

        String strReaction = "";
        for (Species s : reaction.getReactants().keySet()) {
            strReaction += reaction.getReactants().get(s) + " " + s.getRef() + " ";
        }
        strReaction += "<-> ";
        for (Species s : reaction.getProducts().keySet()) {
            strReaction += reaction.getProducts().get(s) + " " + s.getRef() + " ";
        }
        System.out.println(strReaction);
        
        System.out.println(reaction.calculateHf());
    }
    
    @Test
    @Ignore
    public void solveTest2() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
        //iss.addReferenceSpecies(MockISDSpecies.getC2H4());
        //iss.addReferenceSpecies(MockISDSpecies.getC2H4O());
        iss.addReferenceSpecies(MockSpecies.getC2H6());
        iss.addReferenceSpecies(MockSpecies.getC2H6O());
        iss.addReferenceSpecies(MockSpecies.getC3H6());
        iss.addReferenceSpecies(MockSpecies.getC3H8());
        iss.addReferenceSpecies(MockSpecies.getCH4());
        //iss.addReferenceSpecies(MockISDSpecies.getCH4O());
        
        Reaction reaction = iss.solve(C2H4);
        //System.out.println(reaction + " -> " + reaction.calculateHf());
        
        assert (reaction.calculateHf() <= 52.47 + 10 && reaction.calculateHf() >= 52.47 - 10);
    }
    
    @Test
    @Ignore
    public void solveTest3() throws Exception, NoFeasibleSolutionException, LpSolverException {
        Species C2H4 = MockSpecies.getC2H4();

        LPSolver solver = new TerminalLPSolveSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        Solver iss = new Solver(solver, new MPSFormat(true, new ISGReactionType(true)), new VariableFactory("v"));
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
