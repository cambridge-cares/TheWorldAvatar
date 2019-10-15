/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.glpk;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import org.cam.ceb.como.enthalpy.calculator.MockSpeciesConnectivity;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.NoFeasibleSolutionException;
import org.cam.ceb.como.enthalpy.calculator.solver.Solver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HHDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HODReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISGReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class TerminalGLPKSolverIdentificationTest {
    
    @Test
    public void identifyISDTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        
        Collection<Species> refSpecies = new HashSet<Species>();
        refSpecies.add(MockSpeciesConnectivity.getC8H10());
        refSpecies.add(MockSpeciesConnectivity.getCH4());
        refSpecies.add(MockSpeciesConnectivity.getC2H6());
        refSpecies.add(MockSpeciesConnectivity.getC2H2());
        
        assert(identifyIntegerReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(!identifyIntegerReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(!identifyIntegerReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(identifyDoubleReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(!identifyDoubleReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(!identifyDoubleReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
    }
    
    @Test
    public void identifyHODTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        
        Collection<Species> refSpecies = new HashSet<Species>();
        refSpecies.add(MockSpeciesConnectivity.getC8H10());
        refSpecies.add(MockSpeciesConnectivity.getC2H6());
        refSpecies.add(MockSpeciesConnectivity.getIsoC4H8());
        refSpecies.add(MockSpeciesConnectivity.getC3H6());
        refSpecies.add(MockSpeciesConnectivity.getC3H4());
        
        assert(identifyIntegerReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(identifyIntegerReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(!identifyIntegerReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(identifyDoubleReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(identifyDoubleReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(!identifyDoubleReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
    }
    
    @Test
    public void identifyHDTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        
        Collection<Species> refSpecies = new HashSet<Species>();
        refSpecies.add(MockSpeciesConnectivity.getC8H10());
        refSpecies.add(MockSpeciesConnectivity.getC6H10());
        refSpecies.add(MockSpeciesConnectivity.getC4H4());
        
        assert(identifyIntegerReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(!identifyIntegerReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(identifyDoubleReaction(new HDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new HODReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC2H4(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC2H4(), refSpecies));
        
        assert(!identifyDoubleReaction(new HHDReactionType(), MockSpeciesConnectivity.getC2H4(), refSpecies));
    }
    
    @Test
    @Ignore
    public void identifyHHDTest() throws IOException, NoFeasibleSolutionException, LpSolverException {
        
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        
        Collection<Species> refSpecies = new HashSet<Species>();
        refSpecies.add(MockSpeciesConnectivity.getC8H10());
        refSpecies.add(MockSpeciesConnectivity.getC4H8());
        refSpecies.add(MockSpeciesConnectivity.getC6H10());
        refSpecies.add(MockSpeciesConnectivity.getC4H4());
        refSpecies.add(MockSpeciesConnectivity.getC5H8());
        
        assert(identifyIntegerReaction(new HHDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyIntegerReaction(new HDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyIntegerReaction(new HODReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyIntegerReaction(new ISDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyIntegerReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC3H6(), refSpecies));
        
        assert(identifyDoubleReaction(new HHDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyDoubleReaction(new HDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyDoubleReaction(new HODReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyDoubleReaction(new ISDReactionType(), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(true), MockSpeciesConnectivity.getC3H6(), refSpecies));
        assert(identifyDoubleReaction(new ISGReactionType(false), MockSpeciesConnectivity.getC3H6(), refSpecies));
    }
    
    protected boolean identifyIntegerReaction(ReactionType rt, Species targetSpecies, Collection<Species> refSpecies) {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, rt), new VariableFactory("v"));
        for (Species s : refSpecies) {
            iss.addReferenceSpecies(s);
        }

        Reaction reaction;
        try {
            reaction = iss.solve(targetSpecies);
        } catch (NoFeasibleSolutionException ex) {
            return false;
        } catch (LpSolverException ex) {
            return false;
        }
        
        System.out.println(reaction.toString());
        return true;
    }
    
    protected boolean identifyDoubleReaction(ReactionType rt, Species targetSpecies, Collection<Species> refSpecies) {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        Solver iss = new Solver(solver, new MPSFormat(false, rt), new VariableFactory("v"));
        for (Species s : refSpecies) {
            iss.addReferenceSpecies(s);
        }

        Reaction reaction;
        try {
            reaction = iss.solve(targetSpecies);
        } catch (NoFeasibleSolutionException ex) {
            return false;
        } catch (LpSolverException ex) {
            return false;
        }
        
        System.out.println(reaction.toString());
        return true;
    }
}
