/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import java.io.File;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpeciesConnectivity;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LpSolverException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HHDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HODReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class TerminalGLPKSolverIdentificationTest {
    
    @Test
    public void identifyISDTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        
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
    public void identifyHODTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        
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
    public void identifyHDTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        
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
    public void identifyHHDTest() throws Exception, NoFeasibleSolutionException, LpSolverException {
        
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
