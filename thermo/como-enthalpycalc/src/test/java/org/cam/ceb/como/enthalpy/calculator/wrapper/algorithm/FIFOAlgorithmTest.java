/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm;

import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.MockSpeciesConnectivity;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.MPSFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HHDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HODReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISGReactionType;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.MultiCalculator;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.ObjectPoolCalculator;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class FIFOAlgorithmTest {
    
    // Reverse order
    protected FIFOAlgorithm alg;
    protected int numResults = 25;
    
    public void init(ObjectPool pool) {
        MultiCalculator calculators = new MultiCalculator();
        calculators.add(new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new HHDReactionType()), pool));
        calculators.add(new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new HDReactionType()), pool));
        calculators.add(new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new HODReactionType()), pool));
        calculators.add(new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new ISDReactionType()), pool));
        calculators.add(new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(true, new ISGReactionType(true)), pool));
        alg = new FIFOAlgorithm(calculators);
    }
            
    @Test
    @Ignore
    public void fifoAlgorithmISGTest() throws Exception {
        // identification of isogyric reaction
        
        // create the pool first
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpeciesConnectivity.getC8H10());
        pool.add(MockSpeciesConnectivity.getC4H8());
        pool.add(MockSpeciesConnectivity.getC6H10());
        pool.add(MockSpeciesConnectivity.getC4H4());
        pool.add(MockSpeciesConnectivity.getC5H10());
        
        // initialise the calculators
        init(pool);
        
        ReactionList solution = alg.solve(MockSpeciesConnectivity.getC3H6());
        Map<Species, ObjectPoolCalculator> calculator = alg.getSuccessor();
        for (Species s : calculator.keySet()) {
            MPSFormat format = (MPSFormat) calculator.get(s).getFormat();
            ReactionType rt = format.getReactionType();
            assert(rt instanceof ISGReactionType || 
                    rt instanceof ISDReactionType ||
                    rt instanceof HODReactionType ||
                    rt instanceof HDReactionType ||
                    rt instanceof HHDReactionType);
        }
    }
    
    @Test
    @Ignore
    public void fifoAlgorithmISDTest() throws Exception {
        // create the pool first
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpeciesConnectivity.getC8H10());
        pool.add(MockSpeciesConnectivity.getCH4());
        pool.add(MockSpeciesConnectivity.getC2H6());
        pool.add(MockSpeciesConnectivity.getC2H2());
        
        // initialise the calculators
        init(pool);
        
        ReactionList solution = alg.solve(MockSpeciesConnectivity.getC2H4());
        Map<Species, ObjectPoolCalculator> calculator = alg.getSuccessor();
        for (Species s : calculator.keySet()) {
            MPSFormat format = (MPSFormat) calculator.get(s).getFormat();
            ReactionType rt = format.getReactionType();
            assert(!(rt instanceof ISGReactionType) && 
                    (rt instanceof ISDReactionType ||
                    rt instanceof HODReactionType ||
                    rt instanceof HDReactionType ||
                    rt instanceof HHDReactionType));
        }
    }
    
    @Test
    @Ignore
    public void fifoAlgorithmHODTest() throws Exception {
        // create the pool first
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpeciesConnectivity.getC8H10());
        pool.add(MockSpeciesConnectivity.getC2H6());
        pool.add(MockSpeciesConnectivity.getC4H8());
        pool.add(MockSpeciesConnectivity.getC3H6());
        pool.add(MockSpeciesConnectivity.getC3H4());
        
        // initialise the calculators
        init(pool);
        
        ReactionList solution = alg.solve(MockSpeciesConnectivity.getC2H4());
        Map<Species, ObjectPoolCalculator> calculator = alg.getSuccessor();
        for (Species s : calculator.keySet()) {
            MPSFormat format = (MPSFormat) calculator.get(s).getFormat();
            ReactionType rt = format.getReactionType();
            assert(!(rt instanceof ISGReactionType && 
                    rt instanceof ISDReactionType) &&
                    (rt instanceof HODReactionType ||
                    rt instanceof HDReactionType ||
                    rt instanceof HHDReactionType));
        }
    }
    
    @Test
    @Ignore
    public void fifoAlgorithmHDTest() throws Exception {
        // create the pool first
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpeciesConnectivity.getC8H10());
        pool.add(MockSpeciesConnectivity.getC6H10());
        pool.add(MockSpeciesConnectivity.getC4H4());
        
        // initialise the calculators
        init(pool);
        
        ReactionList solution = alg.solve(MockSpeciesConnectivity.getC2H4());
        Map<Species, ObjectPoolCalculator> calculator = alg.getSuccessor();
        for (Species s : calculator.keySet()) {
            MPSFormat format = (MPSFormat) calculator.get(s).getFormat();
            ReactionType rt = format.getReactionType();
            assert(!(rt instanceof ISGReactionType && 
                    rt instanceof ISDReactionType &&
                    rt instanceof HODReactionType) &&
                    (rt instanceof HDReactionType ||
                    rt instanceof HHDReactionType));
        }
    }
    
    @Test
    @Ignore
    public void fifoAlgorithmHHDTest() throws Exception {
        // create the pool first
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpeciesConnectivity.getC8H10());
        pool.add(MockSpeciesConnectivity.getC4H8());
        pool.add(MockSpeciesConnectivity.getC6H10());
        pool.add(MockSpeciesConnectivity.getC4H4());
        pool.add(MockSpeciesConnectivity.getC5H10());
        
        // initialise the calculators
        init(pool);
        
        ReactionList solution = alg.solve(MockSpeciesConnectivity.getC3H6());
        Map<Species, ObjectPoolCalculator> calculator = alg.getSuccessor();
        for (Species s : calculator.keySet()) {
            MPSFormat format = (MPSFormat) calculator.get(s).getFormat();
            ReactionType rt = format.getReactionType();
            assert(!(rt instanceof ISGReactionType && 
                    rt instanceof ISDReactionType &&
                    rt instanceof HODReactionType &&
                    rt instanceof HDReactionType) &&
                    rt instanceof HHDReactionType);
        }
    }
}
