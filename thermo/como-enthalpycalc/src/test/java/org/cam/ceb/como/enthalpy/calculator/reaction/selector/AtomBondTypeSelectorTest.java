/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.io.File;
import java.util.HashMap;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.MPSFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.ISDReactionType;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class AtomBondTypeSelectorTest {
    
    protected int numResults = 25;
    
    @Test
    @Ignore
    public void atomBondTypeSelectorTest1() throws Exception {
        
        Reaction r = new Reaction(MockSpecies.getC2H4());
        r.addProduct(MockSpecies.getC2H4O(), 1);
        r.addProduct(MockSpecies.getCH4(), 1);
        r.addReactant(MockSpecies.getCH4O(), 1);
        
        ReactionList rList = new ReactionList();
        rList.add(r);
        
        AtomBondTypeSelector selector = new AtomBondTypeSelector(2, true);
        ReactionList newRList = selector.select(rList);
        
        assert(newRList.size() == 1);
    }
    
    @Test
    @Ignore
    public void atomBondTypeSelectorTest2() throws Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));

        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.add(MockSpecies.getC2H4O());
        pool.add(MockSpecies.getC2H6());
        pool.add(MockSpecies.getC2H6O());
        pool.add(MockSpecies.getC3H6());
        pool.add(MockSpecies.getC3H8());
        pool.add(MockSpecies.getCH4());
        pool.add(MockSpecies.getCH4O());

        PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, new MPSFormat(true, new ISDReactionType()), pool);
        calc.calculate(MockSpecies.getC2H4());
        HashMap<Species, ReactionList> list = (HashMap<Species, ReactionList>) calc.get();

        AtomBondTypeSelector selector = new AtomBondTypeSelector(1, true);
        for (Species s : list.keySet()) {
            ReactionList selected = selector.select(list.get(s));
            System.out.println(selected.size());
        }
        
//        double exact = MockISDSpecies.getC2H4().getHf();
//        for (Species s : list.keySet()) {
//            for (int i = 0; i < list.get(s).size(); i++) {
//                System.out.println(i + ": " + list.get(s).get(i).calculateHf());
//                assert (Math.abs(exact - list.get(s).get(i).calculateHf()) < 10);
//            }
//        }
    }
}
