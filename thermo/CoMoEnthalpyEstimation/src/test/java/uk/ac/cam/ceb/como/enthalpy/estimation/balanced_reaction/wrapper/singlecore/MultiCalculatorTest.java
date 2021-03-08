/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import com.cmclinnovations.data.collections.ObjectPool;
import java.io.File;
import java.io.FileNotFoundException;

import java.util.Collection;
import java.util.HashMap;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.RadicalsFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MedianReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.NoFeasibleSolutionException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class MultiCalculatorTest {

    protected int numResults = 25;
    
    @Test
    @Ignore
    public void calculatorTest() throws Exception {
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

        double exact = MockSpecies.getC2H4().getHf();
        for (Species s : list.keySet()) {
            for (int i = 0; i < list.get(s).size(); i++) {
                System.out.println(i + ": " + list.get(s).get(i).calculateHf());
                assert (Math.abs(exact - list.get(s).get(i).calculateHf()) < 10);
            }
        }
    }
    
    @Test
    @Ignore
    public void errorDistributionWithCompleteSetTest() throws FileNotFoundException, Exception, Exception {
        LPSolver solver = new TerminalGLPKSolver(true, true);
        solver.setDirectory(new File("test_data/"));
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/csv/subset_35.csv"));
        CSVParser parserSoi = new SpeciesPoolParser(new File("test_data/csv/issues/pool_hydrocarbons_12595-82-3.csv"));
        //CSVParser parserSoi = new ISDParser(new File("test_data/csv/reduced_subset.csv"));
        parserRef.parse();
        parserSoi.parse();

        Collection<Species> ref = parserRef.getRefSpecies();
        Collection<Species> soi = parserSoi.getRefSpecies();
        
//        Filter f = new RadicalsFilter(0, 1);
//        f.set(soi);
//        f.filter();
//        soi = f.getValidSpecies();

        for (Species s : soi) {
            // exclude this species from the calculation
            try {
            PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, new MPSFormat(false, new ISDReactionType()), getPool(s, ref));
            calc.calculate(s);
            HashMap<Species, ReactionList> list = (HashMap<Species, ReactionList>) calc.get();
            
            MedianReactionSelector selector = new MedianReactionSelector();
            for (Species sp : list.keySet()) {
                try {
                    Reaction r = selector.select(list.get(sp)).get(0);
                    //System.out.println(r.getSpecies().getRef() + ": " + Math.abs(r.getSpecies().getHf() - r.calculateHf()));
                    System.out.println(r.getSpecies().getRef() + " (" + Math.abs(r.getSpecies().getHf() - r.calculateHf()) + ")" + ": " + r.toString());
                } catch (IndexOutOfBoundsException ioobe) {
                    System.out.println(s.getRef() + " - reaction could not be selected");
                }
            }
            } catch (NoFeasibleSolutionException nfse) {
                System.out.println(s.getRef() + " - no feasable solution found");
            }
        }
    }
    
    protected ObjectPool getPool(Species exclude, Collection<Species> ref) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (ref != null && !ref.isEmpty()) {
            for (Species s : ref) {
                if (!exclude.equals(s, true)) {
                    pool.addValidated(s);
                }
            }
        }
        return pool;
    }
}
