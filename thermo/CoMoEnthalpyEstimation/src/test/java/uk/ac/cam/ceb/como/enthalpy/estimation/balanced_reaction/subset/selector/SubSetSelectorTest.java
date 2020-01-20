/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.subset.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.subset.selector.SubSetSelectorException;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.subset.selector.SubSetSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.subset.selector.SimpleSubSetSelector;
import java.io.File;
import java.io.FileNotFoundException;

import java.util.Collection;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author Philipp
 */
public class SubSetSelectorTest {
    
    @Test
    public void createSubSetTest() throws FileNotFoundException, Exception, SubSetSelectorException {
        CSVParser parser = new SpeciesPoolParser(new File("test_data/analysis/pools/pool_hydrocarbons3.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        
        SubSetSelector selector = new SimpleSubSetSelector(ref);
        
        Collection<Species> sp200 = selector.select(200);
        Collection<Species> sp175 = selector.select(175);
        Collection<Species> sp150 = selector.select(150);
        Collection<Species> sp125 = selector.select(125);
        Collection<Species> sp100 = selector.select(100);
        Collection<Species> sp75 = selector.select(75);
        Collection<Species> sp50 = selector.select(50);
        Collection<Species> sp25 = selector.select(25);
        Collection<Species> sp10 = selector.select(10);
        
        assert(sp200.size() == 200);
        assert(sp175.size() == 175);
        assert(sp150.size() == 150);
        assert(sp125.size() == 125);
        assert(sp100.size() == 100);
        assert(sp75.size() == 75);
        assert(sp50.size() == 50);
        assert(sp25.size() == 25);
        assert(sp10.size() == 10);
    }
    
    @Test
    @Ignore
    public void createSubSetWriteFiles() throws FileNotFoundException, Exception, SubSetSelectorException {
        CSVParser parser = new SpeciesPoolParser(new File("test_data/analysis/pool_hydrocarbons3.csv"));
        parser.parse();

        Collection<Species> ref = parser.getRefSpecies();
        
        SubSetSelector selector = new SimpleSubSetSelector(ref);
        
        Collection<Species> sp200 = selector.select(200);
        Collection<Species> sp175 = selector.select(175);
        Collection<Species> sp150 = selector.select(150);
        Collection<Species> sp125 = selector.select(125);
        Collection<Species> sp100 = selector.select(100);
        Collection<Species> sp75 = selector.select(75);
        Collection<Species> sp50 = selector.select(50);
        Collection<Species> sp25 = selector.select(25);
        Collection<Species> sp10 = selector.select(10);
        
        assert(sp200.size() == 200);
        assert(sp175.size() == 175);
        assert(sp150.size() == 150);
        assert(sp125.size() == 125);
        assert(sp100.size() == 100);
        assert(sp75.size() == 75);
        assert(sp50.size() == 50);
        assert(sp25.size() == 25);
        assert(sp10.size() == 10);
        
        CSVWriter writer = new SpeciesPoolWriter();
        
        writer.set(new File("test_data/analysis/pool_200.csv"));
        writer.set(sp200, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_175.csv"));
        writer.set(sp175, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_150.csv"));
        writer.set(sp150, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_125.csv"));
        writer.set(sp125, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_100.csv"));
        writer.set(sp100, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_75.csv"));
        writer.set(sp75, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_50.csv"));
        writer.set(sp50, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_25.csv"));
        writer.set(sp25, false);
        writer.write();
        
        writer.set(new File("test_data/analysis/pool_10.csv"));
        writer.set(sp10, false);
        writer.write();
    }
}
