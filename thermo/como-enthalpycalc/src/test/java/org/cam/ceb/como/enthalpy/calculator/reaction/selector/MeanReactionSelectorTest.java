/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.io.pool.SpeciesPoolParser;
import org.cam.ceb.como.enthalpy.calculator.io.reactions.ReactionData;
import org.cam.ceb.como.enthalpy.calculator.io.reactions.ReactionDataInterpreter;
import org.cam.ceb.como.enthalpy.calculator.io.reactions.ReactionDataList;
import org.cam.ceb.como.enthalpy.calculator.io.reactions.ReactionListParser;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class MeanReactionSelectorTest {
    
    @Test
    public void meanReactionSelectorTest() throws FileNotFoundException, IOException {        
        CSVParser parserPool = new SpeciesPoolParser(new File("test_data/analysis/pools/radical-1/pool_230_radicals_1.csv"));
        parserPool.parse();
        
        Collection<Species> allSpecies = parserPool.getAllSpecies();
        
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/isd_subset_25_marking.csv"));
        parserRef.parse();
        
        ReactionListParser rListParser = new ReactionListParser();
        
        rListParser.setPath(new File("test_data/reactions/isd_marking25_10runs_pool230_det_589-34-4.rct"));
        rListParser.parse();
        ReactionDataList rListDetData = (ReactionDataList) rListParser.get();
        
        // convert data into list
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(allSpecies);
        Map<ReactionData, Reaction> dataDet = interpreter.get(rListDetData);

        ReactionList rListDet = new ReactionList();
        for (ReactionData d : dataDet.keySet()) {
            rListDet.add(dataDet.get(d));
        }
        
        //-195.51522016394262
        //-195.51522016394262
        //-196.145506716079
        //-197.21211696646301
        //-198.57274985513212
        //-199.3035255533961
        //-199.707776214974
        //-199.707776214974
        //-201.0632405964975
        //-203.93682379205993
        double err = 0.00001;
        
        MeanReactionSelector selector = new MeanReactionSelector(1);
        ReactionList rList1 = selector.select(rListDet);
        assert(rList1.size() == 1);
        assert(Math.abs(-198.57274985513212 - getAvg(rList1)) < err);
        
        selector = new MeanReactionSelector(2);
        ReactionList rList2 = selector.select(rListDet);
        assert(rList2.size() == 2);
        assert(Math.abs(-198.9381377042641 - getAvg(rList2)) < err);
        
        selector = new MeanReactionSelector(3);
        ReactionList rList3 = selector.select(rListDet);
        assert(rList3.size() == 3);
        assert(Math.abs(-198.36279745833042 - getAvg(rList3)) < err);
        
        selector = new MeanReactionSelector(4);
        ReactionList rList4 = selector.select(rListDet);
        assert(rList4.size() == 4);
        assert(Math.abs(-198.6990421474913 - getAvg(rList4)) < err);
        
        selector = new MeanReactionSelector(5);
        ReactionList rList5 = selector.select(rListDet);
        assert(rList5.size() == 5);
        assert(Math.abs(-198.18833506120885 - getAvg(rList5)) < err);
        
        selector = new MeanReactionSelector(6);
        ReactionList rList6 = selector.select(rListDet);
        assert(rList6.size() == 6);
        assert(Math.abs(-198.44157525350303 - getAvg(rList6)) < err);
        
        selector = new MeanReactionSelector(7);
        ReactionList rList7 = selector.select(rListDet);
        assert(rList7.size() == 7);
        assert(Math.abs(-198.02352452642296 - getAvg(rList7)) < err);
        
        selector = new MeanReactionSelector(8);
        ReactionList rList8 = selector.select(rListDet);
        assert(rList8.size() == 8);
        assert(Math.abs(-198.4034890351823 - getAvg(rList8)) < err);
        
        selector = new MeanReactionSelector(9);
        ReactionList rList9 = selector.select(rListDet);
        assert(rList9.size() == 9);
        assert(Math.abs(-198.08257027171123 - getAvg(rList9)) < err);
        
        selector = new MeanReactionSelector(10);
        ReactionList rList10 = selector.select(rListDet);
        assert(rList10.size() == 10);
        assert(Math.abs(-198.6679956237461 - getAvg(rList10)) < err);
    } 
    
    protected double getAvg(ReactionList rList) {
        double sum = 0;
        for (Reaction r : rList) {
            sum += r.calculateHf();
        }
        return sum / rList.size();
    }
}
