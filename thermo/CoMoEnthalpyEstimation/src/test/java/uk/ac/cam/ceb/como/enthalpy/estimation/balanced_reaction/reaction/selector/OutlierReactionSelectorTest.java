/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.MADReactionSelector;
import java.io.File;
import java.io.FileNotFoundException;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.CSVParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionDataInterpreter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionDataList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionListParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class OutlierReactionSelectorTest {
 
    @Test
    public void outlierDetectionTest() throws FileNotFoundException, Exception {
        CSVParser parserPool = new SpeciesPoolParser(new File("test_data/analysis/pools/special/pool_hydrocarbons_1295-82-3.csv"));
        parserPool.parse();
        
        ReactionListParser parser = new ReactionListParser(new File("test_data/analysis/pools/special/outlier_identification_MP_74-84-0_2.csv"));
        parser.parse();
        
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(parserPool.getAllSpecies());
        ReactionDataList list = (ReactionDataList) parser.get();
        ReactionList rList = new ReactionList();
        for (int i = 0; i < list.size(); i++) {
            Reaction r = interpreter.get(list.get(i));
            rList.add(r);
            //System.out.println(r.toString() + " --> " + r.calculateHf());
        }
        
        MADReactionSelector selector = new MADReactionSelector(1.5);
        //OutlierReactionSelector selector = new OutlierReactionSelector();
        ReactionList outliers = selector.select(rList);
        
        for (int i = 0; i < outliers.size(); i++) {
            System.out.println(outliers.get(i).toString() + " --> " + outliers.get(i).calculateHf());
        }
    }
}
