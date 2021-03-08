/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
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
 * @author Philipp
 */
public class RemoveFlaggedReactionsTest {

    @Test
    public void checkRemovedFlaggedReactionsTest() throws FileNotFoundException, IOException {
        CSVParser parserPool = new SpeciesPoolParser(new File("test_data/analysis/pools/radical-1/pool_230_radicals_1.csv"));
        parserPool.parse();
        
        Collection<Species> allSpecies = parserPool.getAllSpecies();
        
        CSVParser parserRef = new SpeciesPoolParser(new File("test_data/isd_subset_25_marking.csv"));
        parserRef.parse();
        
        ReactionListParser rListParser = new ReactionListParser();
        
        rListParser.setPath(new File("test_data/reactions/isd_marking25_10runs_pool230_sol.rct"));
        rListParser.parse();
        ReactionDataList rListSolData = (ReactionDataList) rListParser.get();
        
        rListParser.setPath(new File("test_data/reactions/isd_marking25_10runs_pool230_det.rct"));
        rListParser.parse();
        ReactionDataList rListDetData = (ReactionDataList) rListParser.get();
        
        // convert data into list
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(allSpecies);
        Map<ReactionData, Reaction> dataSol = interpreter.get(rListSolData);
        Map<ReactionData, Reaction> dataDet = interpreter.get(rListDetData);
        
        ReactionList rListSol = new ReactionList();
        ReactionList rListDet = new ReactionList();
        
        for (ReactionData d : dataSol.keySet()) {
            rListSol.add(dataSol.get(d));
        }
        for (ReactionData d : dataDet.keySet()) {
            rListDet.add(dataDet.get(d));
        }

        ArrayList<String> flag = new ArrayList<String>();
        flag.add("2");
        Collection<Species> refMarked = parserRef.getRefSpecies(flag);
        
        for (Reaction r : rListSol) {
            // check if rListSol has any flagged species
            if (hasFlaggedSpecies(r, refMarked)) {
                // if so, check if a reaction would have been available without flagged species
                ReactionList rList = getDetReactionList(rListDet, r.getSpecies());
                boolean identified = false;
                for (Reaction rDet : rList) {
                    if (!hasFlaggedSpecies(rDet, refMarked)) {
                        identified = true;
                        break;
                    }
                }
                if (identified) {
                    assert(false);
                }
            }
        }
    }
    
    protected boolean hasFlaggedSpecies(Reaction r, Collection<Species> markedSpecies) {
        boolean containsFlaggedSpecies = true;
        containsFlaggedSpecies &= hasFlaggedSpecies(r.getReactants().keySet(), markedSpecies);
        containsFlaggedSpecies &= hasFlaggedSpecies(r.getProducts().keySet(), markedSpecies);
        return containsFlaggedSpecies;
    }
    
    protected boolean hasFlaggedSpecies(Collection<Species> species, Collection<Species> markedSpecies) {
        for (Species s : species) {
            for (Species m : markedSpecies) {
                if (s.equals(s, true)) {
                    return true;
                }
            }
        }
        return false;
    }
    
    protected ReactionList getDetReactionList(ReactionList det, Species s) {
        ReactionList rList = new ReactionList();
        for (Reaction r : det) {
            if (r.getSpecies().equals(s, true)) {
                rList.add(r);
            }
        }
        return rList;
    }
    
    @Test
    public void removeFlaggedReactionsTest() throws FileNotFoundException, IOException, Exception {
        Species sC2H4 = MockSpecies.getC2H4();
        Species sC2H4O = MockSpecies.getC2H4O();
        Species sC2H6 = MockSpecies.getC2H6();
        Species sC2H6O = MockSpecies.getC2H6O();
        Species sC3H6 = MockSpecies.getC3H6();
        Species sC3H8 = MockSpecies.getC3H8();
        Species sCH4 = MockSpecies.getCH4();
        Species sCH4O = MockSpecies.getCH4O();

        // create fake reactions
        Reaction r1 = new Reaction(sCH4);
        r1.addReactant(sC2H6, 1);
        r1.addProduct(sC2H4, 1);
        r1.addProduct(sC2H4O, 1);
        
        Reaction r2 = new Reaction(sCH4);
        r2.addReactant(sC3H8, 1);
        r2.addProduct(sC3H6, 1);
        r2.addProduct(sC2H6O, 1);
        
        Reaction r3 = new Reaction(sCH4);
        r3.addReactant(sC2H6, 1);
        r3.addProduct(sC3H6, 1);
        r3.addProduct(sC3H8, 1);
        
        Reaction r4 = new Reaction(sCH4);
        r4.addReactant(sC2H4O, 1);
        r4.addProduct(sC3H8, 1);
        r4.addProduct(sCH4O, 1);
        
        Reaction r5 = new Reaction(sCH4);
        r5.addReactant(sC2H6O, 1);
        r5.addProduct(sC3H6, 1);
        r5.addProduct(sCH4O, 1);
        
        ReactionList rList = new ReactionList();
        rList.add(r1);
        rList.add(r2);
        rList.add(r3);
        rList.add(r4);
        rList.add(r5);
        
        HashMap<Species, String> flags = new HashMap<Species, String>();
        flags.put(sC2H4, "1");
        flags.put(sC2H4O, "1");
        flags.put(sC2H6, "2");
        flags.put(sC2H6O, "1");
        flags.put(sC3H6, "1");
        flags.put(sC3H8, "1");
        flags.put(sCH4, "1");
        flags.put(sCH4O, "1");        
        
        RemoveFlaggedReactions selector = new RemoveFlaggedReactions(flags, "2");
        ReactionList rListRed = selector.select(rList);
        
        assert(rList.size() == 5);
        assert(rListRed.size() == 3);
        assert(rList.contains(r2));
        assert(rList.contains(r4));
        assert(rList.contains(r5));
    }
}
