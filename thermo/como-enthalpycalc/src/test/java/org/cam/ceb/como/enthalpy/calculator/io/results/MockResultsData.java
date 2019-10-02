/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import java.util.ArrayList;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class MockResultsData {
    
    // create mock results data using real mock reactions
    public static ResultsData getC2H4ResultsData(String id, String comment, int numDist) throws Exception {
        if (numDist <= 0) {
            throw new Exception ("An invalid number of distributions has been defined!");
        }
        ReactionList rList = new ReactionList();
        rList.add(RealMockReactionsC2H4.getSolution1());
        rList.add(RealMockReactionsC2H4.getSolution2());
        rList.add(RealMockReactionsC2H4.getSolution3());
        rList.add(RealMockReactionsC2H4.getSolution4());
        rList.add(RealMockReactionsC2H4.getSolution5());
        ArrayList<ReactionList> dist = new ArrayList<ReactionList>();
        for (int i = 0; i < numDist; i++) {
            dist.add(rList);
        }        
        return new ResultsData(id, MockSpecies.getC2H4(), dist, comment);
    }
}
