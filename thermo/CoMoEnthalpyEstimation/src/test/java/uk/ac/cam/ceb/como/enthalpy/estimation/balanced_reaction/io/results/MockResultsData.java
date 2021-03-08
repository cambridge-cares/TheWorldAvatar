/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsData;
import java.util.ArrayList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

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
