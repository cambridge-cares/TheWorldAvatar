/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions;

import java.io.File;
import java.io.FileNotFoundException;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionListParserTest {
    
    @Test
    public void reactionListParserTest1() throws FileNotFoundException, Exception {
        // read file
        File file = new File("test_data/reactions/test_reactions1.rct");
        ReactionListParser parser = new ReactionListParser(file);
        
        // parse file
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        
        // compare results
        for (ReactionData r : data) {
            assert(MockReactionData.getC2H4().equals(r)
                    || MockReactionData.getC2H4Modified().equals(r)
                    || MockReactionData.getC2H4O().equals(r)
                    || MockReactionData.getC2H6O().equals(r)
                    || MockReactionData.getC3H6().equals(r)
                    || MockReactionData.getCH4().equals(r)
                    || MockReactionData.getCH4O().equals(r));
        }
    }
    
    @Test
    public void reactionListParserTest2() throws FileNotFoundException, Exception {
        // read file
        File file = new File("test_data/reactions/test_reactions2.rct");
        ReactionListParser parser = new ReactionListParser(file);
        
        // parse file
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        
        // compare results
        for (ReactionData r : data) {
            assert(MockReactionData.getC2H4().equals(r)
                    || MockReactionData.getC2H4Modified().equals(r));
        }
    }
    
    @Test
    public void reactionListParserTest3() throws FileNotFoundException, Exception {
        // read file
        File file = new File("test_data/reactions/test_reactions3.rct");
        ReactionListParser parser = new ReactionListParser(file);
        
        // parse file
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        
        // compare results
        for (ReactionData r : data) {
            assert(MockReactionData.getC2H4().equals(r)
                    || MockReactionData.getC2H4Modified().equals(r)
                    || MockReactionData.getC2H4O().equals(r)
                    || MockReactionData.getC2H6O().equals(r)
                    || MockReactionData.getC3H6().equals(r)
                    || MockReactionData.getCH4().equals(r)
                    || MockReactionData.getCH4O().equals(r));
        }
    }
}
