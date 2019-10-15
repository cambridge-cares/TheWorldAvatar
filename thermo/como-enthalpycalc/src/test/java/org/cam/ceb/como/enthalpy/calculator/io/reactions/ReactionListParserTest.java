/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionListParserTest {
    
    @Test
    public void reactionListParserTest1() throws FileNotFoundException, IOException {
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
    public void reactionListParserTest2() throws FileNotFoundException, IOException {
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
    public void reactionListParserTest3() throws FileNotFoundException, IOException {
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
