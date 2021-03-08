/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionListWriterTest {

    @Test
    public void writeISDTest1() throws FileNotFoundException, IOException {
        // create some mock reactions
        ReactionList rList = MockReactions.getCompleteList();

        File file = new File("test_data/test_isdcsvwriter_reactions.csv");
        ReactionListWriter writer = new ReactionListWriter(file);
        writer.set(rList);
        writer.overwrite(true);
        writer.write();

        // read the file and check the lines
        ReactionListParser parser = new ReactionListParser(file);
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        assert (data.size() == rList.size());

        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(MockSpecies.getPool());
        Map<ReactionData, Reaction> convData = interpreter.get(data);

        for (ReactionData d : convData.keySet()) {
            Reaction r = convData.get(d);
            assert (r.equals(MockReactions.getReactionC2H4())
                    || r.equals(MockReactions.getReactionC2H4O())
                    || r.equals(MockReactions.getReactionC2H6O())
                    || r.equals(MockReactions.getReactionC3H6())
                    || r.equals(MockReactions.getReactionCH4())
                    || r.equals(MockReactions.getReactionCH4O())
                    || r.equals(MockReactions.getReactionC2H4Modified()));
        }

        file.delete();
    }
    
    @Test
    public void writeISDTest2() throws FileNotFoundException, IOException {
        // create some mock reactions
        ReactionList rList = MockReactions.getCompleteList();
        rList.add(MockReactions.getReactionC2H4Modified());

        File file = new File("test_data/test_isdcsvwriter_reactions.csv");
        ReactionListWriter writer = new ReactionListWriter(file);
        writer.set(rList);
        writer.overwrite(true);
        writer.write();

        // read the file and check the lines
        ReactionListParser parser = new ReactionListParser(file);
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        assert (data.size() == rList.size());

        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(MockSpecies.getPool());
        Map<ReactionData, Reaction> convData = interpreter.get(data);

        for (ReactionData d : convData.keySet()) {
            Reaction r = convData.get(d);
            assert (r.equals(MockReactions.getReactionC2H4())
                    || r.equals(MockReactions.getReactionC2H4O())
                    || r.equals(MockReactions.getReactionC2H6O())
                    || r.equals(MockReactions.getReactionC3H6())
                    || r.equals(MockReactions.getReactionCH4())
                    || r.equals(MockReactions.getReactionCH4O())
                    || r.equals(MockReactions.getReactionC2H4Modified()));
        }

        file.delete();
    }
    
    @Test
    public void writeISGTest1() throws FileNotFoundException, IOException {
        // create some mock reactions
        ReactionList rList = MockReactions.getCompleteList();

        File file = new File("test_data/test_isdcsvwriter_reactions.csv");
        ReactionListWriter writer = new ReactionListWriter(file);
        writer.set(rList);
        writer.overwrite(true);
        writer.write();

        // read the file and check the lines
        ReactionListParser parser = new ReactionListParser(file);
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        assert (data.size() == rList.size());

        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(MockSpecies.getPool());
        Map<ReactionData, Reaction> convData = interpreter.get(data);

        for (ReactionData d : convData.keySet()) {
            Reaction r = convData.get(d);
            assert (r.equals(MockReactions.getReactionC2H4())
                    || r.equals(MockReactions.getReactionC2H4O())
                    || r.equals(MockReactions.getReactionC2H6O())
                    || r.equals(MockReactions.getReactionC3H6())
                    || r.equals(MockReactions.getReactionCH4())
                    || r.equals(MockReactions.getReactionCH4O())
                    || r.equals(MockReactions.getReactionC2H4Modified()));
        }

        file.delete();
    }
    
    @Test
    public void writeISGTest2() throws FileNotFoundException, IOException {
        // create some mock reactions
        ReactionList rList = MockReactions.getCompleteList();
        rList.add(MockReactions.getReactionC2H4Modified());

        File file = new File("test_data/test_isdcsvwriter_reactions.csv");
        ReactionListWriter writer = new ReactionListWriter(file);
        writer.set(rList);
        writer.overwrite(true);
        writer.write();

        // read the file and check the lines
        ReactionListParser parser = new ReactionListParser(file);
        parser.parse();
        ReactionDataList data = (ReactionDataList) parser.get();
        assert (data.size() == rList.size());

        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(MockSpecies.getPool());
        Map<ReactionData, Reaction> convData = interpreter.get(data);

        for (ReactionData d : convData.keySet()) {
            Reaction r = convData.get(d);
            assert (r.equals(MockReactions.getReactionC2H4())
                    || r.equals(MockReactions.getReactionC2H4O())
                    || r.equals(MockReactions.getReactionC2H6O())
                    || r.equals(MockReactions.getReactionC3H6())
                    || r.equals(MockReactions.getReactionCH4())
                    || r.equals(MockReactions.getReactionCH4O())
                    || r.equals(MockReactions.getReactionC2H4Modified()));
        }

        file.delete();
    }

//    @Test
//    public void appendTest() {
//    }
}
