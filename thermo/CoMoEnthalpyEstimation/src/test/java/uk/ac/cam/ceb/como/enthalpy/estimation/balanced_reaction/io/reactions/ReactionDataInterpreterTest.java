/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions;

import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionData;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions.ReactionDataInterpreter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ReactionDataInterpreterTest {
    
    @Test
    public void reactionDataInterpreterTest() {
        ReactionDataInterpreter interpreter = new ReactionDataInterpreter(MockSpecies.getPool());
        Map<ReactionData, Reaction> convData = interpreter.get(MockReactionData.getCompleteList());
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
    }   
}
