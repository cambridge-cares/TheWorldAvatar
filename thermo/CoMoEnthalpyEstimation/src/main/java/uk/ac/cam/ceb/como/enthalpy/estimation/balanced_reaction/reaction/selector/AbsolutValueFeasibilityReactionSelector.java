/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class AbsolutValueFeasibilityReactionSelector extends ReactionSelector {

    // identification of big jump
    // ordering of reactionlist
    // removal of non feasible solutions
    protected double jump = 50;
    
    @Override
    public ReactionList select(ReactionList reactions) {
        for (int i = 0; i < reactions.size(); i++) {
            for (int j = i + 1; j < reactions.size(); j++) {
                if (Math.abs(reactions.get(i).calculateHf()) > Math.abs(reactions.get(j).calculateHf())) {
                    Reaction r = reactions.get(i);
                    reactions.set(i, reactions.get(j));
                    reactions.set(j, r);
                }
            }    
        }
        
        // check for large jumps compared to the variance between the results
        ReactionList valid = new ReactionList();
        for (int i = 0; i < reactions.size() - 1; i++) {
            if (Math.abs(reactions.get(i + 1).calculateHf()) - Math.abs(reactions.get(i).calculateHf()) > jump) {
                break;
            } else {
                valid.add(reactions.get(i));
            }
        }
        
        return valid;
    }
}
