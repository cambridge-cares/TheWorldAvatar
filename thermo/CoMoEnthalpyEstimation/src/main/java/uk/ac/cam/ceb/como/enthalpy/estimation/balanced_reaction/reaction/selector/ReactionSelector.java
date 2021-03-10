/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author pb556
 */
public abstract class ReactionSelector {
    
    public abstract ReactionList select(ReactionList reactions);
    
    public List<ReactionList> select(List<ReactionList> reactions) {
        ArrayList<ReactionList> selectedReactions = new ArrayList<ReactionList>();
        for (int i = 0; i < reactions.size(); i++) {
            selectedReactions.add(select(reactions.get(i)));
        }
        return selectedReactions;
    }
    
}
