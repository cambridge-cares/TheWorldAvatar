/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.ArrayList;
import java.util.List;

import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

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
