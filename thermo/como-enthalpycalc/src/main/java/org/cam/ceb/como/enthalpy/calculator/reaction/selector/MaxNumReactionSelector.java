/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.HashMap;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class MaxNumReactionSelector extends ReactionSelector {

    private Logger logger = Logger.getLogger(MaxNumReactionSelector.class);

    @Override
    public ReactionList select(ReactionList reactions) {
        HashMap<String, Reaction> strReaction = new HashMap<String, Reaction>();
        HashMap<String, Integer> strCtr = new HashMap<String, Integer>();
        for (Reaction r : reactions) {
            if (strCtr.containsKey(r.toString())) {
                strCtr.put(r.toString(), strCtr.get(r.toString()) + 1);
            } else {
                strCtr.put(r.toString(), 1);
                strReaction.put(r.toString(), r);
            }
        }
        int max = Integer.MIN_VALUE;
        for (String s : strCtr.keySet()) {
            if (max < strCtr.get(s)) {
                max = strCtr.get(s);
            }
        }
        ReactionList rList = new ReactionList();
        for (String s : strCtr.keySet()) {
            if (strCtr.get(s) == max) {
                rList.add(strReaction.get(s));
            }
        }
        return rList;
    }
    
}
