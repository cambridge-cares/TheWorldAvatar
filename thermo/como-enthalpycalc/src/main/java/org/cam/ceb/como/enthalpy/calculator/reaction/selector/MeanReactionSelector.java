/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class MeanReactionSelector extends ReactionSelector {

    protected int num;
    
    public MeanReactionSelector(int num) {
        super();
        this.num = num;
    }
    
    @Override
    public ReactionList select(ReactionList reactions) {
        double[] val = new double[reactions.size()];
        Reaction[] r = new Reaction[reactions.size()];
        for (int i = 0; i < reactions.size(); i++) {
            val[i] = reactions.get(i).calculateHf();
            r[i] = reactions.get(i);
        }
        // sort it
        for (int i = 0; i < val.length; i++) {
            for (int j = i + 1; j < val.length; j++) {
                if (val[i] > val[j]) {
                    // swap
                    double m = val[i];
                    Reaction rM = r[i];
                    val[i] = val[j];
                    r[i] = r[j];
                    val[j] = m;
                    r[j] = rM;
                }
            }
        }
        
//        for (int i = 0; i < r.length; i++) {
//            System.out.println(r[i].calculateHf());
//        }
        
        int start = (int) reactions.size() / 2 - (int) num / 2; 
        
        ReactionList rList = new ReactionList();
        for (int i = start; i < start + num; i++) {
            rList.add(r[i]);
        }
        
        // get median (sort off)
        return rList;
    }
}
