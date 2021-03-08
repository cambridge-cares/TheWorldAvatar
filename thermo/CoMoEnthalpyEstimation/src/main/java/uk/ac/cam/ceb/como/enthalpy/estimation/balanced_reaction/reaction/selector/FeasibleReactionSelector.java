/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class FeasibleReactionSelector extends ReactionSelector {

    protected double min = Double.MIN_VALUE;
    protected double max = Double.MAX_VALUE;
    
    public FeasibleReactionSelector() {
        super();
    }
    
    public FeasibleReactionSelector(double min, double max) {
        super();
        this.min = min;
        this.max = max;
    }
    
    public void setValidRange(double min, double max) {
        this.min = min;
        this.max = max;
    }
    
    @Override
    public ReactionList select(ReactionList reactions) {
        ReactionList rList = new ReactionList();
        for (int i = 0; i < reactions.size(); i++) {
            double hf = reactions.get(i).calculateHf();
            System.out.println(hf + "");
            if (hf >= min && hf <= max) {
                rList.add(reactions.get(i));
            }
        }
        return rList;
    }
    
}
