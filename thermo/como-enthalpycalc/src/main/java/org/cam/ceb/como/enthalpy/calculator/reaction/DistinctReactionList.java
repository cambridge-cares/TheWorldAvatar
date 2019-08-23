/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction;

/**
 *
 * @author pb556
 */
public class DistinctReactionList {

    protected ReactionList list = null;

    public DistinctReactionList(ReactionList list) {
        this.list = list;
    }

    public ReactionList getDistinctList() {
        ReactionList newList = new ReactionList();
        if (list != null) {
            for (Reaction r : list) {
                boolean identified = false;
                for (Reaction r2 : newList) {
                    if (r.equals(r2)) {
                        identified = true;
                        break;
                    }
                }
                if (identified) {
                    continue;
                } else {
                    newList.add(r);
                }
            }
        }
        return newList;
    }
}
