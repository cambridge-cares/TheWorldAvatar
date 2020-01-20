/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;

/**
 *
 * @author pb556
 */

public class ReactionList extends ArrayList<Reaction> {

    public ReactionList() {
    }

    public ReactionList(ArrayList<Reaction> reactions) {
        addAll(reactions);
    }

    /**
     * Remove empty reactions.
     */
    public void trim() {
        for (Iterator<Reaction> it = this.iterator(); it.hasNext();) {
            if (it.next().isEmpty()) {
                it.remove();
            } else {
                break;
            }
        }
    }

    public List<Reaction> get(ReactionSelector selector) {
    	/**
    	 * @author NK510
    	 * Added <Reaction> to ArrayList.
    	 */
        ArrayList<Reaction> list = (ArrayList<Reaction>) selector.select(this);
        this.clear();
        this.addAll(list);
        return this;
    }

    /**
     * 
     * @author nk510 (caresssd@hermes.cam.ac.uk)
     * Collection<Reaction> -> List<Reaction>
     * @return ArrayList of reactions.
     * 
     */
    public List<Reaction> getCollection() {
        ArrayList<Reaction> list = new ArrayList<Reaction>();
        for (Reaction r : this) {
            list.add(r);
        }
        return list;
    }

    public boolean has(Reaction reaction) {
        for (Reaction r : this) {
            if (reaction.equals(r)) {
                return true;
            }
        }
        return false;
    }
}