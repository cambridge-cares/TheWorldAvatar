/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction;

import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.ReactionSelector;

/**
 *
 * @author pb556
 */
public class ReactionList extends ArrayList<Reaction> {

    public ReactionList() {
    }

    public ReactionList(List<Reaction> reactions) {
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
        ArrayList list = (ArrayList) selector.select(this);
        this.clear();
        this.addAll(list);
        return this;
    }

    public Collection<Reaction> getCollection() {
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
