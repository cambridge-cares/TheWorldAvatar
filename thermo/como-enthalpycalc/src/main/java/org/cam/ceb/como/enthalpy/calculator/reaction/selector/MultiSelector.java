/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class MultiSelector extends ReactionSelector {

    protected List<ReactionSelector> selectors = new ArrayList<ReactionSelector>();
    private Logger logger = Logger.getLogger(MultiSelector.class);

    public MultiSelector() {
        super();
    }
    
    public MultiSelector(List<ReactionSelector> selectors) {
        super();
        this.selectors = selectors;
    }
    
    public void set(List<ReactionSelector> selectors) {
        this.selectors = selectors;
    }
    
    public boolean add(ReactionSelector selector) {
        return selectors.add(selector);
    }
    
    public void add(int index, ReactionSelector selector) {
        selectors.add(index, selector);
    }
    
    public boolean addAll(ArrayList<ReactionSelector> selectors) {
        return selectors.addAll(selectors);
    }
    
    public boolean addAll(int index, ArrayList<ReactionSelector> selectors) {
        return selectors.addAll(index, selectors);
    }
    
    public boolean remove(ReactionSelector selector) {
        return selectors.remove(selector);
    }
    
    public ReactionSelector remove(int index) {
        return selectors.remove(index);
    }
    
    public boolean removeAll(ArrayList<ReactionSelector> selectors) {
        return selectors.removeAll(selectors);
    }

    @Override
    public ReactionList select(ReactionList reactions) {
        ReactionList selectedReactions = new ReactionList();
        if (selectors == null || selectors.isEmpty()) {
            logger.warn("No selectors are defined!");
        } else {
            selectedReactions.addAll(reactions);
            for (ReactionSelector selector : selectors) {
                ReactionList buffer = new ReactionList();
                ReactionList valid = selector.select(reactions);
                for (int i = 0; i < selectedReactions.size(); i++) {
                    for (int j = 0; j < valid.size(); j++) {
                        if (selectedReactions.get(i).equals(valid.get(j))) {
                            buffer.add(selectedReactions.get(i));
                            break;
                        }
                    }
                }
                selectedReactions = new ReactionList(buffer);
            }
        }
        return selectedReactions;
    }
}
