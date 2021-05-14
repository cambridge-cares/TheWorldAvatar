/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.subset.selector;

import java.util.Collection;

/**
 *
 * @author Philipp
 */
public abstract class SubSetSelector {
    
    protected Collection collection;
    
    public SubSetSelector(Collection collection) {
        this.collection = collection;
    }
    
    public abstract Collection select(int num) throws SubSetSelectorException;
}
