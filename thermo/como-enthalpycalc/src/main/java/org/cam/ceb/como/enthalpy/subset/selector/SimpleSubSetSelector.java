/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.cam.ceb.como.enthalpy.subset.selector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Philipp
 */
public class SimpleSubSetSelector extends SubSetSelector {

    public SimpleSubSetSelector(Collection collection) {
        super(collection);
    }

    @Override
    public Collection select(int num) throws SubSetSelectorException {
        if (collection == null || collection.isEmpty()) {
            throw new SubSetSelectorException("Invalid parent set defined!");
        }
        if (num > collection.size() || num < 0) {
            throw new SubSetSelectorException("Invalid subset size defined!");
        }
        ArrayList list = new ArrayList(collection);
        Collections.shuffle(list);
        return new ArrayList(list.subList(0, num));
    }
}
