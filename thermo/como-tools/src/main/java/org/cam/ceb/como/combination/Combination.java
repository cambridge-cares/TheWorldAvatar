/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.combination;

import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class Combination {

    public static ArrayList<ArrayList<Object>> compute(ArrayList<Object> restOfVals) {
        if (restOfVals.size() < 2) {
            ArrayList<ArrayList<Object>> c = new ArrayList<ArrayList<Object>>();
            c.add(restOfVals);
            return c;
        } else {
            ArrayList<ArrayList<Object>> newList = new ArrayList<ArrayList<Object>>();
            for (Object o : restOfVals) {
//make a copy of the array
                ArrayList<Object> rest = new ArrayList<Object>(restOfVals);
//remove the object
                rest.remove(o);
                newList.addAll(prependToEach(o, compute(rest)));
            }
            return newList;
        }

    }

    private static ArrayList<ArrayList<Object>> prependToEach(Object v, ArrayList<ArrayList<Object>> vals) {
        for (ArrayList<Object> o : vals) {
            o.add(0, v);
        }
        return vals;
    }
}
