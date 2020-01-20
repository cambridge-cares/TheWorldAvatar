/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author pb556
 */
public class SimpleRandomSampler implements Sampler {

    @Override
    public Collection randomSample(List c, int m) {
        Collections.shuffle(c);
        ArrayList<ArrayList> subsets = new ArrayList<ArrayList>();
        for (int i = 0; i < m; i++) {
            ArrayList l = new ArrayList();
            //System.out.println(i * c.size()/m);
            for (int j = i * c.size()/m; j < (i + 1) * c.size()/m; j++) {
                l.add(c.get(j));
            }
            subsets.add(l);
        }
        return subsets;
    }

//    @Override
//    public Collection randomSample(List c, int m) {
//        List<List> subsets = new ArrayList<List>();
//        for (int i = 0; i < c.size() / m; i++) {
//            subsets.add(new ArrayList(m));
//        }
//        solve(c, m, 0, subsets);
//        return subsets;
//    }
//
//    private void solve(List a, int k, int i, List<List> subsets) {
//        if (i != a.size()) {
//            // loop over all subsets and try to put a[i] in
//            for (int j = 0; j < subsets.size(); j++) {
//                if (subsets.get(j).size() < k) {
//                    // subset j not full
//                    subsets.get(j).add(a.get(i));
//                    solve(a, k, i + 1, subsets); // do recursion
//                    subsets.get(j).remove(a.get(i));
//
//                    if (subsets.get(j).isEmpty()) {
//                        // don't skip empty subsets, so you won't get duplicates
//                        break;
//                    }
//                }
//            }
//        }
//    }
}
