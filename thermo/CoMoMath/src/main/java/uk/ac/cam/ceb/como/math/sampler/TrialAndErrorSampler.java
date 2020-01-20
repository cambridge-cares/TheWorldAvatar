/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

/**
 *
 * @author pb556
 */
public class TrialAndErrorSampler implements Sampler {

    @Override
    public Collection randomSample(List c, int m) {
        Random rnd = new Random();
        Collection res = new HashSet();
        int n = c.size();
        if (m > n / 2) { // The optimization
            Collection negativeSet = randomSample(c, n - m);
            for (Object item : c) {
                if (!negativeSet.contains(item)) {
                    res.add(item);
                }
            }
        } else { // The main loop
            while (res.size() < m) {
                int randPos = rnd.nextInt(n);
                res.add(c.get(randPos));
            }
        }
        return res;
    }
}
