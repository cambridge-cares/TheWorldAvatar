/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

/**
 *
 * @author pb556
 */
public class FullScanSampler implements Sampler {

    @Override
    public Collection randomSample(List c, int m) {
        Random rnd = new Random();
        Collection res = new ArrayList(m);
        int visited = 0;
        Iterator it = c.iterator();
        while (m > 0) {
            Object item = it.next();
            if (rnd.nextDouble() < ((double) m) / (c.size() - visited)) {
                res.add(item);
                m--;
            }
            visited++;
        }
        return res;
    }
}
