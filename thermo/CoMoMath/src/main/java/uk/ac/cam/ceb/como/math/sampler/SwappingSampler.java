/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import java.util.Collection;
import java.util.List;
import java.util.Random;

/**
 *
 * @author pb556
 */
public class SwappingSampler implements Sampler {

    @Override
    public Collection randomSample(List c, int m) {
        Random rnd = new Random(c.size());
        for (int i = 0; i < m; i++) {
            int pos = i + rnd.nextInt(c.size() - i);
            Object tmp = c.get(pos);
            c.set(pos, c.get(i));
            c.set(i, tmp);
        }
        return c.subList(0, m);
    }
}
