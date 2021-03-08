/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.sampler;

import uk.ac.cam.ceb.como.math.sampler.SimpleRandomSampler;
import java.util.ArrayList;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class SimpleRandomSamplerTest {
    
    @Test
    public void randomSampleTest() {
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(1);
        l.add(2);
        l.add(3);
        l.add(4);
        l.add(5);
        l.add(6);
        l.add(7);
        l.add(8);
        l.add(9);
        l.add(10);
        l.add(11);
        l.add(12);
        
        SimpleRandomSampler sampler = new SimpleRandomSampler();
        ArrayList<ArrayList<Integer>> subsets = (ArrayList<ArrayList<Integer>>) sampler.randomSample(l, 4);
        for (int i = 0; i < subsets.size(); i++) {
            System.out.println();
            for (int j = 0; j < subsets.get(i).size(); j++) {
                System.out.print(subsets.get(i).get(j) + " ");
            }
        }
    }
}
