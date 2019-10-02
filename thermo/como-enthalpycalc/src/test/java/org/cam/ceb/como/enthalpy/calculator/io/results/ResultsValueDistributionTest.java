/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ResultsValueDistributionTest {

    @Test
    public void getterTest() throws Exception {
        ResultsValueDistribution dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        assert(dist.getMean() == 1.0);
        assert(dist.getMedian() == 1.0);
        
        dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(2.0);
        dist.add(3.0);
        assert(dist.getMean() == 2.0);
        assert(dist.getMedian() == 2.0);
        
        dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(2.0);
        dist.add(3.0);
        dist.add(3.0);
        assert(dist.getMean() == 2.25);
        System.out.println(dist.getMedian());
        assert(dist.getMedian() == 2.5);
    }

    @Test
    public void toStringTest() throws Exception {
        ResultsValueDistribution dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.0);
        assert (dist.toString().compareToIgnoreCase("[1.0, 1.0, 1.0, 1.0, 1.0]") == 0);
        
        dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(2.0);
        dist.add(1.0);
        dist.add(2.0);
        dist.add(1.0);
        assert (dist.toString().compareToIgnoreCase("[1.0, 2.0, 1.0, 2.0, 1.0]") == 0);
        
        dist = new ResultsValueDistribution();
        dist.add(1.0);
        dist.add(1.0);
        dist.add(1.555);
        dist.add(1.0);
        dist.add(1.0);
        assert (dist.toString().compareToIgnoreCase("[1.0, 1.0, 1.555, 1.0, 1.0]") == 0);
    }
}
