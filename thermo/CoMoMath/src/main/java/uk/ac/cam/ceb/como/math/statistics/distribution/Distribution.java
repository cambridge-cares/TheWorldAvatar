/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.statistics.distribution;

/**
 *
 * @author pb556
 */
public interface Distribution {
    public double getPDF(double x);
    public double getCDF(double x);
}
