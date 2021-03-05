/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.variability;

/**
 *
 * @author pb556
 */
public abstract class Variability {

    public abstract double calculate(int[] values) throws Exception;

    public abstract double calculate(long[] values) throws Exception;

    public abstract double calculate(float[] values) throws Exception;

    public abstract double calculate(double[] values) throws Exception;
}
