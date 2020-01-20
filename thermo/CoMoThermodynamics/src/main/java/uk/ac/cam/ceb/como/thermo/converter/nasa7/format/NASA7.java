/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.converter.nasa7.format;

/**
 *
 * @author pb556
 */
public class NASA7 {
    
    private static final int n_poly = 7;
    private double T_min = 20d;
    private double T_mid = 1000d;
    private double T_max = 4000d;
    private double[] a_low = new double[n_poly];
    private double[] a_high = new double[n_poly];
    private int polynomialCount = 2;
    
}
