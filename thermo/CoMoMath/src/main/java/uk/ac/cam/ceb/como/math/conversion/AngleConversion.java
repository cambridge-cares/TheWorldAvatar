/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.conversion;

/**
 *
 * @author pb556
 */
public class AngleConversion {
    public static double toRadians(double degrees) {
        return degrees * Math.PI / 180.0;
    }
    
    public static double toDegrees(double radians) {
        return radians * 180.0 / Math.PI;
    }
}
