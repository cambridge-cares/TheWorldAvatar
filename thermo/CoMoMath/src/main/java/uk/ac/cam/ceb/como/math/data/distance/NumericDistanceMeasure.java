/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.distance;

/**
 *
 * @author pb556
 */
public abstract class NumericDistanceMeasure implements DistanceMeasure<Number> {
    
    protected boolean isNumeric(Object obj) {
        return obj instanceof Number;
    }
}
