/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.distance;

import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;

/**
 *
 * @author pb556
 */
public interface DistanceMeasure<T> {
    
    public T getDistance(DataPoint p1, DataPoint p2) throws DistanceMeasureException ;
    
    public T[][] getDistance(DataSpace s) throws DistanceMeasureException ;
}
