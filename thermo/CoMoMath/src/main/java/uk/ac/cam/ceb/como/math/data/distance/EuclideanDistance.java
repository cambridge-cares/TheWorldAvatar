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

public class EuclideanDistance extends NumericDistanceMeasure {

    @Override
    public Double getDistance(DataPoint p1, DataPoint p2) throws DistanceMeasureException {
        Object[] coordinateP1 = p1.getCoordinate();
        Object[] coordinateP2 = p2.getCoordinate();
        
        if (coordinateP1.length != coordinateP2.length) {
            throw new DistanceMeasureException();
        }
        
        double sum = 0.0;
        for (int i = 0; i < coordinateP1.length; i++) {
            Number n1 = (Number) coordinateP1[i];
            Number n2 = (Number) coordinateP2[i];
            double diff = n1.doubleValue() - n2.doubleValue();
            sum += Math.pow(diff, 2);
        }
        
        return Math.sqrt(sum);
    }

    @Override
    public Double[][] getDistance(DataSpace s) throws DistanceMeasureException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
