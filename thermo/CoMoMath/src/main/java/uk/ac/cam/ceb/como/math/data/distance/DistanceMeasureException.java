/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.distance;

/**
 *
 * @author pb556
 */
public class DistanceMeasureException extends Exception {
    
    public DistanceMeasureException(Throwable cause) {
        super(cause);
    }

    public DistanceMeasureException(String message, Throwable cause) {
        super(message, cause);
    }

    public DistanceMeasureException(String message) {
        super(message);
    }

    public DistanceMeasureException() {
        super();
    }
}
