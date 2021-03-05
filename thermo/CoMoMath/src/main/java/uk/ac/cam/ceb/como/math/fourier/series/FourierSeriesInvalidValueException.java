/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series;

/**
 *
 * @author pb556
 */

public class FourierSeriesInvalidValueException extends Exception {

    public FourierSeriesInvalidValueException(Throwable cause) {
        super(cause);
    }

    public FourierSeriesInvalidValueException(String message, Throwable cause) {
        super(message, cause);
    }

    public FourierSeriesInvalidValueException(String message) {
        super(message);
    }

    public FourierSeriesInvalidValueException() {
        super();
    }

}