/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.fitting;

/**
 *
 * @author pb556
 */
public class FittingException  extends Exception {

    public FittingException(Throwable cause) {
        super(cause);
    }

    public FittingException(String message, Throwable cause) {
        super(message, cause);
    }

    public FittingException(String message) {
        super(message);
    }

    public FittingException() {
        super();
    }
}
