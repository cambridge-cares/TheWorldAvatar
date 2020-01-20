/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm;

/**
 *
 * @author pb556
 */
public class InconcludingSolutionException extends Exception {

    public InconcludingSolutionException(Throwable cause) {
        super(cause);
    }

    public InconcludingSolutionException(String message, Throwable cause) {
        super(message, cause);
    }

    public InconcludingSolutionException(String message) {
        super(message);
    }

    public InconcludingSolutionException() {
        super();
    }

}