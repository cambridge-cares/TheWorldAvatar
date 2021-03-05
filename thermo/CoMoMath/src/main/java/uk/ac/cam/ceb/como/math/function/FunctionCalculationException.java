/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.function;

/**
 *
 * @author pb556
 */
public class FunctionCalculationException extends Exception {

    public FunctionCalculationException(Throwable cause) {
        super(cause);
    }

    public FunctionCalculationException(String message, Throwable cause) {
        super(message, cause);
    }

    public FunctionCalculationException(String message) {
        super(message);
    }

    public FunctionCalculationException() {
        super();
    }

}
