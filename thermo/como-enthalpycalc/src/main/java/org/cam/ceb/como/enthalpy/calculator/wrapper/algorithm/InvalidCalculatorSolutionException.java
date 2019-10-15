/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm;

/**
 *
 * @author pb556
 */
public class InvalidCalculatorSolutionException extends Exception {

    public InvalidCalculatorSolutionException(Throwable cause) {
        super(cause);
    }

    public InvalidCalculatorSolutionException(String message, Throwable cause) {
        super(message, cause);
    }

    public InvalidCalculatorSolutionException(String message) {
        super(message);
    }

    public InvalidCalculatorSolutionException() {
        super();
    }

}
