/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.ceb.como.ontokin.exception;

/**
 *
 * @author pb556
 */
public class ConverterException extends Exception {

    public ConverterException(Throwable cause) {
        super(cause);
    }

    public ConverterException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConverterException(String message) {
        super(message);
    }

    public ConverterException() {
        super();
    }
}
