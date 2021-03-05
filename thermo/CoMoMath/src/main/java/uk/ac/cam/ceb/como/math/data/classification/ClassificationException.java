/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.data.classification;

/**
 *
 * @author pb556
 */
public class ClassificationException extends Exception {

    public ClassificationException(Throwable cause) {
        super(cause);
    }

    public ClassificationException(String message, Throwable cause) {
        super(message, cause);
    }

    public ClassificationException(String message) {
        super(message);
    }

    public ClassificationException() {
        super();
    }
}
