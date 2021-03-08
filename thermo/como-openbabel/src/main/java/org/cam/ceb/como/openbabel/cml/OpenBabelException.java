package org.cam.ceb.como.openbabel.cml;

/**
 *
 * @author Weerapong
 */
public class OpenBabelException extends Exception {

    public OpenBabelException(Throwable cause) {
        super(cause);
    }

    public OpenBabelException(String message, Throwable cause) {
        super(message, cause);
    }

    public OpenBabelException(String message) {
        super(message);
    }

    public OpenBabelException() {
        super();
    }

}
