/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package org.cam.ceb.como.enthalpy.subset.selector;

/**
 *
 * @author Philipp
 */
public class SubSetSelectorException extends Exception {
    
    public SubSetSelectorException(Throwable cause) {
        super(cause);
    }

    public SubSetSelectorException(String message, Throwable cause) {
        super(message, cause);
    }

    public SubSetSelectorException(String message) {
        super(message);
    }

    public SubSetSelectorException() {
        super();
    }
}
