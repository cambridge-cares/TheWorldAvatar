/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public abstract class Parser {

    private Logger logger = Logger.getLogger(getClass());
    protected File file = null;

    public Parser() {
    }

    public Parser(File csv) {
        file = csv;
    }

    public void setPath(File csv) {
        file = csv;
    }

    public void setPath(String csv) {
        setPath(new File(csv));
    }
    
    public abstract Object get();

    public abstract void parse() throws FileNotFoundException, IOException;
}
