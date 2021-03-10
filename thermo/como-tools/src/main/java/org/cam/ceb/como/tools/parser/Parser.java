/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.builder.Builder;

/**
 *
 * @author pb556
 */
public abstract class Parser implements Builder {

    protected Lexer lexer = null;
    protected Object obj = null;
    
    public void setObject(Object obj) {
        this.obj = obj;
    }

    public void setLexer(Lexer lexer) {
        this.lexer = lexer;
    }

    public abstract void parse() throws Exception;
}
