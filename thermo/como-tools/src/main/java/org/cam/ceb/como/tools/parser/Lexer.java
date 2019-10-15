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
public abstract class Lexer implements Builder{

    // character sequence that need to be analysed
    protected Object obj = null;
    protected Object product = null;

    public void setObject(Object obj) {
        this.obj = obj;
    }
    
    @Override
    public Object getProduct() {
        return this.product;
    }

    @Override
    public abstract void build() throws Exception;
}
