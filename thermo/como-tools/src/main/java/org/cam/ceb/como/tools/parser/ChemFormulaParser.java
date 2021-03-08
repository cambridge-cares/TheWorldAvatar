/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.composite.Composite;

/**
 *
 * @author pb556
 */
public class ChemFormulaParser extends Parser {
    
    protected Composite root = null;
    
    @Override
    public void build() throws Exception {
        // molecule structure and other information
        
        this.parse();
    }

    @Override
    public Object getProduct() {
        return this.root;
    }

    @Override
    public void parse() throws Exception {
        String content = this.obj.toString();
        
        if (this.lexer == null || content == null) {
            throw new Exception("Missing objects!");
        }
        
        this.lexer.setObject(content);
        this.lexer.build();
        this.root = (Composite) this.lexer.getProduct();
    }
    
}
