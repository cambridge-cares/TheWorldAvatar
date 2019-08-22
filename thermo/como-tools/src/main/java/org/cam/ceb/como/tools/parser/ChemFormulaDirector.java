/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.builder.Builder;
import org.cam.ceb.como.tools.pattern.builder.Director;

/**
 *
 * @author pb556
 */
public class ChemFormulaDirector extends Director {
    
    protected String content = null;
    protected Parser parser = null;
    
    public void setContent(String content) {
        this.content = content;
    }
    
    public void setParser(Builder parser) {
        
    }
    
    public boolean validityCheck() {
        if (this.content == null || this.parser == null) {
            return false;
        }
        return true;
    }

    @Override
    public void construct() {
        // parse the file
        if (!this.validityCheck()) {
            return;
        }
        this.parse(this.content);
    }
    
    private void parse(String content) {
        
    }
    
}
