/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.parser;

import org.cam.ceb.como.tools.pattern.composite.Leaf;

/**
 *
 * @author pb556
 */
public class Token extends Leaf {

    protected String chemFormula = null;
    protected int numBranches = 0;
    
    public Token() {
        this.chemFormula = null;
        this.numBranches = 0;
    }
    
    public Token(String chemFormula, int numBranches) {
        this.chemFormula = chemFormula;
        this.numBranches = numBranches;
    }

    public void set(String chemFormula, int numBranches) {
        this.chemFormula = chemFormula;
        this.numBranches = numBranches;
    }

    public String getChemicalFormula() {
        return this.chemFormula;
    }

    public int getNumberOfBranches() {
        return this.numBranches;
    }

    @Override
    public void execute() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
