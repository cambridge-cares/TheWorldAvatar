/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.info;

/**
 *
 * @author pb556
 */
public class NISTSpeciesId {
    // species name
    // species formula
    // CAS registry number (if known)

    private String name = "";
    private String formula = "";
    private String CASRegNr = "";

    public NISTSpeciesId(String name, String formula, String CASRegNr) {
        this.name = name;
        this.formula = formula;
        this.CASRegNr = CASRegNr;
    }

    public String getName() {
        return name;
    }

    public String getFormula() {
        return formula;
    }

    public String getCASRegNr() {
        return CASRegNr;
    }
}
