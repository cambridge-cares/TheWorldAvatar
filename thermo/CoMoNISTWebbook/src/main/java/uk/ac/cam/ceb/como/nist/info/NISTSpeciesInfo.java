/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.info;

import java.util.Collection;
import java.util.HashSet;

/**
 *
 * @author pb556
 */
public class NISTSpeciesInfo {

    private String name = "";
    private String inchi = "";
    private String key = "";
    private String cas = "";
    private double weight = 0.0;
    private ChemFormula formula = null;
    private Collection<String> names = new HashSet<String>();
    private Collection<String> isotopologues = new HashSet<String>();
    private String url2D = "";
    private String url3D = "";

    public void setUrl2DMolFile(String url) {
        url2D = url;
    }
    
    public void setUrl3DSDFile(String url) {
        url3D = url;
    }
    
    public void setName(String name) {
        this.name = name;
    }

    public void setFormula(ChemFormula formula) {
        this.formula = formula;
    }

    public void setMolecularWeight(double weight) {
        this.weight = weight;
    }

    public void setInChI(String inchi) {
        this.inchi = inchi;
    }

    public void setInChIKey(String key) {
        this.key = key;
    }

    public void setCASRegNr(String nr) {
        cas = nr;
    }

    public void setOtherNames(Collection<String> names) {
        this.names = names;
    }

    public void setIsotopologues(Collection<String> isotopologues) {
        this.isotopologues = isotopologues;
    }

    public String getName() {
        return name;
    }

    public ChemFormula getFormula() {
        return formula;
    }

    public double getMolecularWeight() {
        return weight;
    }

    public String getInChI() {
        return inchi;
    }

    public String getInChIKey() {
        return key;
    }

    public String getCASRegNr() {
        return cas;
    }

    public Collection<String> getOtherNames() {
        return names;
    }

    public Collection<String> getIsotopologues() {
        return isotopologues;
    }
    
    public String getUrl2DMolFile() {
        return url2D;
    }
    
    public String getUrl3DSDFile() {
        return url3D;
    }
}
