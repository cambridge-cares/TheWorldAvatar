/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.StringList;
import org.cam.ceb.como.nist.webbook.parser.NISTHTMLReaderHelper;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pb556
 */
public class NISTGasPhaseThermoChemReader {
    // read data
    // extract entropy data
    // extract enthalpy data
    // extract nasa data
    // extract references

    protected String path = "";

    public NISTGasPhaseThermoChemReader() {
    }

    public void setPath(String path) {
        this.path = path;
    }

    public void parse() {
        StringList body = null;
        try {
            body = NISTHTMLReaderHelper.extractHTMLBody(new File(path));
        } catch (FileNotFoundException ex) {
            Logger.getLogger(NISTGasPhaseThermoChemReader.class.getName()).log(Level.SEVERE, "The file " + path + "does not exist.", ex);
        } catch (IOException ex) {
            Logger.getLogger(NISTGasPhaseThermoChemReader.class.getName()).log(Level.SEVERE, "The file " + path + "could not be read.", ex);
        }
        if (body != null) {
            parseGeneralInformation(body);
            parseThermoChemData(body);
        }
    }

    protected void parseGeneralInformation(StringList body) {
        // GENERAL INFO
        // Titel
        // Formula
        // Molecular weight
        // IUPAC Standard InChI
        // IUPAC Standard InChIKey
        // CAS Registry Number
        // Name
        // Other names
        // Isotopologues
    }

    protected void parseThermoChemData(StringList body) {
    }
    // GAS PHASE THERMOCHEMISTRY DATA
    // Enthalpy
    // Entropy
    // Heat capacity
    // Constant pressure heat capacity of gas
    // Gas Phase Heat Capacity (Shomate Equation) --> Polynomial coefficients
}
