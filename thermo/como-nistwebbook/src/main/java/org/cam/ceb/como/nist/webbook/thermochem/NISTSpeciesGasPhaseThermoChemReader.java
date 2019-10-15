/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.StringList;
import org.cam.ceb.como.nist.webbook.parser.NISTHTMLReaderHelper;
import org.cam.ceb.como.tools.util.StringUtil;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pb556
 */
public class NISTSpeciesGasPhaseThermoChemReader {
    // general info about the gas phase thermochemistry for the selected species
    // just an object containing the information needed about experiments

    protected String path = "";
    protected NISTSpeciesGasPhaseThermoChem thermoChem = new NISTSpeciesGasPhaseThermoChem();

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
            parseThermoChem(body);
        }
    }

    public NISTSpeciesGasPhaseThermoChem getThermoChemData() {
        return thermoChem;
    }

    protected void parseThermoChem(StringList body) {
        thermoChem.addAllEnthalpies(extractEnthalpy(body));

//        info.setCASRegNr(extractCASRegNr(body));
//        info.setFormula(extractFormula(body));
//        info.setInChI(extractInChI(body));
//        info.setInChIKey(extractInChIKey(body));
//        info.setIsotopologues(extractIsotopologues(body));
//        info.setMolecularWeight(extractMolecularWeight(body));
//        info.setName(extractName(body));
//        info.setOtherNames(extractOtherNames(body));
    }

    protected Collection<NISTEnthalpy> extractEnthalpy(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("<h2><a id=\"Thermo-Gas\" name=\"Thermo-Gas\">Gas phase thermochemistry data</a></h2>")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return new HashSet<NISTEnthalpy>();
        }

        StringList table = NISTHTMLReaderHelper.extractTable(lineIndex, body);
        // header lines are empty!!!
        ArrayList<ArrayList<String>> data = NISTHTMLReaderHelper.extractTableCellEntries(table);
        ArrayList<NISTEnthalpy> enthalpies = new ArrayList<NISTEnthalpy>();

        for (int i = 0; i < data.size(); i++) {
            if (data.get(i).size() == 6) {
                NISTEnthalpy enthalpy = new NISTEnthalpy();
                enthalpy.setTolerance(0, 0);
                enthalpy.setComment(NISTHTMLReaderHelper.extractContent(data.get(i).get(5)).get(0));
                enthalpy.setMethod(NISTHTMLReaderHelper.extractContent(data.get(i).get(3)).get(0));
                enthalpy.setReference(NISTHTMLReaderHelper.extractContent(data.get(i).get(4)).get(0));
                enthalpy.setUnits(NISTHTMLReaderHelper.extractContent(data.get(i).get(2)).get(0));

                String valueInclTol = NISTHTMLReaderHelper.extractContent(data.get(i).get(1)).get(0);
                String[] items = valueInclTol.split(" ");

                if (items.length > 0) {
                    try {
                        double value = 0.0;
                        NISTEnthalpy.ValueType vType = NISTEnthalpy.ValueType.EXACT;
                        NISTEnthalpy.EnthalpyType eType = NISTEnthalpy.EnthalpyType.NONE;
                        if (StringUtil.isNumber(items[0].trim())) {
                            value = Double.parseDouble(items[0]);
                        } else if (items[0].contains("&lt;")) {
                            vType = NISTEnthalpy.ValueType.UPPER_THRESHOLD;
                        } else if (items[0].contains("&gt;")) {
                            vType = NISTEnthalpy.ValueType.LOWER_THRESHOLD;
                        }
                        if (data.get(i).get(0).contains("<sub>f</sub>H&deg;<sub>gas</sub>")) {
                            eType = NISTEnthalpy.EnthalpyType.FORMATION;
                        }
                        if (data.get(i).get(0).contains("<sub>c</sub>H&deg;<sub>gas</sub>")) {
                            eType = NISTEnthalpy.EnthalpyType.COMBUSTION;
                        }
                        enthalpy.setValue(value, NISTEnthalpy.ValueType.EXACT, eType);//data.get(i).get(0).contains("<sub>f</sub>H&deg;<sub>gas</sub>"));
                        double pos = parseValueInclTolerance(valueInclTol, "&plusmn;");
                        double neg = parseValueInclTolerance(valueInclTol, "&plusmn;");
                        if (valueInclTol.contains("&plusmn;")) {
                            pos = Double.parseDouble(items[2]);
                            neg = pos;
                        }
                        if (pos == -1) {
                            pos = parseValueInclTolerance(valueInclTol, "&plus;");
                        } else {
                            pos = 0;
                        }
                        if (neg == -1) {
                            neg = parseValueInclTolerance(valueInclTol, "&mn;");
                        } else {
                            neg = 0;
                        }
                        enthalpy.setTolerance(pos, neg);
                        enthalpies.add(enthalpy);
                    } catch (Exception e) {
                        System.out.println(path);
                    }
                }
            }
        }

        return enthalpies;
    }

    private double parseValueInclTolerance(String s, String sign) {
        if (s.contains("&plus;")) {
            String[] items = s.split(" ");
            for (int j = 0; j < items.length; j++) {
                if (items[j].contains(sign)) {
                    return Double.parseDouble(items[j + 1]);
                }
            }
        }
        return -1;
    }
}
