/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.info;

import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.StringList;
import uk.ac.cam.ceb.como.nist.parser.NISTHTMLReaderHelper;
import uk.ac.cam.ceb.como.nist.parser.NISTParser;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

/**
 *
 * @author pb556
 */
public class NISTInfoReader extends NISTParser {

    protected NISTSpeciesInfo info = new NISTSpeciesInfo();

    @Override
    public void parseSection(StringList body) {
        info.setCASRegNr(extractCASRegNr(body));
        info.setFormula(extractFormula(body));
        info.setInChI(extractInChI(body));
        info.setInChIKey(extractInChIKey(body));
        info.setIsotopologues(extractIsotopologues(body));
        info.setMolecularWeight(extractMolecularWeight(body));
        info.setName(extractName(body));
        info.setOtherNames(extractOtherNames(body));
        info.setUrl2DMolFile(extractUrl2DMol(body));
        info.setUrl3DSDFile(extractUrl3DSD(body));
    }

    @Override
    public Object get() {
        return info;
    }

    protected String extractCASRegNr(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("CAS Registry Number:")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 0 && content.contains("CAS Registry Number:")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("CAS Registry Number:")) {
                    if (i + 1 < content.size()) {
                        // cas nr
                        if (verifyCASRegNr(content.get(i + 1).trim())) {
                            return content.get(i + 1).trim();
                        }
                    }
                }
            }
        }
        return "";
    }

    protected ChemFormula extractFormula(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("Formula</a>:")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return new ChemFormula("");
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 0 && content.contains("Formula")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("Formula")) {
                    String data = NISTHTMLReaderHelper.removeTags(body.get(lineIndex));
                    return new ChemFormula(data.replace("Formula:", ""));
                }
            }
        }
        return new ChemFormula("");
    }

    protected double extractMolecularWeight(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("Molecular weight</a>:")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return 0.0;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 0 && content.contains("Molecular weight")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("Molecular weight")) {
                    String data = NISTHTMLReaderHelper.removeTags(body.get(lineIndex));
                    String weight = data.replace("Molecular", "").replace("weight:", "").trim();
                    return Double.parseDouble(weight);
                }
            }
        }
        return 0.0;
    }

    protected Collection<String> extractIsotopologues(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("Isotopologues:")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return new ArrayList<String>();
        }

        HashSet<String> isotopologues = new HashSet<String>();
        for (int i = lineIndex + 2; i < body.size(); i++) {
            String content = NISTHTMLReaderHelper.removeTags(body.get(i));
            if (content.isEmpty()) {
                return isotopologues;
            }
            isotopologues.add(content.trim());
        }
        return isotopologues;
    }

    protected String extractName(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("<h1><a id=\"Top\" name=\"Top\">")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() == 1) {
            return content.get(0);
        }
        return "";
    }

    protected Collection<String> extractOtherNames(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("Other names:")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return new ArrayList<String>();
        }

        HashSet<String> otherNames = new HashSet<String>();
        for (int i = lineIndex + 1; i < body.size(); i++) {
            String content = NISTHTMLReaderHelper.removeTags(body.get(i));
            if (content.isEmpty()) {
                return otherNames;
            }
            String[] items = content.split(";");
            for (String item : items) {
                item = item.replace(";", "");
                otherNames.add(item.trim());
            }
        }
        return otherNames;
    }

    protected String extractInChI(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("InChI=")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 0) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("InChI=")) {
                    return NISTHTMLReaderHelper.removeTags(body.get(lineIndex));
                }
            }
        }
        return "";
    }

    protected String extractInChIKey(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("IUPAC Standard InChIKey:")) {
                for (int j = i + 1; j < body.size(); j++) {
                    if (body.get(j).length() > 20) {
                        lineIndex = j;
                        break;
                    }
                }
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() == 1) {
            return content.get(0);
        }
        return "";
    }

    protected boolean verifyCASRegNr(String cas) {
        return cas.matches("\\s*(\\d+)-(\\d+)-(\\d+)\\s*");
    }
    
    protected String extractUrl2DMol(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains(">2d Mol file</a>")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractTagValue(body.get(lineIndex), "href");
        if (content.size() == 1) {
            return content.get(0);
        }
        return "";
    }
    
    protected String extractUrl3DSD(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains(">3d SD file</a>")) {
                lineIndex = i;
                break;
            }
        }
        //body.getFirstMatchPosition(0, ".*CAS//s+Registry//s+Number:.*");
        if (lineIndex < 0) {
            return "";
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractTagValue(body.get(lineIndex), "href");
        if (content.size() == 1) {
            return content.get(0);
        }
        return "";
    }
}
