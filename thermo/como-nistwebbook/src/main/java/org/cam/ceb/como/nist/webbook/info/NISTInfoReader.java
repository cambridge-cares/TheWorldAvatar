/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.info;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.StringList;
import org.cam.ceb.como.nist.webbook.parser.NISTHTMLReaderHelper;
import org.cam.ceb.como.nist.webbook.parser.NISTParser;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.EnthalpyType;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.ValueType;

/**
 *
 * @author pb556
 * 
 */
public class NISTInfoReader extends NISTParser {

    protected NISTSpeciesInfo info = new NISTSpeciesInfo();
    private Temperature temperature;
    private Pressure pressure;
    private List<NISTEnthalpy> enthalpies;

    @Override
    public void parseSection(StringList body) {
        info.setCASRegNr(extractCASRegNr(body));
        info.setFormula(extractFormula(body));
        info.setInChI(extractInChI(body));
        info.setInChIKey(extractInChIKey(body));
        info.setIsotopologues(extractIsotopologues(body));
        info.setMolecularWeight(extractMolecularWeight(body));
        info.setName(extractName(title));
        info.setOtherNames(extractOtherNames(body));
        info.setUrl2DMolFile(extractUrl2DMol(body));
        info.setUrl3DSDFile(extractUrl3DSD(body));
        info.setPermanentLink(extractPermanentLink(body));
        info.settBoil(extractTBoil(body));
        info.settCritical(extractTCritical(body));
        info.setpTriple(extractPTriple(body));
        info.settFusion(extractTFusion(body));
        info.setEnthalpy(extractEoF(body));
        info.setPhase(extractPhaseAtCondensedState(body));
        /**
         * @author NK510
         */
        
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
    
    /**
     * Extracts name of the current species.
     * 
     * @param title
     * @return
     */
    protected String extractName(String title) {
        String content = title;
        if(content.contains("<title") && content.indexOf(">")<content.length() && content.contains("</title>")){
        	content = content.substring(content.indexOf(">")+1, content.indexOf("</title>"));
        	return content;
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
    
    /**
     * 
     * @author NK510
     * @param body
     * @return permanent link as a string
     * 
     */
    protected String extractPermanentLink(StringList body) {
    	
    	int lineIndex = -1;
    	
    	for(int i =0; i<body.size();i++) {
    		
    		if(body.get(i).contains(">Permanent link</a>")) {
    		
    			lineIndex=i;
    			
    			break;
    		}
    	}
    		
    		if(lineIndex<0) {
    			
    			return "";
    		}
    		
    		ArrayList<String> content = NISTHTMLReaderHelper.extractTagValue(body.get(lineIndex), "href");
    		
    		if(content.size()==1) {
    			
    			return content.get(0);
    		}
    	
    	return "";
    }
    
    /**
     * Extract the boiling point (temperature and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected Temperature extractTBoil(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("T<sub>boil</sub></td><td")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 4 && content.contains("T")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("T") && content.get(i+1).trim().contains("boil")) {
                	// boiling temperature
                	temperature = new Temperature();
                	if(content.get(i+2).contains("&plusmn;")){
                		temperature.setValue(content.get(i+2).replace("&plusmn;", "±"));
                	}else{
                    	temperature.setValue(content.get(i+2));                		
                	}
                	temperature.setUnits(content.get(i+3));
                    return temperature;
                }
            }
        }
        return null;
    }

    /**
     * Extract the critical point (temperature and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected Temperature extractTCritical(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("T<sub>c</sub></td><td")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 4 && content.contains("T")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("T") && content.get(i+1).trim().contains("c")) {
                	// boiling temperature
                	temperature = new Temperature();
                	if(content.get(i+2).contains("&plusmn;")){
                		temperature.setValue(content.get(i+2).replace("&plusmn;", "±"));
                	}else{
                    	temperature.setValue(content.get(i+2));
                	}
                	temperature.setUnits(content.get(i+3));
                    return temperature;
                }
            }
        }
        return null;
    }
    
    /**
     * Extract the triple point pressure (and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected Pressure extractPTriple(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("P<sub>triple</sub></td><td")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 4 && content.contains("P")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("P") && content.get(i+1).trim().contains("triple")) {
                	// critical point pressure
                	pressure = new Pressure();
                	if(content.get(i+2).contains("&plusmn;")){
                		pressure.setValue(content.get(i+2).replace("&plusmn;", "±"));
                	}else{
                		pressure.setValue(content.get(i+2));
                	}
                	pressure.setUnits(content.get(i+3));
                    return pressure;
                }
            }
        }
        return null;
    }
    
    /**
     * Extract the fusion or melting point (temperature and units) of the</br> 
     * current species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected Temperature extractTFusion(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("T<sub>fus</sub></td><td")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 4 && content.contains("T")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("T") && content.get(i+1).trim().contains("fus")) {
                	// fusion point temperature
                	temperature = new Temperature();
                	if(content.get(i+2).contains("&plusmn;")){
                		temperature.setValue(content.get(i+2).replace("&plusmn;", "±"));
                	}else{
                		temperature.setValue(content.get(i+2));
                	}
                	temperature.setUnits(content.get(i+3));
                    return temperature;
                }
            }
        }
        return null;
    }
    
    /**
     * Extract the enthalpies of formation of the current species from</br> 
     * the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected List<NISTEnthalpy> extractEoF(StringList body) {
        int lineIndex = -1;
    	enthalpies = new ArrayList<NISTEnthalpy>();
        // Flag defined to stop looking for the enthalpies of formation when
    	//  the corresponding table is parsed.
    	boolean tableFlag = false;
    	for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("<sub>f</sub>H&deg;<sub>gas</sub></td><td")) {
                tableFlag = true; 
            	lineIndex = i;
                if (lineIndex < 0) {
                	return null;
                }
                ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
                if (content.size() > 6 && content.contains("f")) {
                	for (int j = 0; j < content.size(); j++) {
                		if (content.get(j).trim().contains("f") && content.get(j+1).trim().contains("H&deg;")) {
                			NISTEnthalpy enthalpy = new NISTEnthalpy();
                			enthalpies.add(enthalpy);
                			// fusion point temperature
                			if(content.get(j+3).contains("&plusmn;")){
                				String[] tokens = content.get(j+3).split("&plusmn;");
                				if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
                					enthalpy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
                					enthalpy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE, EnthalpyType.FORMATION);
                				}
                			}else{
                				if(isNumeric(content.get(j+3))){
                					enthalpy.setValue(Double.parseDouble(content.get(j+3)), ValueType.NONE, EnthalpyType.FORMATION);                			
                				}
                			}
                			enthalpy.setUnits(content.get(j+4));
                			enthalpy.setReference(content.get(j+6));
                		}
                		break;
                	}
                }
            }
            if(tableFlag && body.get(i).contains("</table>")){
            	return enthalpies;
            }
        }
    	return enthalpies;
    }

    /**
     * Extract the phase of the current species at condensed state from the</br>
     * corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected String extractPhaseAtCondensedState(StringList body) {
        int lineIndex = -1;
        // Defined a flag to identify the line within the "Condensed phase
        // thermochemistry data" block where the phase of interest appears.  
        boolean flag = false;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("id=\"Thermo-Condensed\">Condensed phase thermochemistry data<")) {
                flag = true;
            }
            if(flag && body.get(i).contains("><sub>f</sub>H&deg;<sub>")){
            	lineIndex = i;
            	break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
        if (content.size() > 2 && content.contains("f")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("f") && content.get(i+1).trim().contains("H&deg;")) {
                	// phase
                	if(content.get(i+2).contains("liquid") || content.get(i+2).contains("gas") || content.get(i+2).contains("solid")){
                		return content.get(i+2);
                	}
                }
            }
        }
        return "";
    }
    
    public static boolean isNumeric(String str) {
    	  return str.matches("-?\\d+(\\.\\d+)?");  //match a number with optional '-' and decimal.
    }
}