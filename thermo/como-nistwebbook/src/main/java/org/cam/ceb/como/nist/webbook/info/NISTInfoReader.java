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
import org.cam.ceb.como.nist.webbook.thermochem.NISTDensity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.EnthalpyType;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.ValueType;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfX;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfXStCondition;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEntropy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTGibbsFreeEnergy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHeatCapacity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHenrysLawConstant;
import org.cam.ceb.como.nist.webbook.thermochem.NISTVolume;

/**
 * A class created for parsing information about a chemical species and </br>
 * populating the in-memory data structure of the species.  
 *
 * @author pb556
 * @author msff2
 * 
 */
public class NISTInfoReader extends NISTParser {

    protected NISTSpeciesInfo info = new NISTSpeciesInfo();
    private NISTTemperature temperature;
    private NISTPressure pressure;
    private NISTVolume volume;
    private NISTDensity density;
    private NISTEnthalpyOfXStCondition enthalpyOfVapAtStC;
    private NISTEnthalpyOfXStCondition enthalpyOfSubAtStC;
    private List<NISTEnthalpy> enthalpies;
    private NISTEnthalpy enthalpy;
    private List<AppearanceEnergy> appearanceEnergies;
    private AppearanceEnergy appearanceEnergy;
    private List<IonisationEnergy> ionisationEnergies;
    private IonisationEnergy ionisationEnergy;
    private List<NISTHeatCapacity> heatCapacities;
    private NISTHeatCapacity heatCapacity;
    private NISTEnthalpyOfX enthalpyOfX;
    private NISTEntropy entropy;
    private NISTGibbsFreeEnergy gibbsFreeEnergy;
    private NISTHenrysLawConstant henrysLawConstant;
    // Defined for storing units temporarily
    private String unit;
    // Defined for storing any value temporarily
    private String temp;
    // Defined for storing references temporarily
    private String reference;
    // Defined for storing comments temporarily
    private String comment;
    // Defined for storing method temporarily
    private String method;    
    // Defined for storing multiple units reported in a table of data
    private List<String> units;
    // Defined for temporarily storing the name of solution for which</br>
    // the solubility of the current species is reported, e.g. water solution
    private String solution;
    // Defined for temporarily storing an extracted reaction
    private String reaction;
    // Defined for temporarily storing an extracted line of content
    private ArrayList<String> content;

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
        extractTTriple(body);
        extractPTriple(body);
        extractTCritical(body);
        extractPCritical(body);
        extractVCritical(body);
        extractρCritical(body);
        extractDeltaVapHDegree(body);
        extractDeltaSubHDegree(body);
        extractDeltaVapH(body);
        extractDeltaSubH(body);
        extractDeltaFusH(body);
        extractDeltaFusS(body);
        extractDeltaSubS(body);
        extractDeltaVapS(body);
        info.settFusion(extractTFusion(body));
        extractEnthalpyOfFormationInLiquid(body);
        extractEnthalpyOfFormationInGas(body);
        extractEnthalpyOfFormationInSolid(body);
        extractEnthalpyOfCombustionInGas(body);
        extractEnthalpyOfCombustionInLiquid(body);
        extractEnthalpyOfCombustionInSolid(body);
        extractEntropyInLiquidAtStCondition(body);
        extractEntropyInGasAtStCondition(body);
        extractEntropyInSolidAtStCondition(body);
        extractHenrysLawConstant(body);
        extractEnthalpyOfReaction(body);
        extractEntropyOfReaction(body);
        extractGibbsFreeEnergyOfReaction(body);
        info.setPhase(extractPhaseAtCondensedState(body));
        info.setAppearanceEnergy(extractAppearanceEnergy(body));
        info.setIonisationEnergy(extractIonisationEnergy(body));
        info.setHeatCapacityOfGas(extractHeatCapacityOfGas(body));
        info.setHeatCapacityOfLiquid(extractHeatCapacityOfLiquid(body));
        info.setHeatCapacityOfSolid(extractHeatCapacityOfSolid(body));
        /**
         * @author NK510
         * @author msff2
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
                    System.out.println("lineindex:"+lineIndex+" line:"+body.get(lineIndex));
                	String data = NISTHTMLReaderHelper.removeTags(body.get(lineIndex));
                    return new ChemFormula(data.replace("Formula:", "").trim());
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
     * Extracts the name of the current species.
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
    
    /**
     * Extracts alternative names of the current species.
     * 
     * @param body
     * @return
     */
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
    
    /**
     * Extracts the InChI identifier of the current species.
     * 
     * @param body
     * @return
     */
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
    
    /**
     * Extracts the InChI key of the current species. 
     * 
     * @param body
     * @return
     */
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
    
    /**
     * Extracts the CAS Registration number of the current species.
     * 
     * @param cas
     * @return
     */
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
     * Extracts the boiling point (temperature and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected NISTTemperature extractTBoil(StringList body) {
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
        if (content.size() > 5 && content.contains("T")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("T") && content.get(i+1).trim().contains("boil")) {
                	temperature = new NISTTemperature();
                	formatTemperature(temperature, content.get(i+2));
                	temperature.setUnits(content.get(i+3));
                	if(!content.get(i+5).contains("N/A") && !content.get(i+5).isEmpty()){
                		temperature.setReference(content.get(i+5));
                	}
                    return temperature;
                }
            }
        }
        return null;
    }
    
    /**
     * Extracts the critical point (temperature and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractTCritical(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("T<sub>c</sub></td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("T")) {
					int j = 0;
						if (content.get(j).trim().contains("T") && content.get(j + 1).trim().contains("c")) {
							// critical point temperature
							temperature = new NISTTemperature();
							formatTemperature(temperature, content.get(j+2));
							temperature.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								temperature.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								temperature.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								temperature.setComment(comment);
							}
							info.gettCritical().add(temperature);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("T<sub>c</sub></td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the critical point pressure and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractPCritical(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("P<sub>c</sub></td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("P")) {
					int j = 0;
						if (content.get(j).trim().contains("P") && content.get(j + 1).trim().contains("c")) {
							// critical point pressure
							pressure = new NISTPressure();
							if (content.get(j + 2).contains("&plusmn;")) {
								pressure.setValue(content.get(j + 2).replace("&plusmn;", "±"));
							} else {
								pressure.setValue(content.get(j + 2));
							}
							pressure.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								pressure.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								pressure.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								pressure.setComment(comment);
							}
							info.getpCritical().add(pressure);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("P<sub>c</sub></td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the critical volume and units of the current species from</br> 
     * the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractVCritical(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("V<sub>c</sub></td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("V")) {
					int j = 0;
						if (content.get(j).trim().contains("V") && content.get(j + 1).trim().contains("c")) {
							// critical point volume
							volume = new NISTVolume();
							if (content.get(j + 2).contains("&plusmn;")) {
								volume.setValue(content.get(j + 2).replace("&plusmn;", "±"));
							} else {
								volume.setValue(content.get(j + 2));
							}
							volume.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								volume.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								volume.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								volume.setComment(comment);
							}
							info.getvCritical().add(volume);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("V<sub>c</sub></td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the critical density and units of the current species from</br> 
     * the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractρCritical(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("alt=\"rho\" /><sub>c</sub></td>")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("c")) {
					int j = 0;
						if (content.get(j).trim().contains("c")) {
							// critical point density
							density = new NISTDensity();
							if (content.get(j + 1).contains("&plusmn;")) {
								density.setValue(content.get(j + 1).replace("&plusmn;", "±"));
							} else {
								density.setValue(content.get(j + 1));
							}
							density.setUnits(content.get(j + 2));
							if (!content.get(j + 3).contains("N/A") && !content.get(j + 3).isEmpty()) {
								density.setMethod(content.get(j + 3));
							}
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								density.setReference(content.get(j + 4));
							}
							comment = "";
							for(int k=j+5;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								density.setComment(comment);
							}
							info.getρCritical().add(density);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("alt=\"rho\" /><sub>c</sub></td>")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the Enthalpy of vaporization at standard conditions and</br> 
     * units of the current species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaVapHDegree(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>vap</sub>H&deg;</td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 6 && content.contains("vap") && content.contains("H&deg;")) {
					int j = 0;
						if (content.get(j).trim().contains("vap") && content.get(j+1).trim().contains("H&deg;")) {
							// Enthalpy of vaporisation at standard conditions. 
							enthalpyOfVapAtStC = new NISTEnthalpyOfXStCondition();
							if (content.get(j + 2).contains("&plusmn;")) {
								enthalpyOfVapAtStC.setValue(content.get(j + 2).replace("&plusmn;", "±"));
							} else {
								enthalpyOfVapAtStC.setValue(content.get(j + 2));
							}
							enthalpyOfVapAtStC.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								enthalpyOfVapAtStC.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								enthalpyOfVapAtStC.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								enthalpyOfVapAtStC.setComment(comment);
							}
							info.getEnthalpyOfVapAtStC().add(enthalpyOfVapAtStC);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("<sub>vap</sub>H&deg;</td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the Enthalpy of sublimation at standard conditions and</br> 
     * units of the current species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaSubHDegree(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>sub</sub>H&deg;</td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 6 && content.contains("sub") && content.contains("H&deg;")) {
					int j = 0;
						if (content.get(j).trim().contains("sub") && content.get(j+1).trim().contains("H&deg;")) {
							// Enthalpy of vaporisation at standard conditions. 
							enthalpyOfSubAtStC = new NISTEnthalpyOfXStCondition();
							if (content.get(j + 2).contains("&plusmn;")) {
								enthalpyOfSubAtStC.setValue(content.get(j + 2).replace("&plusmn;", "±"));
							} else {
								enthalpyOfSubAtStC.setValue(content.get(j + 2));
							}
							enthalpyOfSubAtStC.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								enthalpyOfSubAtStC.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								enthalpyOfSubAtStC.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								enthalpyOfSubAtStC.setComment(comment);
							}
							info.getEnthalpyOfSubAtStC().add(enthalpyOfSubAtStC);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("<sub>sub</sub>H&deg;</td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extracts the Enthalpy of vaporisation and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaVapH(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Enthalpy of vaporization\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("vap") && content.get(1).contains("H")) {
						// Adding the units of enthalpy of vaporisation
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				enthalpyOfX = new NISTEnthalpyOfX();
				info.getEnthalpiesOfVap().add(enthalpyOfX);
			}
			// Extract the enthalpy of a vaporisation process of the current species.
			extractDeltaH(i, lineIndex, content, units, enthalpyOfX);
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }
    
    /**
     * Extracts the Enthalpy of sublimation and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaSubH(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Enthalpy of sublimation\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("sub") && content.get(1).contains("H")) {
						// Adding the units of enthalpy of vaporisation
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				enthalpyOfX = new NISTEnthalpyOfX();
				info.getEnthalpiesOfSub().add(enthalpyOfX);
			}
			// Extract the enthalpy of a sublimation process of the current species.
			extractDeltaH(i, lineIndex, content, units, enthalpyOfX);
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }

    /**
     * Extracts the Enthalpy of fusion and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaFusH(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Enthalpy of fusion\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("fus") && content.get(1).contains("H")) {
						// Adding the units of enthalpy of vaporisation
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				enthalpyOfX = new NISTEnthalpyOfX();
				info.getEnthalpiesOfFus().add(enthalpyOfX);
			}
			// Extract the enthalpy of a sublimation process of the current species.
			extractDeltaH(i, lineIndex, content, units, enthalpyOfX);
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }
    
    /**
     * Extract the enthalpy of a conversion process of the current species.
     * 
     * @param i
     * @param lineIndex
     * @param content
     * @param units
     */
    private void extractDeltaH(int i, int lineIndex, List<String> content, List<String> units, NISTEnthalpyOfX enthalpyOfX){
		if (i - (lineIndex + 1) == 1) {
			if (content.size() > 0) {
				if (content.get(0).contains("Temperature")) {
					// Adds the units of temperature.
					units.add(1,content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")")));
				}
			}
		}
		if (content.size() > 4 && body.get(i).contains("<tr class")) {
			method = content.get(2);
			reference = content.get(3);
			comment = "";
			for(int j=4;j<content.size();j++){
				comment = comment.concat(content.get(j));
			}
			if(comment.contains("&nbsp;")){
				comment = comment.replace("&nbsp;", "");
			}
		}
		if (content.size() > 1 && body.get(i).contains("<tr class")) {
			if (content.get(0).contains("&plusmn;")) {
				enthalpyOfX.setValue(content.get(0).replace("&plusmn;", "±"));
			} else {
				enthalpyOfX.setValue(content.get(0));
			}
			if(units.size()>0){
				enthalpyOfX.setUnits(units.get(0));
			}
			temperature = new NISTTemperature();
			formatTemperature(temperature, content.get(1));
			if(units.size()>1){
				temperature.setUnits(units.get(1));
			}
			enthalpyOfX.setTemperature(temperature);
			if (method!=null && !method.contains("N/A") && !method.isEmpty()) {
				enthalpyOfX.setMethod(method);
			}
			if (reference!=null && !reference.contains("N/A") && !reference.isEmpty()) {
				enthalpyOfX.setReference(reference);
			}
			if(comment!=null && comment.trim().length()>0){
				enthalpyOfX.setComment(comment);
			}
		}
    }
    
    
    /**
     * Extracts the entropy of fusion and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaFusS(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Entropy of fusion\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("fus") && content.get(1).contains("S")) {
						// Adds the units of entropy of fusion
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 0) {
					if (content.get(0).contains("Temperature")) {
						// Adds the units of temperature.
						units.add(1,content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")")));
					}
				}
			}
			if (content.size() > 3 && body.get(i).contains("<tr class")) {
				reference = content.get(2);
				comment = "";
				for(int j=3;j<content.size();j++){
					comment = comment.concat(content.get(j));
				}
				if(comment.contains("&nbsp;")){
					comment = comment.replace("&nbsp;", "");
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				entropy = new NISTEntropy();
    			if(content.get(0).contains("&plusmn;")){
    				String[] tokens = content.get(0).split("&plusmn;");
    				if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
    					entropy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
    					entropy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
    				}
    			}else{
    				if(isNumeric(content.get(0))){
    					entropy.setValue(Double.parseDouble(content.get(0)), ValueType.NONE);                			
    				}
    			}
				if(units.size()>0){
					entropy.setUnits(units.get(0));
				}
				temperature = new NISTTemperature();
				formatTemperature(temperature, content.get(1));
				if(units.size()>1){
					temperature.setUnits(units.get(1));
				}
				entropy.setTemperature(temperature);
				entropy.setReference(reference);
				if(comment!=null && comment.trim().length()>0){
					entropy.setComment(comment);
				}
				info.getEntropyOfFusion().add(entropy);
			}
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }

    /**
     * Extracts the entropy of sublimation and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaSubS(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Entropy of sublimation\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("sub") && content.get(1).contains("S")) {
						// Adds the units of entropy of fusion
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 0) {
					if (content.get(0).contains("Temperature")) {
						// Adds the units of temperature.
						units.add(1,content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")")));
					}
				}
			}
			if (content.size() > 3 && body.get(i).contains("<tr class")) {
				reference = content.get(2);
				comment = "";
				for(int j=3;j<content.size();j++){
					comment = comment.concat(content.get(j));
				}
				if(comment.contains("&nbsp;")){
					comment = comment.replace("&nbsp;", "");
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				entropy = new NISTEntropy();
    			if(content.get(0).contains("&plusmn;")){
    				String[] tokens = content.get(0).split("&plusmn;");
    				if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
    					entropy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
    					entropy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
    				}
    			}else{
    				if(isNumeric(content.get(0))){
    					entropy.setValue(Double.parseDouble(content.get(0)), ValueType.NONE);                			
    				}
    			}
				if(units.size()>0){
					entropy.setUnits(units.get(0));
				}
				temperature = new NISTTemperature();
				formatTemperature(temperature, content.get(1));
				if(units.size()>1){
					temperature.setUnits(units.get(1));
				}
				entropy.setTemperature(temperature);
				entropy.setReference(reference);
				if(comment.trim().length()>0){
					entropy.setComment(comment);
				}
				info.getEntropyOfSublimation().add(entropy);
			}
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }

    /**
     * Extracts the entropy of vaporisation and units of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractDeltaVapS(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("\"Entropy of vaporization\"><tr>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return;
		}
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 0) {
				if (content.size() > 1) {
					if (content.get(0).contains("vap") && content.get(1).contains("S")) {
						// Adds the units of entropy of fusion
						units.add(0,content.get(1).substring(content.get(1).indexOf("(") + 1, content.get(1).indexOf(")")));
					}
				}
			}
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 0) {
					if (content.get(0).contains("Temperature")) {
						// Adds the units of temperature.
						units.add(1,content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")")));
					}
				}
			}
			if (content.size() > 3 && body.get(i).contains("<tr class")) {
				reference = content.get(2);
				comment = "";
				for(int j=3;j<content.size();j++){
					comment = comment.concat(content.get(j));
				}
				if(comment.contains("&nbsp;")){
					comment = comment.replace("&nbsp;", "");
				}
			}
			if (content.size() > 1 && body.get(i).contains("<tr class")) {
				entropy = new NISTEntropy();
    			if(content.get(0).contains("&plusmn;")){
    				String[] tokens = content.get(0).split("&plusmn;");
    				if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
    					entropy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
    					entropy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
    				}
    			}else{
    				if(isNumeric(content.get(0))){
    					entropy.setValue(Double.parseDouble(content.get(0)), ValueType.NONE);                			
    				}
    			}
				if(units.size()>0){
					entropy.setUnits(units.get(0));
				}
				temperature = new NISTTemperature();
				formatTemperature(temperature, content.get(1));
				if(units.size()>1){
					temperature.setUnits(units.get(1));
				}
				entropy.setTemperature(temperature);
				entropy.setReference(reference);
				if(comment.trim().length()>0){
					entropy.setComment(comment);
				}
				info.getEntropyOfVaporisation().add(entropy);
			}
			if (body.get(i).contains("</table>")) {
				return;
			}
		}
		return;
    }
    
    /**
     * Extracts the triple point pressure (and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractPTriple(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("P<sub>triple</sub></td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("P")) {
					int j = 0;
					if (content.get(j).trim().contains("P") && content.get(j + 1).trim().contains("triple")) {
							// critical point pressure
							pressure = new NISTPressure();
							if (content.get(j + 2).contains("&plusmn;")) {
								pressure.setValue(content.get(j + 2).replace("&plusmn;", "±"));
							} else {
								pressure.setValue(content.get(j + 2));
							}
							pressure.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								pressure.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								pressure.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								pressure.setComment(comment);
							}
							info.getpTriple().add(pressure);
						}
				}
			}else if((lineIndex>=0) && !body.get(i).contains("P<sub>triple</sub></td><td")){
				return;
			}
		}
        return;
    }
    
    /**
     * Extract the triple point temperature (and units) of the current</br> 
     * species from the corresponding HTML file.
     * 
     * @param body
     */
    protected void extractTTriple(StringList body) {
        int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("T<sub>triple</sub></td><td")) {
				// Nonnegative lineIndex proofs that the above statement is 
				// contained in the file.
				lineIndex = i;  
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 5 && content.contains("T")) {
					int j = 0;
						if (content.get(j).trim().contains("T") && content.get(j + 1).trim().contains("triple")) {
							// critical point pressure
							temperature = new NISTTemperature();
							formatTemperature(temperature, content.get(j+2));
							temperature.setUnits(content.get(j + 3));
							if (!content.get(j + 4).contains("N/A") && !content.get(j + 4).isEmpty()) {
								temperature.setMethod(content.get(j + 4));
							}
							if (!content.get(j + 5).contains("N/A") && !content.get(j + 5).isEmpty()) {
								temperature.setReference(content.get(j + 5));
							}
							comment = "";
							for(int k=j+6;k<content.size();k++){
								comment = comment.concat(content.get(k));
							}
							if(comment.length()>0){
								temperature.setComment(comment);
							}
							info.gettTriple().add(temperature);
						}
				}
			}else if((lineIndex>0) && !body.get(i).contains("T<sub>triple</sub></td><td")){
				return;
			}
		}
        return;
    }

    /**
     * Extract the fusion or melting point (temperature and units) of the</br> 
     * current species from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected NISTTemperature extractTFusion(StringList body) {
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
        if (content.size() > 5 && content.contains("T")) {
            for (int i = 0; i < content.size(); i++) {
                if (content.get(i).trim().contains("T") && content.get(i+1).trim().contains("fus")) {
                	// fusion point temperature
                	temperature = new NISTTemperature();
                	formatTemperature(temperature, content.get(i+2));
                	temperature.setUnits(content.get(i+3));
                	if(!content.get(i+5).contains("N/A") && !content.get(i+5).isEmpty()){
                		temperature.setReference(content.get(i+5));
                	}
                    return temperature;
                }
            }
        }
        return null;
    }
    
    /**
     * Extract the enthalpies of formation of the current species in the</br> 
     * gas-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfFormationInGas(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>f</sub>H&deg;<sub>gas</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("f")) {
					if (content.get(0).trim().contains("f") && content.get(1).trim().contains("H&deg;")
							&& content.get(2).trim().contains("gas")) {
						NISTEnthalpy enthalpy = new NISTEnthalpy();
						info.getEnthalpyOfFormationInGas().add(enthalpy);
						// Processes the enthalpy of formation of a gaseous
						// substance to extract the value, units, etc.
						processEnthalpyOfFormation(content, enthalpy);
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}
    
    /**
     * Extract the enthalpies of formation of the current species from</br> 
     * the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfFormationInLiquid(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>f</sub>H&deg;<sub>liquid</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("f")) {
					if (content.get(0).trim().contains("f") && content.get(1).trim().contains("H&deg;")
							&& content.get(2).trim().contains("liquid")) {
						NISTEnthalpy enthalpy = new NISTEnthalpy();
						info.getEnthalpyOfFormationInLiquid().add(enthalpy);
						// Processes the enthalpy of formation of a liquid
						// substance to extract the value, units, etc.
						processEnthalpyOfFormation(content, enthalpy);
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}

    /**
     * Extract the enthalpies of formation of the current species in the</br> 
     * solid-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfFormationInSolid(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>f</sub>H&deg;<sub>solid</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("f")) {
					if (content.get(0).trim().contains("f") && content.get(1).trim().contains("H&deg;")
							&& content.get(2).trim().contains("solid")) {
						NISTEnthalpy enthalpy = new NISTEnthalpy();
						info.getEnthalpyOfFormationInSolid().add(enthalpy);
						// Processes the enthalpy of formation of a solid
						// substance to extract the value, units, etc.
						processEnthalpyOfFormation(content, enthalpy);
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}
    
    /**
     * Processes the enthalpy of formation of a gas, liquid or solid</br>
     * substance to extract, identify and codify them.  
     * 
     * @param content
     * @param enthalpy
     */
    private void processEnthalpyOfFormation(ArrayList<String> content, NISTEnthalpy enthalpy){
		if(content.get(3).contains("&plusmn;")){
			String[] tokens = content.get(3).split("&plusmn;");
			if(tokens[0].trim().endsWith(".")){
				tokens[0] = tokens[0].trim().concat("0");
			}
			if(tokens[1].trim().endsWith(".")){
				tokens[1] = tokens[1].trim().concat("0");
			}
			if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
				enthalpy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
				enthalpy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE, EnthalpyType.FORMATION);
			}
		}else{
			if(isNumeric(content.get(3))){
				enthalpy.setValue(Double.parseDouble(content.get(3)), ValueType.NONE, EnthalpyType.FORMATION);
			}
		}
		enthalpy.setUnits(content.get(4));
    	if(!content.get(5).contains("N/A")){
    		enthalpy.setMethod(content.get(5));
    	}
    	if(!content.get(6).contains("N/A")){
    		enthalpy.setReference(content.get(6));
    	}
    	comment = "";
    	for(int k=7;k<content.size();k++){
    		if(!content.get(7).contains("N/A") && !content.get(7).isEmpty()){
    			comment = comment.concat(content.get(k));
    		}
    	}
		if(comment.contains("&nbsp;")){
			comment = comment.replace("&nbsp;", "");
		}
		if(comment.trim().length()>0){
			enthalpy.setComment(comment);
		}
    }
    
    /**
     * Extract the enthalpies of combustion of the current species in the</br> 
     * gas-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfCombustionInGas(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>c</sub>H&deg;<sub>gas</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("c")) {
					if (content.size() > 7 && content.contains("c")) {
						if (content.get(0).trim().contains("c") && content.get(1).trim().contains("H&deg;")
								&& content.get(2).trim().contains("gas")) {
							NISTEnthalpy enthalpy = new NISTEnthalpy();
							info.getEnthalpyOfCombustionInGas().add(enthalpy);
							// Processes the enthalpy of combustion of a liquid
							// substance to extract the value, units, etc.
							processEnthalpyOfCombustion(content, enthalpy);
						}
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}
    
    /**
     * Extract the enthalpies of combustion of the current species from</br> 
     * the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfCombustionInLiquid(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>c</sub>H&deg;<sub>liquid</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("c")) {
					if (content.get(0).trim().contains("c") && content.get(1).trim().contains("H&deg;")
							&& content.get(2).trim().contains("liquid")) {
						NISTEnthalpy enthalpy = new NISTEnthalpy();
						info.getEnthalpyOfCombustionInLiquid().add(enthalpy);
						// Processes the enthalpy of combustion of a liquid
						// substance to extract the value, units, etc.
						processEnthalpyOfCombustion(content, enthalpy);
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}

    /**
     * Extract the enthalpies of combustion of the current species in the</br> 
     * solid-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
	protected List<NISTEnthalpy> extractEnthalpyOfCombustionInSolid(StringList body) {
		int lineIndex = -1;
		// Flag defined to stop looking for the enthalpies of formation when
		// the corresponding table is parsed.
		boolean tableFlag = false;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<sub>c</sub>H&deg;<sub>solid</sub></td><td")) {
				tableFlag = true;
				lineIndex = i;
				if (lineIndex < 0) {
					return null;
				}
				ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
				if (content.size() > 7 && content.contains("c")) {
					if (content.size() > 7 && content.contains("c")) {
						if (content.get(0).trim().contains("c") && content.get(1).trim().contains("H&deg;")
								&& content.get(2).trim().contains("solid")) {
							NISTEnthalpy enthalpy = new NISTEnthalpy();
							info.getEnthalpyOfCombustionInSolid().add(enthalpy);
							// Processes the enthalpy of combustion of a liquid
							// substance to extract the value, units, etc.
							processEnthalpyOfCombustion(content, enthalpy);
						}
					}
				}
			}
			if (tableFlag && body.get(i).contains("</table>")) {
				return enthalpies;
			}
		}
		return enthalpies;
	}

    /**
     * Processes the enthalpy of combustion of a gas, liquid or solid</br>
     * substance to extract, identify and codify them.  
     * 
     * @param content
     * @param enthalpy
     */
    private void processEnthalpyOfCombustion(ArrayList<String> content, NISTEnthalpy enthalpy){
		// Reading the value of enthalpy of combusion
		if(content.get(3).contains("&plusmn;")){
			String[] tokens = content.get(3).split("&plusmn;");
			if(tokens[0].trim().endsWith(".")){
				tokens[0] = tokens[0].trim().concat("0");
			}
			if(tokens[1].trim().endsWith(".")){
				tokens[1] = tokens[1].trim().concat("0");
			}
			if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
				enthalpy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
				enthalpy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE, EnthalpyType.COMBUSTION);
			}
		}else{
			if(isNumeric(content.get(3))){
				enthalpy.setValue(Double.parseDouble(content.get(3)), ValueType.NONE, EnthalpyType.COMBUSTION);
			}
		}
		enthalpy.setUnits(content.get(4));
    	if(!content.get(5).contains("N/A")){
    		enthalpy.setMethod(content.get(5));
    	}
    	if(!content.get(6).contains("N/A")){
    		enthalpy.setReference(content.get(6));
    	}
    	comment = "";
    	for(int k=7;k<content.size();k++){
    		if(!content.get(7).contains("N/A") && !content.get(7).isEmpty()){
    			comment = comment.concat(content.get(k));
    		}
    	}
		if(comment.contains("&nbsp;")){
			comment = comment.replace("&nbsp;", "");
		}
		if(comment.trim().length()>0){
			enthalpy.setComment(comment);
		}
    }
        
    /**
     * Extract the entropy of the current species at standard conditions</br>
     * in the liquid phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected void extractEntropyInLiquidAtStCondition(StringList body) {
        int lineIndex = -1;
        // Flag defined to stop looking for the enthalpies of formation until
    	//  the corresponding table is found.
    	boolean tableFlag = false;
    	for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains(">S&deg;<sub>liquid</sub></td><td")) {
                tableFlag = true; 
            	lineIndex = i;
                if (lineIndex < 0) {
                	return;
                }
                ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
                if (content.size() > 6 && content.contains("S&deg;")) {
                		if (content.get(0).trim().contains("S&deg;") && content.get(1).trim().contains("liquid")) {
                			NISTEntropy entropy = new NISTEntropy();
                			info.getEntropyInLiquidAtStCondition().add(entropy);
                			// Processes the entropy of a liquid substance to extract
                			// the value, units, etc.
                			processEntropyOfSubstanceAtStCondition(content, entropy);
                		}
                }
            }
            if(tableFlag && body.get(i).contains("</table>")){
            	return;
            }
        }
    	return;
    }

    /**
     * Extract the entropy of the current species at standard conditions</br>
     * in the gas-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected void extractEntropyInGasAtStCondition(StringList body) {
        int lineIndex = -1;
        // Flag defined to stop looking for the enthalpies of formation when
    	//  the corresponding table is parsed.
    	boolean tableFlag = false;
    	for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains(">S&deg;<sub>gas,1 bar</sub></td><td") || body.get(i).contains(">S&deg;<sub>gas</sub></td><td")) {
                tableFlag = true; 
            	lineIndex = i;
                if (lineIndex < 0) {
                	return;
                }
                ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
                if (content.size() > 6 && content.contains("S&deg;")) {
                		if (content.get(0).trim().contains("S&deg;") && (content.get(1).trim().contains("gas") || content.get(1).trim().contains("gas,1 bar"))) {
                			NISTEntropy entropy = new NISTEntropy();
                			info.getEntropyInGasAtStCondition().add(entropy);
                			// Processes the entropy of a gaseous substance to extract
                			// the value, units, etc.
                			processEntropyOfSubstanceAtStCondition(content, entropy);
                		}
                }
            }
            if(tableFlag && body.get(i).contains("</table>")){
            	return;
            }
        }
    	return;
    }

    /**
     * Extract the entropy of the current species at standard conditions</br>
     * in the solid-phase from the corresponding HTML file.
     * 
     * @param body
     * @return
     */
    protected void extractEntropyInSolidAtStCondition(StringList body) {
        int lineIndex = -1;
        // Flag defined to stop looking for the enthalpies of formation when
    	//  the corresponding table is parsed.
    	boolean tableFlag = false;
    	for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains(">S&deg;<sub>solid,1 bar</sub></td><td") || body.get(i).contains(">S&deg;<sub>solid</sub></td><td")) {
                tableFlag = true; 
            	lineIndex = i;
                if (lineIndex < 0) {
                	return;
                }
                ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(lineIndex));
                if (content.size() > 6 && content.contains("S&deg;")) {
                		if (content.get(0).trim().contains("S&deg;") && (content.get(1).trim().contains("solid") || content.get(1).trim().contains("solid,1 bar"))) {
                			NISTEntropy entropy = new NISTEntropy();
                			info.getEntropyInSolidAtStCondition().add(entropy);
                			// Processes the entropy of a solid substance to extract
                			// the value, units, etc.
                			processEntropyOfSubstanceAtStCondition(content, entropy);
                		}
                }
            }
            if(tableFlag && body.get(i).contains("</table>")){
            	return;
            }
        }
    	return;
    }

    /**
     * Extract the entropy of a substance (reported by different research</br>
     * groups) of the current species in the gas-phase from the</br>
     * corresponding HTML file.
     * 
     * @param content
     * @param entropy
     */
    private void processEntropyOfSubstanceAtStCondition(List<String> content, NISTEntropy entropy){
		// fusion point temperature
		if(content.get(2).contains("&plusmn;")){
			String[] tokens = content.get(2).split("&plusmn;");
			if(tokens[0].trim().endsWith(".")){
				tokens[0] = tokens[0].trim().concat("0");
			}
			if(tokens[1].trim().endsWith(".")){
				tokens[1] = tokens[1].trim().concat("0");
			}
			if(tokens.length>=2 && isNumeric(tokens[0].trim()) && isNumeric(tokens[1].trim())){
				entropy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1]));
				entropy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
			}
		}else{
			if(isNumeric(content.get(2))){
				entropy.setValue(Double.parseDouble(content.get(2)), ValueType.NONE);
			}
		}
		entropy.setUnits(content.get(3));
    	if(!content.get(4).contains("N/A")){
    		entropy.setMethod(content.get(4));
    	}
    	if(!content.get(5).contains("N/A")){
    		entropy.setReference(content.get(5));
    	}
    	comment = "";
    	for(int k=6;k<content.size();k++){
    		if(!content.get(6).contains("N/A") && !content.get(6).isEmpty()){
    			comment = comment.concat(content.get(k));
    		}
    	}
		if(comment.contains("&nbsp;")){
			comment = comment.replace("&nbsp;", "");
		}
		if(comment.trim().length()>0){
			entropy.setComment(comment);
		}
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
    
    /**
     * Extract appearance energies of the current species.
     * 
     * @param body
     * @return
     */
    protected List<AppearanceEnergy> extractAppearanceEnergy(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("<h3>Appearance energy determinations</h3>")) {
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return null;
        }
        appearanceEnergies = new ArrayList<AppearanceEnergy>();
        unit = "";
        for(int i=lineIndex+1;i<body.size();i++){
        	ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 2) {
				if (content.size() > 0) {
					if (content.get(0).contains("AE")) {
						unit = content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")"));
					}
				}
			}
        	if (content.size() > 5) {
            	appearanceEnergy = new AppearanceEnergy();            	
        		boolean flag = false;
        		String ion ="";
        		int index = 0; // To keep the index of the + symbol of ion.
        		for (int j = 0; j < content.size(); j++) {
        			if(!flag && content.get(j).contains("+")){
        				ion = ion.concat(content.get(j));
        				appearanceEnergy.setIon(ion);
        				index = j;
        				flag = true;
        			} else if(!flag){
        				ion = ion.concat(content.get(j));
        			}
        			
        			if(index!=0){
                    	if(content.get(index+1).contains("&plusmn;")){
                    		appearanceEnergy.setValue(content.get(index+1).replace("&plusmn;", "±"));
                    	}else{
                    		appearanceEnergy.setValue(content.get(index+1));
                    	}
                    	appearanceEnergy.setUnits(unit);
                    	appearanceEnergy.setComment(content.get(content.size()-1));
                    	appearanceEnergy.setReference(content.get(content.size()-2));
                    	appearanceEnergy.setMethod(content.get(content.size()-3));
                    	appearanceEnergies.add(appearanceEnergy);
                    	break;
        			}
                }
            }
            if(body.get(i).contains("</table>")){
            	return appearanceEnergies;
            }
        }
        return null;
    }
 
    /**
     * Extract ionisation energies of the current species.
     * 
     * @param body
     * @return
     */
	protected List<IonisationEnergy> extractIonisationEnergy(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<h3>Ionization energy determinations</h3>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return null;
		}
		ionisationEnergies = new ArrayList<IonisationEnergy>();
		unit = "";
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 0) {
					if (content.get(0).contains("IE")) {
						unit = content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")"));
					}
				}
			}
			if (content.size() > 3) {
				ionisationEnergy = new IonisationEnergy();
				if (content.get(0).contains("&plusmn;")) {
					ionisationEnergy.setValue(content.get(0).replace("&plusmn;", "±"));
				} else {
					ionisationEnergy.setValue(content.get(0));
				}
				ionisationEnergy.setUnits(unit);
				temp = "";
				for(int j=3;j<content.size();j++){
					temp = temp.concat(content.get(j));
				}
				ionisationEnergy.setComment(temp);
				temp = "";
				ionisationEnergy.setReference(content.get(2));
				ionisationEnergy.setMethod(content.get(1));
				ionisationEnergies.add(ionisationEnergy);
			}
			if (body.get(i).contains("</table>")) {
				return ionisationEnergies;
			}
		}
		return null;
	}
    
    /**
     * Extract constant pressure heat capacity of the current species in the gas phase.
     * 
     * @param body
     * @return
     */
	protected List<NISTHeatCapacity> extractHeatCapacityOfGas(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<h3>Constant pressure heat capacity of gas</h3>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return null;
		}
		heatCapacities = new ArrayList<NISTHeatCapacity>();
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 2) {
					if (content.get(0).contains("C") && content.get(1).contains("p,gas")) {
						// Adding the units of heat capacity
						units.add(0,content.get(2).substring(content.get(2).indexOf("(") + 1, content.get(2).indexOf(")")));
					}
				}
			}
			// Processes the heat capacity of a gaseous substance to extract
			// the value, units, etc.
			processHeatCapacity(i, lineIndex, content, heatCapacities);
			if (body.get(i).contains("<hr />")) {
				return heatCapacities;
			}
		}
		return null;
	}	

	/**
     * Extract constant pressure heat capacity of the current species in the liquid phase.
     * 
     * @param body
     * @return
     */
	protected List<NISTHeatCapacity> extractHeatCapacityOfLiquid(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<h3>Constant pressure heat capacity of liquid</h3>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return null;
		}
		heatCapacities = new ArrayList<NISTHeatCapacity>();
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 2) {
					if (content.get(0).contains("C") && content.get(1).contains("p,liquid")) {
						// Adding the units of heat capacity
						units.add(0,content.get(2).substring(content.get(2).indexOf("(") + 1, content.get(2).indexOf(")")));
					}
				}
			}
			// Processes the heat capacity of a liquid substance to extract
			// the value, units, etc.
			processHeatCapacity(i, lineIndex, content, heatCapacities);
			if (body.get(i).contains("</table>")) {
				return heatCapacities;
			}
		}
		return null;
	}	

    /**
     * Extract constant pressure heat capacity of the current species in the solid phase.
     * 
     * @param body
     * @return
     */
	protected List<NISTHeatCapacity> extractHeatCapacityOfSolid(StringList body) {
		int lineIndex = -1;
		for (int i = 0; i < body.size(); i++) {
			if (body.get(i).contains("<h3>Constant pressure heat capacity of solid</h3>")) {
				lineIndex = i;
				break;
			}
		}
		if (lineIndex < 0) {
			return null;
		}
		heatCapacities = new ArrayList<NISTHeatCapacity>();
		units = new ArrayList<String>();
		for (int i = lineIndex + 1; i < body.size(); i++) {
			ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 1) {
				if (content.size() > 2) {
					if (content.get(0).contains("C") && content.get(1).contains("p,solid")) {
						// Adding the units of heat capacity
						units.add(0,content.get(2).substring(content.get(2).indexOf("(") + 1, content.get(2).indexOf(")")));
					}
				}
			}
			// Processes the heat capacity of a solid substance to extract
			// the value, units, etc.
			processHeatCapacity(i, lineIndex, content, heatCapacities);
			if (body.get(i).contains("</table>")) {
				return heatCapacities;
			}
		}
		return null;
	}	

	/**
     * Extract the heat capacities (reported by different research groups)</br>
     * of the current species in the gas-phase from the corresponding HTML file.
     *  
	 * @param i
	 * @param lineIndex
	 * @param content
	 * @param heatCapacities
	 */
	private void processHeatCapacity(int i, int lineIndex, List<String> content, List<NISTHeatCapacity> heatCapacities){
		if (i - (lineIndex + 1) == 2) {
			if (content.size() > 0) {
				if (content.get(0).contains("Temperature")) {
					// Adds the units of temperature.
					units.add(1,content.get(0).substring(content.get(0).indexOf("(") + 1, content.get(0).indexOf(")")));
				}
			}
		}
		if (content.size() > 3 && body.get(i).contains("<tr class")) {
			reference = content.get(2);
			comment = "";
			for(int j=3;j<content.size();j++){
				comment = comment.concat(content.get(j));
			}
		}
		if (content.size() > 1 && body.get(i).contains("<tr class")) {
			heatCapacity = new NISTHeatCapacity();
			temp = content.get(0);
			String[] tokens = temp.split("&plusmn;");
			if (tokens.length >= 1) {
				if (isNISTNumeric(tokens[0].trim())) {
					heatCapacity.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
				}
			}
			if (tokens.length >= 2) {
				if (isNISTNumeric(tokens[1].trim())) {
					heatCapacity.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1].trim()));
				}
			}
			if(units.size()>0){
				heatCapacity.setUnits(units.get(0));
			}
			temperature = new NISTTemperature();
			formatTemperature(temperature, content.get(1));
			if(units.size()>1){
				temperature.setUnits(units.get(1));
			}
			heatCapacity.setTemperature(temperature);
			heatCapacity.setReference(reference);
			heatCapacity.setComment(comment);
			heatCapacities.add(heatCapacity);
		}
	}
	
    /**
     * Extract Henry's Law Constant of the current species.
     * 
     * @param body
     * @return
     */
    public void extractHenrysLawConstant(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("<h3>Henry's Law constant")) {
                solution = body.get(i).substring(body.get(i).indexOf("(") + 1, body.get(i).indexOf(")"));
                lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return;
        }
        boolean flag = false;
        for(int i=lineIndex+1;i<body.size();i++){
        	ArrayList<String> content = NISTHTMLReaderHelper.extractContent(body.get(i));
			if (i - (lineIndex + 1) == 2) {
				if (content.size() > 1) {
					if (content.get(0).contains("k&deg;")) {
						units = new ArrayList<String>();
						// this is the units of Henry's Law Constant for solubility at a given temperature.
						String unit = content.get(2).substring(content.get(2).indexOf("(") + 1, content.get(2).indexOf(")"));
						units.add(unit);
						if(content.get(2).contains("at")){
							String temp = content.get(2).substring(content.get(2).lastIndexOf("at")).trim();
							String[] tokens = temp.split(" ");
							if(tokens.length>2){
								if(isNumeric(tokens[1])){
									flag = true;
									temperature = new NISTTemperature();
									formatTemperature(temperature, tokens[1]);
									temperature.setUnits(tokens[2]);
								}
								units.add(tokens[2]);
							}
						}
						
					}
				}
			}
        	if (i - (lineIndex + 1)>3 && content.size() >= 5) {
            	henrysLawConstant = new NISTHenrysLawConstant();
        		info.getHenrysLawConstant().add(henrysLawConstant);
				if (flag && temperature != null) {
					henrysLawConstant.setTemperature(temperature);
					temp = content.get(0);
					if (temp.endsWith(".")) {
						temp = temp.concat("0");
					}
					if (isNISTNumeric(temp)) {
						henrysLawConstant.setValue(Double.parseDouble(temp));
						if (units.size() > 0) {
							henrysLawConstant.setUnits(units.get(0));
						}
					}
					temp = content.get(1);
					if (isNISTNumeric(temp)) {
						henrysLawConstant.setTemperatureDependentConstant(Double.parseDouble(temp));
					}
					temp = content.get(2);
					if (!(temp.contains("N/A") || temp.contains("&nbsp;"))) {
						henrysLawConstant.setMethod(temp);
					}
					temp = content.get(3);
					if (!(temp.contains("N/A") || temp.contains("&nbsp;"))) {
						henrysLawConstant.setMethod(temp);
					}
					temp = content.get(4);
					if (!(temp.contains("N/A") || temp.contains("&nbsp;"))) {
						for (int j = 5; j < content.size(); j++) {
							temp = temp.concat(content.get(j));
						}
						henrysLawConstant.setReference(temp);
					}
					if(solution!=null && !solution.isEmpty()){
						henrysLawConstant.setSolution(solution);
					}
				}
            }
            if(body.get(i).contains("</table>")){
            	return;
            }
        }
        return;
    }
 
    /**
     * Extract the enthalpy of reaction of the current species.
     * 
     * @param body
     * @return
     */
    public void extractEnthalpyOfReaction(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("By formula: <span")) {
            	content = NISTHTMLReaderHelper.extractContent(body.get(i));
            	reaction = "";
            	for(int j=1; j<content.size();j++){
            		if(content.get(j).contains("&nbsp;")){
            			reaction = reaction.concat(content.get(j).replace("&nbsp;", " "));
            		}else{
            			reaction = reaction.concat(content.get(j));
            		}
            	}
            	lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return;
        }
		for (int i = lineIndex + 10; i < body.size(); i++) {
			if (body.get(i).contains("<sub>r</sub>H&deg;</td><td")) {
				content = NISTHTMLReaderHelper.extractContent(body.get(i));
				if (content.size() >= 6) {
					enthalpy = new NISTEnthalpy();
					info.getEnthalpyOfReactions().add(enthalpy);
					temp = content.get(2);
					String[] tokens = temp.split("&plusmn;");
					if (tokens.length >= 1) {
						if (isNISTNumeric(tokens[0].trim())) {
							enthalpy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE, EnthalpyType.REACTION);
						}
					}
					if (tokens.length >= 2) {
						if (isNISTNumeric(tokens[1].trim())) {
							enthalpy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1].trim()));
						}
					}
					if (!content.get(3).contains("N/A")) {
						enthalpy.setUnits(content.get(3));
					}
					if (!content.get(4).contains("N/A")) {
						enthalpy.setMethod(content.get(4));
					}
					if (!content.get(5).contains("N/A")) {
						enthalpy.setReference(content.get(5));
					}
					if (!content.get(6).contains("N/A")) {
						temp = "";
						for (int j = 6; j < content.size(); j++) {
							temp = temp.concat(content.get(j));
						}
						enthalpy.setComment(temp);
					}
					if (reaction.length() > 0) {
						enthalpy.setReaction(reaction);
					}
				}
			}
			if (body.get(i).contains("<h2 id=\"IR-Spec\">IR Spectrum</h2>")) {
				return;
			}
		}
        return;
    }
    
    /**
     * Extract the entropy of reaction of the current species.
     * 
     * @param body
     * @return
     */
    public void extractEntropyOfReaction(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("By formula: <span")) {
            	content = NISTHTMLReaderHelper.extractContent(body.get(i));
            	reaction = "";
            	for(int j=1; j<content.size();j++){
            		if(content.get(j).contains("&nbsp;")){
            			reaction = reaction.concat(content.get(j).replace("&nbsp;", " "));
            		}else{
            			reaction = reaction.concat(content.get(j));
            		}
            	}
            	lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return;
        }
		for (int i = lineIndex + 10; i < body.size(); i++) {
			if (body.get(i).contains("<sub>r</sub>S&deg;</td><td")) {
				content = NISTHTMLReaderHelper.extractContent(body.get(i));
				if (content.size() >= 6) {
					entropy = new NISTEntropy();
					info.getEntropyOfReactions().add(entropy);
					temp = content.get(2);
					String[] tokens = temp.split("&plusmn;");
					if (tokens.length >= 1) {
						if (isNISTNumeric(tokens[0].trim())) {
							entropy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
						}
					}
					if (tokens.length >= 2) {
						if (isNISTNumeric(tokens[1].trim())) {
							entropy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1].trim()));
						}
					}
					if (!content.get(3).contains("N/A")) {
						entropy.setUnits(content.get(3));
					}
					if (!content.get(4).contains("N/A")) {
						entropy.setMethod(content.get(4));
					}
					if (!content.get(5).contains("N/A")) {
						entropy.setReference(content.get(5));
					}
					if (!content.get(6).contains("N/A")) {
						temp = "";
						for (int j = 6; j < content.size(); j++) {
							temp = temp.concat(content.get(j));
						}
						entropy.setComment(temp);
					}
					if (reaction.length() > 0) {
						entropy.setReaction(reaction);
					}
				}
			}
			if (body.get(i).contains("<h2 id=\"IR-Spec\">IR Spectrum</h2>")) {
				return;
			}
		}
        return;
    }
 
    /**
     * Extract the Gibb's Free energy of reaction of the current species.
     * 
     * @param body
     * @return
     */
    public void extractGibbsFreeEnergyOfReaction(StringList body) {
        int lineIndex = -1;
        for (int i = 0; i < body.size(); i++) {
            if (body.get(i).contains("By formula: <span")) {
            	content = NISTHTMLReaderHelper.extractContent(body.get(i));
            	reaction = "";
            	for(int j=1; j<content.size();j++){
            		if(content.get(j).contains("&nbsp;")){
            			reaction = reaction.concat(content.get(j).replace("&nbsp;", " "));
            		}else{
            			reaction = reaction.concat(content.get(j));
            		}
            	}
            	lineIndex = i;
                break;
            }
        }
        if (lineIndex < 0) {
            return;
        }
		for (int i = lineIndex + 10; i < body.size(); i++) {
			if (body.get(i).contains("<sub>r</sub>G&deg;</td><td")) {
				content = NISTHTMLReaderHelper.extractContent(body.get(i));
				if (content.size() >= 6) {
					gibbsFreeEnergy = new NISTGibbsFreeEnergy();
					info.getGibbsFreeEnergyOfReactions().add(gibbsFreeEnergy);
					temp = content.get(2);
					String[] tokens = temp.split("&plusmn;");
					if (tokens.length >= 1) {
						if (isNISTNumeric(tokens[0].trim())) {
							gibbsFreeEnergy.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
						}
					}
					if (tokens.length >= 2) {
						if (isNISTNumeric(tokens[1].trim())) {
							gibbsFreeEnergy.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1].trim()));
						}
					}
					if (!content.get(3).contains("N/A")) {
						gibbsFreeEnergy.setUnits(content.get(3));
					}
					if (!content.get(4).contains("N/A")) {
						gibbsFreeEnergy.setMethod(content.get(4));
					}
					if (!content.get(5).contains("N/A")) {
						gibbsFreeEnergy.setReference(content.get(5));
					}
					if (!content.get(6).contains("N/A")) {
						temp = "";
						for (int j = 6; j < content.size(); j++) {
							temp = temp.concat(content.get(j));
						}
						gibbsFreeEnergy.setComment(temp);
					}
					if (reaction.length() > 0) {
						gibbsFreeEnergy.setReaction(reaction);
					}
				}
			}
			if (body.get(i).contains("<h2 id=\"IR-Spec\">IR Spectrum</h2>")) {
				return;
			}
		}
        return;
    }
    
    /**
     * Format the temperature as a number.
     * 
     * @param temperature
     * @param temp
     */
    public void formatTemperature(NISTTemperature temperature, String temp){
		String[] tokens = temp.split("&plusmn;");
		if (tokens.length >= 1) {
			if (isNISTNumeric(tokens[0].trim())) {
				temperature.setValue(Double.parseDouble(tokens[0].trim()), ValueType.NONE);
			}
		}
		if (tokens.length >= 2) {
			if (isNISTNumeric(tokens[1].trim())) {
				temperature.setTolerance(Double.parseDouble(tokens[1]), Double.parseDouble(tokens[1].trim()));
			}
		}
    }
    
    public static boolean isNumeric(String str) {
    	// Matches a number with optional '-' and decimal.
    	return str.matches("-?\\d+(\\.\\d+)?");
    }
    
    public static boolean isNISTNumeric(String str){
    	// Matches a number with optional '-' and decimal or no digit after dot.
        return str.matches("-?\\d+(\\.\\d*)?");
    }
}