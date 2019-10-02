/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.pool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.species.Species;

import au.com.bytecode.opencsv.CSVReader;

/**
 *
 * @author pb556
 */
public class SpeciesPoolParser extends CSVParser {

    private Logger logger = Logger.getLogger(getClass());

    public SpeciesPoolParser() {
        super();
    }

    public SpeciesPoolParser(File file) {
        super(file);
    }

    @Override
    public void parse() throws FileNotFoundException, IOException {
        refSpecies = new HashSet<Species>();
        speciesOfInterest = new HashSet<Species>();
        
        CSVReader reader = new CSVReader(new FileReader(file)); //use comma as a separator

        String[] line;
        int ctrLine = 1;

        // parse body
        while ((line = reader.readNext()) != null) {
            if (line.length != 5 && line.length != 6) {
                logger.warn("Line " + ctrLine + " is invalid and is ignored for further processing!");
            } else {
                try {
                    if (line[2].trim().equals("?")) {
                        
                    	Species s = new Species(line[0].trim());
                        
                        s.setTotalEnergy(Double.parseDouble(line[1]));
                        
                        parseMassBalance(s, line[3]);
                        
                        parseBonds(s, line[4]);
                        
                       System.out.println("line 1: " + line[1] + " line[2] :" + line[2] + " line[3] " + line[3] + " line[4]: " +line[4] + " line[5]" +line [5]);
                        
//                        logger.warn("total energy: " + Double.parseDouble(line[1]) + " " + "mass balance: " + line[3] + " bond: " + line[4]);
                        
                        if (line.length == 6) {
                            speciesFlags.put(s, line[5].trim());
                        }
                        speciesOfInterest.add(s);
                    } else {
                        Species s = new Species(line[0].trim(),Double.parseDouble(line[2]),Double.parseDouble(line[1]));
                        
                        parseMassBalance(s, line[3]);
                        parseBonds(s, line[4]);
                        
                        if (line.length == 6) {
                        	
                            speciesFlags.put(s, line[5].trim());
                            
                        }
                        
                        refSpecies.add(s);
                        
                    }
                    
                } catch (NumberFormatException nfe) {
                	
                    logger.warn("Line " + ctrLine + " contains invalid data and is ignored for further processing!");
                    
                } catch (Exception e) {
                	
                    logger.warn("Line " + ctrLine + " contains invalid data and is ignored for further processing: " + e.getMessage());
                    
                }
            }
            
            ctrLine++;
        }
    }

    @Override
    public Object get() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}