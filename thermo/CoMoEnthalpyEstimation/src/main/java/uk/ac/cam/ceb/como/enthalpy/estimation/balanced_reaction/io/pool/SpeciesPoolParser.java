/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 * 
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.log4j.Logger;

import au.com.bytecode.opencsv.CSVReader;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 * 
 */
public class SpeciesPoolParser extends CSVParser {

    private Logger logger = Logger.getLogger(getClass());

    public SpeciesPoolParser() {
    	
        super();
    
    }

    public SpeciesPoolParser(File file) throws Exception {
    	
    super(file);
        
    }

    @Override
    public void parse() throws FileNotFoundException, IOException {
    	
    refSpecies = new LinkedHashSet<>();        
    speciesOfInterest = new LinkedHashSet<>();
        
    CSVReader reader = new CSVReader(new FileReader(f)); //use comma as a separator
        
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
                        
//                        System.out.println("inside public void parse(), SpeciesPoolParser: s.getTotalEnergy(): " + s.getTotalEnergy());
                                              
                        parseMassBalance(s, line[3]);
                        
                        parseBonds(s, line[4]);
                        
                        if (line.length == 6) {
                        	
                            speciesFlags.put(s, line[5].trim());
                            
                        }
                        
                        speciesOfInterest.add(s);
                        
                    } else {
                    	
                        Species s = new Species(line[0].trim(), Double.parseDouble(line[2]), Double.parseDouble(line[1]));
                        
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
    public void clear() throws Exception {
    	/**
    	 * Here we use HashSet<>() - > LinkedHashSet<Species>();
    	 */
        refSpecies = new LinkedHashSet<>();
        speciesOfInterest = new LinkedHashSet<>();
        f = null;
    }

    @Override
    public Set<Species> get() throws Exception {
        return getAllSpecies();
    }
}