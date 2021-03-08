/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.MessageFormat;

import org.apache.log4j.Logger;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesId;

/**
 *
 * @author pb556
 */
public class NISTSpeciesListParser {

    protected String path = "";
    protected String seperator = "\t";
    protected NISTSpeciesList speciesList = new NISTSpeciesList();
    private static Logger logger = Logger.getLogger(NISTSpeciesListParser.class);

    public NISTSpeciesListParser() {
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getPath() {
        return path;
    }

    public void parse() throws FileNotFoundException, IOException {
        
    	// creates the species list
        speciesList = new NISTSpeciesList();
        BufferedReader br = new BufferedReader(new FileReader(new File(path)));
        String line;
        
        int lineCtr = 0;
        int errCtr = 0;
        
        while ((line = br.readLine()) != null) {
        	
            lineCtr++;
            // process the line.
            String[] items = line.split(seperator);
        
            if (items.length != 3) {
                logger.warn(MessageFormat.format("Invalid set in line {0} identified", lineCtr));
                errCtr++;
                continue;
            }
            
            speciesList.add(new NISTSpeciesId(items[0].trim(), items[1].trim(), items[2].trim()));
        }
        
        br.close();
        
        if (errCtr > 0) {
        	
            logger.warn(MessageFormat.format("A total of {0} line(s) were identified", errCtr));
            
        }
    }

    public NISTSpeciesList getNISTSpeciesList() {
        return speciesList;
    }
}
