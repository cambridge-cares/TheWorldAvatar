/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import org.cam.ceb.como.enthalpy.calculator.io.SpeciesInterpreter;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.io.Parser;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.tools.util.StringUtil;

/**
 *
 * @author pb556
 */
public class ResultsParser extends Parser {

    protected ResultsDataList data = new ResultsDataList();
    protected SpeciesInterpreter spInterpreter = null;
    private Logger logger = Logger.getLogger(getClass());

    public ResultsParser() {
        super();
    }

    public ResultsParser(Collection<Species> pool) {
        super();
        spInterpreter = new SpeciesInterpreter(pool);
    }
    
    public ResultsParser(File file) {
        super(file);
    }

    public ResultsParser(File file, Collection<Species> pool) {
        super(file);
        spInterpreter = new SpeciesInterpreter(pool);
    }
    
    public ResultsParser(String path) {
        super(new File(path));
    }

    public ResultsParser(String path, Collection<Species> pool) {
        super(new File(path));
        spInterpreter = new SpeciesInterpreter(pool);
    }

    @Override
    public void parse() throws FileNotFoundException, IOException {
        if (file == null) {
            logger.error("File cannot be read. No path is defined.", new IOException("No path is defined."));
        }
        BufferedReader br = new BufferedReader(new FileReader(file));
        String line;
        int lineNum = 0;
        while ((line = br.readLine()) != null) {
            // process the line
            lineNum++;
            if (line.isEmpty()) {
                continue;
            }
            ResultsData d = parse(line, lineNum);
            if (d != null) {
                data.add(d);
            } else {
                logger.warn("Invalid line identified (line " + lineNum + "): Invalid definition of the reaction");
            }
        }
        br.close();
    }

    protected ResultsData parse(String line, int num) {
        String[] items = line.trim().split("\"\\s*,\\s*\"");
        if (items.length == 6) {
            // pre-process the items
            for (int i = 0; i < items.length; i++) {
                items[i] = items[i].trim();
                items[i] = items[i].replace("\"", "");
//                int start = items[i].indexOf('"');
//                int end = items[i].lastIndexOf('"');
//                items[i] = items[i].substring(start + 1, end);
            }
            
            // species
            Species sp = new Species(items[1], Double.parseDouble(items[3]), Double.parseDouble(items[2]));
            if (spInterpreter != null) {
                sp = spInterpreter.get(items[1]);
                if (sp == null) {
                    logger.warn("Impossible to identify species (line " + num + "): Invalid definition of the target species");
                    sp = new Species(items[1], Double.parseDouble(items[3]), Double.parseDouble(items[2]));
                }
                sp.setTotalEnergy(Double.parseDouble(items[2]));
                sp.setHf(Double.parseDouble(items[3]));
            }
            return new ResultsData(
                    items[0],
                    sp,  
                    parseValueDistributions(items[4], num),
                    items[5]);
        } else {
            logger.warn("Invalid line identified (line " + num + "): Invalid definition of the target species and the reaction");
        }
        return null;
    }
    
    protected ResultsValueDistributionList parseValueDistributions(String dist, int num) {
        dist = dist.substring(dist.indexOf("{") + 1, dist.lastIndexOf("}"));
        char opening = '[';
        char closing = ']';
        String buffer = "";
        
        // pre-processing of the string
        ArrayList<String> ds = new ArrayList<String>();
        for (int i = 0; i < dist.length(); i++) {
            if (dist.charAt(i) == opening) {
                if (!buffer.trim().isEmpty()) {
                    ds.add(buffer);
                    buffer = "";
                    continue;
                }
            } else if (dist.charAt(i) == closing) {
                if (!buffer.trim().isEmpty()) {
                    ds.add(buffer);
                    buffer = "";
                    continue;
                }
            } else {
                buffer += dist.charAt(i);
            }
        }
        if (!buffer.isEmpty()) {
            ds.add(buffer);
        }
        
        ResultsValueDistributionList list = new ResultsValueDistributionList();
        for (String d : ds) {
            ResultsValueDistribution rd = new ResultsValueDistribution();
            String[] items = d.split(",");
            for (int i = 0; i < items.length; i++) {
                items[i] = items[i].trim();
                if (!StringUtil.isNumber(items[i])) {
                    logger.warn("Invalid data identified (line " + num + "): Values from the distribution will be excluded");
                } else {
                    rd.add(Double.parseDouble(items[i]));
                }
            }
            list.add(rd);
        }
        return list;
    }

    protected HashMap<String, Double> extractSpecies(String s) {
        HashMap<String, Double> species = new HashMap<String, Double>();
        String[] items = s.split("\\+");
        if (items.length >= 1) {
            for (int i = 0; i < items.length; i++) {
                String[] subItems = items[i].split("x");
                if (subItems.length == 2) {
                    species.put(subItems[1].trim().replace("\"", ""), Double.parseDouble(subItems[0].trim()));
                } else {
                    logger.warn("Invalid species data identified (" + s + ")");
                    return null;
                }
            }
        }
        return species;
    }

    @Override
    public Object get() {
        return data;
    }
}
 