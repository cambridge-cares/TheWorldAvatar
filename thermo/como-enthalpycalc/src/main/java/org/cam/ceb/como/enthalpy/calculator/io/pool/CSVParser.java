/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.pool;

import org.cam.ceb.como.enthalpy.calculator.io.Parser;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.species.BondType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;

/**
 *
 * @author pb556
 */
public abstract class CSVParser extends Parser {
    private Logger logger = Logger.getLogger(getClass());
    protected HashSet<Species> refSpecies = new HashSet<Species>();
    protected HashSet<Species> speciesOfInterest = new HashSet<Species>();
    protected HashMap<Species, String> speciesFlags = new HashMap<Species, String>();

    public CSVParser() {
        super();
    }

    public CSVParser(File file) {
        super(file);
    }
    
    public Set<Species> getAllSpecies() {
        HashSet<Species> species = new HashSet<Species>();
        species.addAll(refSpecies);
        species.addAll(speciesOfInterest);
        return species;
    }
    
    public Set<Species> getRefSpecies(String validFlag) {
        return get(refSpecies, validFlag);
    }

    public Set<Species> getSpeciesOfInterest(String validFlag) {
        return get(speciesOfInterest, validFlag);
    }
    
    public Set<Species> getRefSpecies(Collection<String> validFlag) {
        HashSet<Species> valid = new HashSet<Species>();
        for (String flag : validFlag) {
            valid.addAll(get(refSpecies, flag));
        }
        return valid;
    }

    public Set<Species> getSpeciesOfInterest(Collection<String> validFlag) {
                HashSet<Species> valid = new HashSet<Species>();
        for (String flag : validFlag) {
            valid.addAll(get(speciesOfInterest, flag));
        }
        return valid;
    }

    public Set<Species> getRefSpecies() {
        return refSpecies;
    }

    public Set<Species> getSpeciesOfInterest() {
        return speciesOfInterest;
    }
    
    public Map<Species, String> getFlags() {
        return speciesFlags;
    }

    protected String removeChars(String s, char[] c) {
        String buffer = "";
        for (int i = 0; i < s.length(); i++) {
            boolean remove = false;
            for (int j = 0; j < c.length; j++) {
                if (s.charAt(i) == c[j]) {
                    remove = true;
                }
            }
            if (!remove) {
                buffer = buffer + s.charAt(i);
            }
        }
        return buffer;
    }

    // need to be adjusted!
    protected void parseMassBalance(Species s, String mB) {
        // "{[C:C1][O:O1][H:H1,H2,H3,H4]}"
        mB = removeChars(mB, new char[]{'{', '}'});
        String[] items = mB.split("]");
        for (String item : items) {
            item = removeChars(item, new char[]{'[', ']'});
            String[] subItems = item.split(":");
            if (subItems.length != 2) {
                logger.error("Invalid datatype present in MassBalance column!", new Exception("Invalid datatype!"));
            } else {
                String[] refs = subItems[1].split(",");
                for (int i = 0; i < refs.length; i++) {
                    if (!refs[i].isEmpty()) {
                        s.addAtom(refs[i].trim(), subItems[0].trim());
                    }
                }
            }
        }
    }
    
    protected void parseBonds(Species s, String bT) {
        // "{[[C1]{1}[H1]][[C1]{1}[H2]][[C1]{1}[H3]][[C1]{1}[O1]][[H4]{1}[O1],1]}"
        bT = removeChars(bT, new char[] {'{','}'});
        ArrayList<String> items = new ArrayList<String>();
        String buffer = "";
        int ctr = 0;
        for (int i = 0; i < bT.length(); i++) {
            if (ctr == 0 && !buffer.isEmpty()) {
                items.add(buffer);
                buffer = "";
            }
            if (bT.charAt(i) == '[') {
                ctr++;
            } else if (bT.charAt(i) == ']') {
                ctr--;
            }
            buffer = buffer + bT.charAt(i);
        }
        if (!buffer.isEmpty()) {
            items.add(buffer);
        }
        if (items.isEmpty()) {
            logger.error("Invalid datatype present in BondTypeConservation column!", new Exception("Invalid datatype!"));
        }

        Pattern p = Pattern.compile("\\[\\[(\\w+)\\](\\d+)\\[(\\w+)\\]\\].*");
        for (String item : items) {
            //item = item.replace("[", "").replace("", "]");
            Matcher m = p.matcher(item);
            if (m.find()) {
                try {
                    BondType b = getBondType(Integer.parseInt(m.group(2)));
                    if (b == null) {
                        throw new Exception("Invalid datatype present in BondTypeConservation column!");
                    }
                    s.addBond(b, m.group(1).trim(), m.group(3).trim());
                } catch (Exception e) {
                    logger.error("Invalid datatype present in BondTypeConservation column!", e);
                }
            } else {
                logger.error("Invalid datatype present in BondTypeConservation column!", new Exception("Invalid datatype!"));
            }
        }
    }
    
    protected BondType getBondType(Integer bT) {
        if (bT == BondType.SINGLE.getValue()) {
            return BondType.SINGLE;
        } else if (bT == BondType.DOUBLE.getValue()) {
            return BondType.DOUBLE;
        } else if (bT == BondType.TRIPLE.getValue()) {
            return BondType.TRIPLE;
        } else if (bT == BondType.AROMATIC.getValue()) {
            return BondType.AROMATIC;
        }
        return null;
    }
    
    protected HashSet<Species> get(HashSet<Species> pool, String flag) {
        HashSet<Species> species = new HashSet<Species>();
        if (pool != null && !pool.isEmpty() && speciesFlags != null && !speciesFlags.isEmpty()) {
            ArrayList<Species> valid = new ArrayList<Species>();
            for (Species s : speciesFlags.keySet()) {
                if (speciesFlags.get(s).trim().equals(flag.trim())) {
                    valid.add(s);
                }
            }
            for (Species s1 : pool) {
                for (Species s2 : valid) {
                    if (s1.equals(s2, true)) {
                        species.add(s1);
                        break;
                    }
                }
            }
        }
        return species;
    }
    
//    protected void parseMassBalance(Species s, String mB) {
//        // {[C,1][O,1][H,4]}
//        mB = removeChars(mB, new char[] {'{','}'});
//        String[] items = mB.split("]");
//        for (String item : items) {
//            item = removeChars(item, new char[] {'[', ']'});
//            String[] subItems = item.split(",");
//            if (subItems.length != 2) {
//                logger.error("Invalid datatype present in MassBalance column!", new Exception("Invalid datatype!"));
//            } else {
//                for (int i = 0; i < Integer.parseInt(subItems[1]); i++) {
//                    s.addAtom(subItems[0].trim());
//                }
//            }
//        }
//    }
}
