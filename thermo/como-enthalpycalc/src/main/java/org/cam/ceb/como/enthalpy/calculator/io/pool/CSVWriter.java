/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.pool;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.species.Bond;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.io.Writer;
import org.cam.ceb.como.tools.periodictable.Element;

/**
 *
 * @author pb556
 */
public abstract class CSVWriter extends Writer {

    protected ArrayList<String[]> content = null;

    public CSVWriter() {
        super();
    }

    public CSVWriter(String path) {
        super(path);
    }

    public CSVWriter(File file) {
        super(file);
    }
    
    public void set(Map<Species, String> species, boolean spOfInterest) {
        content = new ArrayList<String[]>();
        for (Species s : species.keySet()) {
            String[] val = new String[6];
            val[0] = s.getRef().toString(); // id
            val[1] = String.valueOf(s.getTotalEnergy()); // total energy  
            if (spOfInterest) {
                val[2] = "?";
            } else {
                val[2] = String.valueOf(s.getHf()); // hf
            }
            val[3] = getMassBalanceStr(s); // mass balance
            val[4] = getBondTypeConservationStr(s); // bond type conservation
            val[5] = species.get(s); // bond type conservation
            content.add(val);
        }
    }

    public void set(Collection<Species> species, boolean spOfInterest) {
        content = new ArrayList<String[]>();
        for (Species s : species) {
            String[] val = new String[5];
            val[0] = s.getRef().toString(); // id
            val[1] = String.valueOf(s.getTotalEnergy()); // total energy  
            if (spOfInterest) {
                val[2] = "?";
            } else {
                val[2] = String.valueOf(s.getHf()); // hf
            }
            val[3] = getMassBalanceStr(s); // mass balance
            val[4] = getBondTypeConservationStr(s); // bond type conservation
            content.add(val);
        }
    }

    public void set(Species species, String flag, boolean spOfInterest) {
        HashMap<Species, String> s = new HashMap<Species, String>();
        s.put(species, flag);
        set(s, spOfInterest);
    }
    
    public void set(Species species, boolean spOfInterest) {
        List<Species> s = new ArrayList<Species>();
        s.add(species);
        set(s, spOfInterest);
    }

    protected String getMassBalanceStr(Species species) {
        //{[C:C1][O:O1][H:H1,H2,H3,H4]}
        String mb = "{";
        for (Element atom : species.getAtomMultiset().elementSet()) {
            mb += "[" + atom.getSymbol() + ":"; // + species.getAtomMultiset().count(atom) + "]";
            ArrayList<String> refs = new ArrayList<String>();
            for (String ref : species.getAtomMap().keySet()) {
                if (species.getAtomMap().get(ref).equals(atom.getSymbol())) {
                    refs.add(ref);
                }
            }
            if (refs.size() > 0) {
                mb += refs.get(0);
                for (int i = 1; i < refs.size(); i++) {
                    mb += "," + refs.get(i);
                }
                mb += "]";
            }
        }
        return mb + "}";
    }

    protected String getBondTypeConservationStr(Species species) {
        //"{[[C1]{1}[H1]][[C1]{1}[H2]][[C1]{1}[H3]][[C1]{1}[O1]][[H4]{1}[O1],1]}"
        String bt = "{";
        for (Bond bond : species.getBondMap()) {
            //String strBond = bond.toString();
            bt += "[" + bond.toString() + "]";
        }
        return bt + "}";
    }
}