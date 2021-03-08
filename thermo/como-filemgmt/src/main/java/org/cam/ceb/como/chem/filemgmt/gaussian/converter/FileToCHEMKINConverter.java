/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.converter;

import gigadot.chom.chem.structure.Compound;
import java.io.File;
import java.util.Map;
import org.apache.log4j.Logger;
/**
 * commented by nk510 on 10-01-2019.
 * 
import org.cam.ceb.como.thermo.calculator.StatMechThermoCalculator;
import org.cam.ceb.como.thermo.converter.NASAConverter;
**/

import org.cam.ceb.como.tools.structure.util.description.IDBuilder;

/**
 *
 * @author pb556
 */
public abstract class FileToCHEMKINConverter {
 /**
  * commented by nk510 on 10-01-2019.
  *    
    protected StatMechThermoCalculator thermoCal = null;
    protected File src = null;
    protected File dest = null;
    protected Logger logger = Logger.getLogger(this.getClass());

    public FileToCHEMKINConverter(String src, String dest, StatMechThermoCalculator thermoCal) {
        this.src = new File(src);
        this.dest = new File(dest);
        this.thermoCal = thermoCal;
    }

    public FileToCHEMKINConverter(File src, File dest, StatMechThermoCalculator thermoCal) {
        this.src = src;
        this.dest = dest;
        this.thermoCal = thermoCal;
    }

    public void convertToCantera() throws Exception {
        NASAConverter nasab = new NASAConverter(thermoCal);
        //nasab.setSpeciesNameGenerator(new SMILESBuilder());
        nasab.setSpeciesNameGenerator(new IDBuilder());
        //nasab.setInput(src.getAbsolutePath(), validExtension);
        nasab.writeNASA(parseDirectory(), dest + File.separator + "therm.dat", "cantera"); // "cantera" or "chemkin"
    }

    public void convertToChemKin() throws Exception {
        NASAConverter nasab = new NASAConverter(thermoCal);
        //nasab.setSpeciesNameGenerator(new SMILESBuilder());
        nasab.setSpeciesNameGenerator(new IDBuilder());
        //nasab.setInput(src.getAbsolutePath(), validExtension);
        nasab.writeNASA(parseDirectory(), dest + File.separator + "therm.dat", "chemkin"); // "cantera" or "chemkin"
    }
**/
    protected abstract Map<String, Compound> parseDirectory() throws Exception;
}
