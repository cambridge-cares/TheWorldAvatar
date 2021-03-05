/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.converter;

import java.io.File;
import java.util.Map;

import org.apache.log4j.Logger;
/**
 * commented by nk510, 
import org.cam.ceb.como.thermo.calculator.StatMechThermoCalculator;

**/
import org.cam.ceb.como.tools.structure.util.description.CompoundDescriptionBuilderIntf;
import org.cam.ceb.como.tools.structure.util.description.SMILESBuilder;

import gigadot.chom.chem.structure.Compound;
import gigadot.chom.thermo.calculator.StatMechThermoCalculator;

/**
 *
 * @author pb556
 */
public abstract class FileToNASAConverter {

 
    protected StatMechThermoCalculator thermoCal = null;
    protected File src = null;
    protected File dest = null;
    protected CompoundDescriptionBuilderIntf descrBuilder = new SMILESBuilder();
    protected Logger logger = Logger.getLogger(this.getClass());

    public FileToNASAConverter(String src, String dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder) {
        this.src = new File(src);
        this.dest = new File(dest);
        this.thermoCal = thermoCal;
        this.descrBuilder = descrBuilder;
    }

    public FileToNASAConverter(File src, File dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder) {
        this.src = src;
        this.dest = dest;
        this.thermoCal = thermoCal;
        this.descrBuilder = descrBuilder;
    }

    public void convertToCantera() throws Exception {
//        NASAConverter nasab = new NASAConverter(thermoCal);
//        nasab.setSpeciesNameGenerator(descrBuilder);
        //nasab.setSpeciesNameGenerator(new IDBuilder());
        //nasab.setInput(src.getAbsolutePath(), validExtension);
//        nasab.writeNASA(parseDirectory(), dest + File.separator + "therm.cti", "cantera"); // "cantera" or "chemkin"
    }

    public void convertToChemKin() throws Exception {
//        NASAConverter nasab = new NASAConverter(thermoCal);
//        nasab.setSpeciesNameGenerator(descrBuilder);
        //nasab.setSpeciesNameGenerator(new IDBuilder());
        //nasab.setInput(src.getAbsolutePath(), validExtension);
//        nasab.writeNASA(parseDirectory(), dest + File.separator + "therm.dat", "chemkin"); // "cantera" or "chemkin"
    }

    protected abstract Map<String, Compound> parseDirectory() throws Exception;
}
