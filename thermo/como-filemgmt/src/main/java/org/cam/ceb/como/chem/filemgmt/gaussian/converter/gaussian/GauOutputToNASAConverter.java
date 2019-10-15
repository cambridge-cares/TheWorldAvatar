/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.converter.gaussian;

import java.io.File;

import org.cam.ceb.como.chem.filemgmt.gaussian.converter.FileToNASAConverter;
/**
 * commented by nk510 on 10-01-2019.
import org.cam.ceb.como.thermo.calculator.StatMechThermoCalculator;
**/
import org.cam.ceb.como.tools.structure.util.description.CompoundDescriptionBuilderIntf;

import gigadot.chom.thermo.calculator.StatMechThermoCalculator;
/**
 *
 * @author pb556
 */
public abstract class GauOutputToNASAConverter extends FileToNASAConverter {

	public GauOutputToNASAConverter(File src, File dest, StatMechThermoCalculator thermoCal,
			CompoundDescriptionBuilderIntf descrBuilder) {
		super(src, dest, thermoCal, descrBuilder);
		// TODO Auto-generated constructor stub
	}
	
/**
public abstract class GauOutputToNASAConverter extends FileToNASAConverter {

    protected Map<String, Double> hfData = null;
    protected Map<String, Map<Bond, Double>> internalRotVibs = null;
    protected FileFilter fFilter;
    private Logger logger = Logger.getLogger(getClass().getName());
    
    public GauOutputToNASAConverter(String src, String dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder) {
       super(src, dest, thermoCal, descrBuilder);
    }
    
    public GauOutputToNASAConverter(File src, File dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder) {
        super(src, dest, thermoCal, descrBuilder);
    }
    
    // ROTOR DATA HAVE TO BE ADDED AS WELL!
    public GauOutputToNASAConverter(String src, String dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder, Map<String, Double> hfData) {
       super(src, dest, thermoCal, descrBuilder);
       this.hfData = hfData;
    }
    
    public GauOutputToNASAConverter(File src, File dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder, Map<String, Double> hfData) {
        super(src, dest, thermoCal, descrBuilder);
        this.hfData = hfData;
    }
    
    public GauOutputToNASAConverter(String src, String dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder, Map<String, Double> hfData, Map<String, Map<Bond, Double>> internalRotVibs) {
       super(src, dest, thermoCal, descrBuilder);
       this.hfData = hfData;
       this.internalRotVibs = internalRotVibs;
    }
    
    public GauOutputToNASAConverter(File src, File dest, StatMechThermoCalculator thermoCal, CompoundDescriptionBuilderIntf descrBuilder, Map<String, Double> hfData, Map<String, Map<Bond, Double>> internalRotVibs) {
        super(src, dest, thermoCal, descrBuilder);
        this.hfData = hfData;
        this.internalRotVibs = internalRotVibs;
    }
    
    @Override
    protected Map<String, Compound> parseDirectory() throws Exception {
        FrequencyParser parser = new FrequencyParser();
        CompChemParser ccParser = new CompChemParser();
        Collection<File> files = fFilter.getValidFiles(src, true);
        Map<String, Compound> compounds = new HashMap<String, Compound>();
        int skip = 0;
        int ctr = 1;
        for (File f : files) {
            try {
                logger.info("Converting file " + ctr + " of " + files.size());
                ctr++;
                parser.set(f);
                try {
                    parser.parse();
                } catch (OutOfMemoryError oome) {
                    skip++;
                    logger.warn("File " + f.getAbsolutePath() + " could not be read and will be skipped!");
                    System.gc();
                    continue;
                }
                ccParser.setCompChem((CompChem) parser.get());
                ccParser.parse();
                Compound comp = ccParser.get();
                comp.setInchi(f.getName().replace(".g09", ""));
                if (hfData != null) {
                    if (hfData.containsKey(f.getName().replace(".g09", ""))) {
                        EnthalpyOfFormation enf = new EnthalpyOfFormation(1000 * hfData.get(f.getName().replace(".g09", "")));
                        comp.setHf(enf);
                    } else {
                        skip++;
                        logger.warn("No enthalpy data found for file " + f.getAbsolutePath() + " and it will be skipped!");
                        continue;
                    }
                }
                if (internalRotVibs != null) {
                    if (internalRotVibs.containsKey(f.getName())) {
                        Map<Bond, Double> freqs = internalRotVibs.get(f.getName());
                        for (int i = 0; i < comp.getVibrationCount(); i++) {
                            for (Bond b : freqs.keySet()) {
                                double currentFreq = freqs.get(b);
                                if (Math.abs(comp.getVibration(i).Frequency - currentFreq) < 0.01) {
                                    comp.getVibration(i).addIRotorBond(b);
                                    break;
                                }
                            }
                        }
                    }
                }
                compounds.put(f.getName(), comp);
            } catch (Exception e) {
                skip++;
                logger.warn("File " + f.getAbsolutePath() + " could not be read and will be skipped!");
            }
        }
        logger.info((files.size() - skip) +  " / " + files.size() + " have been successfully read.");
        return compounds;
    }
    
    **/
}
