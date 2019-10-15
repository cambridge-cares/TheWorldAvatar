/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.Set;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.io.pool.CSVParser;
import org.cam.ceb.como.enthalpy.calculator.wrapper.Calculator;

/**
 *
 * @author pb556
 */
public class CSVCalculator extends Calculator {

    protected PoolModificationCalculator calc = null;
    protected CSVParser parser = null;
    protected File csvRef = null;
    protected File csvSoi = null;
    private Logger logger = Logger.getLogger(getClass());

    public CSVCalculator(CSVParser parser, LPSolver solver, LPFormat format, PoolModificationCalculator calc) {
        super(solver, format);
        this.parser = parser;
        this.calc = calc;
    }
    
    public CSVCalculator(CSVParser parser, LPSolver solver, LPFormat format, SpeciesFilter filter, PoolModificationCalculator calc) {
        super(solver, format, filter);
        this.parser = parser;
        this.calc = calc;
    }
    
    public void setCSV(File csvRef, File csvSoi) {
        this.csvRef = csvRef;
        this.csvSoi = csvSoi;
    }
    
    private ObjectPool<Species> getPool(File csv, boolean ref) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        pool.addAll(load(csv, ref));
        return pool;
    }

    private Set<Species> load(File csv, boolean ref) {
        try {
            parser.setPath(csv);
            parser.parse();
            if (ref) {
                return parser.getRefSpecies();
            }
            return parser.getSpeciesOfInterest();
        } catch (FileNotFoundException ex) {
            logger.error("The file " + csv.getAbsolutePath() + " does not exist!", ex);
        } catch (IOException ex) {
            logger.error("The file " + csv.getAbsolutePath() + " could not be read!s", ex);
        }
        return null;
    }

    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
        if (csvRef == null) {
            logger.error("No input is defined!", new IOException("No input is defined!"));
        }
        //calc = new MultiCalculator(solver, format, getPool(csvRef, false));
        calc.set(getPool(csvRef, false));
        calc.calculate(load(csvRef, false));
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        if (csvRef == null) {
            logger.error("No input is defined!", new IOException("No input is defined!"));
        }
        //calc = new MultiCalculator(solver, format, getPool(csvRef, false));
        calc.set(getPool(csvRef, false));
        calc.calculate(targetSpecies);
    }

    @Override
    public void calculate() throws Exception {
        if (csvRef == null || csvSoi == null) {
            logger.error("No input is defined!", new IOException("No input is defined!"));
        }
        calculate(load(csvSoi, true));
    }

    @Override
    public Object get() {
        return calc.get();
    }
}
