/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.converter.gau;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util.CMLChemCompound;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util.ChemCompound;
import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import org.cam.ceb.como.chem.filemgmt.writer.ChemFileWriter;
import gigadot.chom.chem.structure.Compound;
import java.io.File;
import org.cam.ceb.como.tools.structure.util.CompoundConverter;

/**
 *
 * @author pb556
 */
public class SDFToGauConverter implements GauConverterIntf {

    protected File input = null;
    protected File output = null;
    protected String hdr = null;
    protected String rhdr = null;
    protected ChemFileParser parser = null;
    protected ChemFileWriter writer = null;
    protected CMLChemCompound comp = null;

    @Override
    public void setInput(File input) {
        this.input = input;
    }

    @Override
    public void setOutput(File output) {
        this.output = output;
    }

    @Override
    public void setHeader(String hdr) {
        this.hdr = hdr;
    }

    @Override
    public void setRestrictedHeader(String hdr) {
        this.rhdr = hdr;
    }

    @Override
    public void setParser(ChemFileParser parser) {
        this.parser = parser;
    }

    @Override
    public void setWriter(ChemFileWriter writer) {
        this.writer = writer;
    }

    @Override
    public void convert() throws Exception {

        if (this.input == null) {
            throw new Exception("No input file is defined.");
        }

        if (this.parser == null || this.writer == null) {
            throw new Exception("No reader or writer is defined.");
        }

        this.parse();

        //Compound compound = (Compound) this.comp.getCompound();
        //compound.recreateMoleculeList();

        if (this.comp instanceof CMLChemCompound) {
        } else {
            throw new Exception("Invalid data type. ChemCompound is requried containing a CMLMolecule object.");
        }

        this.writer.set(this.comp);
        this.writer.setPath(this.output.getAbsolutePath() + "\\" + this.comp.getCMLCompound().getId() + ".gau");
        this.writer.setHeader(this.hdr);
        this.writer.setRestrictedHeader(this.rhdr);
        this.writer.write();
    }

    protected void parse() throws Exception {
        this.parser.set(this.input.getAbsolutePath());
        this.parser.parse();
        ChemCompound comp = (ChemCompound) this.parser.get();
        ((Compound) comp.getCompound()).recreateMoleculeList();
        this.comp = new CMLChemCompound();
        this.comp.setCMLMolecule(CompoundConverter.convert((Compound) comp.getCompound()));
        this.comp.setPath(this.input.getAbsolutePath());
        if (this.comp == null) {
            throw new Exception("Input file could not be read.");
        }
    }    
}