/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.writer.gaussian;

import org.cam.ceb.como.chem.filemgmt.writer.ChemFileWriter;
import gigatools.extra.openbabel.OpenBabelConverter;
import gigatools.extra.openbabel.OpenBabelException;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util.CMLChemCompound;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class GauWriter extends ChemFileWriter {

    protected String append = null;

    public GauWriter() {
        this.hdr = "%NProcShared=8\n"
                + "%Mem=500MW\n"
                + "#p uB971/6-311+G(d,p) "
                + "Opt=(Loose, NewEstmFC, MaxCyc = 200)\n"
                + "GFInput Population=None Integral(Grid=CoarseGrid)\n"
                + "Guess=Mix NoSymmetry";
    }

    public void append(String s) {
        this.append = s;
    }
    
    public String[] getMoleculeSpecification() throws Exception {
        int startLine = this.startLine(2);
        if (startLine < 0) {
            throw new Exception("Text cannot be read.");
        }
        String[] lines = this.getBasicContent().replace("\r", "").split("\n");
        String[] molSpec = new String[lines.length - startLine];
        int j = 0;
        for (int i = startLine; i < lines.length; i++) {
            molSpec[j] = lines[i];
            j++;
        }
        return molSpec;
    }
    
    public String getRoute() throws Exception {
        int startLine = this.startLine(1);
        if (startLine < 0) {
            throw new Exception("Text cannot be read.");
        }
        String[] lines = this.getBasicContent().replace("\r", "").split("\n");
        String route = lines[0];
        for (int i = 1; i < startLine; i++) {
            route += " " + lines[i];
        }
        return route;
    }
    
    public String getTitle() throws Exception {
        if (this.obj == null) {
            throw new Exception("No chemical compound defined.");
        }
        if (((CMLMolecule) this.obj).getId() != null && !((CMLMolecule) this.obj).getId().isEmpty()) {
            return ((CMLMolecule) this.obj).getId();
        }
        return path;
    }

    protected int startLine(int numEmptyLines) throws Exception {
        // get start of the position information
        String[] lines = this.getBasicContent().replace("\r", "").split("\n");
        int ctrEmptyLines = 0;
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].isEmpty()) {
                ctrEmptyLines++;
                if (ctrEmptyLines == numEmptyLines) {
                    return i + 1;
                }
            }
        }
        return -1;
    }

    public String getBasicContent() throws Exception {
        
        if (this.obj == null) {
            throw new Exception("No chemical compound defined.");
        }
        
        
        
        CMLMolecule mol = null;
        
        if (this.obj instanceof CMLMolecule) {
            mol = (CMLMolecule) this.obj;
        } else if (this.obj instanceof CMLChemCompound) {
            CMLChemCompound c = (CMLChemCompound) this.obj;
            mol = c.getCMLCompound();
        }

//        if (this.obj instanceof ChemCompound) {
//            ChemCompound cc = (ChemCompound) this.obj;
//            Compound comp = (Compound) cc.getCompound();
//            comp.recreateMoleculeList();
//            if (cc.getPosition() == null) {
//                mol = Species.getCMLMolecule(comp);
//            } else {
//                mol = Species.getCMLMolecule(comp, (HashMap<Atom, Point3>) cc.getPosition());
//            }
//            mol.setId(cc.getID());
//            mol.setFormalCharge(cc.getFormalCharge());
//            mol.setSpinMultiplicity(cc.getSpinMultiplicity());
//        }
//        else {
//            mol = (CMLMolecule) this.obj;
//        }
        
        try {
            String convert = OpenBabelConverter.convert(mol, OpenBabelConverter.OBFormat.Gau);
            //String convert2 = OpenBabelConverter.convert(this.obj.getPath(), OpenBabelConverter.OBFormat.G03, OpenBabelConverter.OBFormat.Gau, "");
            if (mol.getSpinMultiplicity() == 1) {
                convert = convert.replace("#Put Keywords Here, check Charge and Multiplicity.", this.rhdr);
            }
            else {
                convert = convert.replace("#Put Keywords Here, check Charge and Multiplicity.", this.hdr);
            }
            return convert.replaceFirst("0  1", "0  " + mol.getSpinMultiplicity());
        } catch (OpenBabelException ex) {
            Logger.getLogger(GauWriter.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    @Override
    public void write() throws Exception {
        String convert = this.getBasicContent();
        if (this.append != null) {
            convert += this.append;
        }
        if (this.path == null) {
            throw new Exception("No path defined.");
        }
        try {
            if (!this.path.endsWith(".gau")) {
                this.path = this.path + ".gau";
            }
            FileUtils.writeStringToFile(new File(this.path), convert);
            return;
        } catch (IOException ex) {
            Logger.getLogger(GauWriter.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new Exception("Unsuccessful writing operation.");
    }
    
        protected String getHeader(int numRadicals) {
        if (numRadicals == 0) {
            return this.rhdr;
        }
        return this.hdr;
    }
    
}
