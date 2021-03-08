/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.writer.mol;

import org.cam.ceb.como.chem.filemgmt.writer.ChemFileWriter;
import gigatools.extra.openbabel.OpenBabelConverter;
import gigatools.extra.openbabel.OpenBabelException;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class MolWriter extends ChemFileWriter {

    @Override
    public void write() throws Exception {
        if (this.obj == null) {
            throw new Exception("No chemical compound defined.");
        }

        CMLMolecule mol = (CMLMolecule) this.obj;

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
            String convert = OpenBabelConverter.convert(mol, OpenBabelConverter.OBFormat.Mol);
            try {
                if (!this.path.endsWith(".mol")) {
                    this.path = this.path + ".mol";
                }
                FileUtils.writeStringToFile(new File(this.path), convert);
                return;
            } catch (IOException ex) {
                Logger.getLogger(MolWriter.class.getName()).log(Level.SEVERE, null, ex);
            }
        } catch (OpenBabelException ex) {
            Logger.getLogger(MolWriter.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new Exception("Unsuccessful writing operation.");
    }
}
