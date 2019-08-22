/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry;

import org.cam.ceb.como.chem.filemgmt.cml.parser.molecule.properties.CompChemParser;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.GeometryParser;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry.util.ChemCompound;
import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.model.brownie.Atom;
import java.io.File;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.xmlcml.euclid.Point3;

/**
 *
 * @author pb556
 */
public class GauOutputGeometryParser extends ChemFileParser {

    protected String[] validExtensions = new String[]{".g03", ".g09"};
    protected GeometryParser geomParser = new GeometryParser();

    @Override
    public void parse() throws Exception {
        if (this.path == null) {
            throw new Exception("No file defined.");
        }
        // check file type
        boolean valid = false;
        for (String ext : validExtensions) {
            if (this.path.endsWith(ext)) {
                valid = true;
                break;
            }
        }
        if (!valid) {
            throw new Exception("Invalid file type.");
        }
        this.extractGeometry();
    }

    protected void extractGeometry() throws Exception {
        // loads the geometry from a gau file
        if (this.path != null) {
            ChemCompound chemComp = new ChemCompound();
//            GaussianReader reader = new GaussianReader();
            //gigadot.chom.chem.structure.Compound comp = reader.read(this.path);
//            comp.recreateMoleculeList();
//
            //CompoundConverter converter = new CompoundConverter();
            
//            converter.setComponent(comp);
//            converter.convert();
//            Compound convCompound = (Compound) converter.getConvertedComponent();
            
            this.geomParser.set(this.path);
            this.geomParser.parse();
            CompChem cc = (CompChem) this.geomParser.get();
            CompChemParser ccParser = new CompChemParser();
            ccParser.setCompChem(cc);
            ccParser.parse();
            Compound compound = ccParser.get();
            
            try {
                compound.recreateMoleculeList();

                // create a hashMap containing all the individual position 
                // information of the atoms defined within the Gaussian file
                HashMap<Atom, Point3> pos = new HashMap<Atom, Point3>();
                //HashMap<String, Point3> posAtom = new HashMap<String, Point3>();
                for (int i = 0; i < compound.getAtomCount(); i++) {
                    Atom atom = compound.getAtom(i);
                    Point3 xyz = new Point3();
                    try {
                        int index = -1;
                        for (int j = 0; j < compound.getAtomCount(); j++) {
                            if (compound.getAtom(j).getId().equals(atom.getId())) {
                                index = i;
                                break;
                            }
                        }
                        if (index == -1) {
                            throw new Exception("Atom cannot be identified.");
                        }
                        xyz = new Point3(compound.getAtom(index).getXInA(),
                                compound.getAtom(index).getYInA(),
                                compound.getAtom(index).getZInA());
                    } catch (NumberFormatException nfe) {
                        // Ti atom
                        for (int j = 0; j < compound.getAtomCount(); j++) {
                            if (compound.getAtom(j).getElement().symbol.equals(atom.getElement())) {
                                xyz = new Point3(compound.getAtom(j).getXInA(), compound.getAtom(j).getYInA(), compound.getAtom(j).getZInA());
                                break;
                            }
                        }
                    }
                    pos.put(atom, xyz);
                }

                chemComp.setSpinMultiplicity(compound.getSpinMultiplicity());
                chemComp.setFormalCharge(compound.getCharge());
                String name = new File(this.path).getName();
                for (String ext : this.validExtensions) {
                    if (name.endsWith(ext)) {
                        chemComp.setID(new File(this.path).getName().replace(ext, ""));
                        break;
                    }
                }
                chemComp.setPath(this.path);
                chemComp.setCompound(compound);
                chemComp.setPosition(pos);
                chemComp.setID(new File(this.path).getName().replace(".g09", ""));
                this.obj = chemComp;
                return;

            } catch (NullPointerException npe) {
                Logger.getLogger(G03GeometryParser.class.getName()).log(Level.SEVERE, null, npe);
            }
        }

        throw new Exception("File could not be read.");
    }
}
