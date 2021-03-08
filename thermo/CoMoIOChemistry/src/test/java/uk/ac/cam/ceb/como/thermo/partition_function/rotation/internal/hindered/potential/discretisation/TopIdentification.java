/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

/**
 * Extraction of CML/G09 file information about torsional bonds and extraction
 * of the associated tops within the same molecule.
 *
 * @author pb556
 */
public class TopIdentification {

//    @Test
//   // @Ignore
//    public void cmlTest() throws Exception {
//        String src = "W:\\Data\\TTIP\\hr\\cml\\freq-hr-fine-h-species-0029-radical-1.cml";
//        String dest = "W:\\Data\\TTIP\\hr\\test\\";
//        // extraction from the cml/g09 file
//        CMLHRFileParser parser = new CMLHRFileParser(new File(src));
//        parser.parse();
//        CompChem cc = parser.getCompChem();
//        IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
//        RotationalModes rotModes = ccw.getRotationalModes(true);
//        MolWriter molWriter = new MolWriter();
//        molWriter.setPath(dest + "\\ref_" + new File(src).getName().replace(".cml", ".mol"));
//        molWriter.set(ccw.getFinalMolecule());
//        molWriter.write();
//        for (RotationalMode m : rotModes) {
//            // null has been returned!
//            CMLMolecule mol = m.getTop().getReferenceGeometry();
//            CMLBond torsBond = m.getTop().getBond();
//
//            RotationalMotionDiscretiser discretiser = new RotationalMotionDiscretiser();
//            discretiser.setReferenceGeometry(mol);
//            discretiser.setTorsionalBond(torsBond);
//
//            double angle = 12.0 * Math.PI / 180.0;
//
//            RotationalStep step = discretiser.rotate(angle);
//            CMLMolecule rotMol = step.getCMLMolecule();
//
//            molWriter.setPath(dest + "\\" + CompoundConverter.compressEmpiricalFormula(CompoundConverter.convertToEmpiricalFormulaFromCML(m.getTop().getAtoms())) + "_" + new File(src).getName().replace(".cml", ".mol"));
//            molWriter.set(rotMol);
//            molWriter.write();
//        }
//    }
}
