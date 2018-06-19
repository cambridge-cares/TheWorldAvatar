package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.File;
import java.math.BigInteger;
import java.util.List;

import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLMolecule;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Atom;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.AtomArray;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 * 
 * @author nk510 This class implements methods for generating CompChem XML file
 *         by parsing 'Geometry' data from g09 files. Parser class
 *         'FrequencyParser' of {@author pb556} is used to extract 'Geometry'
 *         data (information)
 */
public class ParsingGeometry {

	public Molecule getGeometryFromG09(File file) throws Exception {

		Molecule molecule = new Molecule();

		AtomArray atom_array_jxb = new AtomArray();

		CMLMolecule cml_m = getFinalCMLMolecule(file);

		/**
		 * 
		 * @author nk510 Set id, convention, spin multiplicity as values of
		 *         corresponding Molecule, and Atom JXB class members, based on parsed g09
		 *         files.
		 * 
		 */

		molecule.setId(cml_m.getId());
		molecule.setConvention(cml_m.getConvention());
		molecule.setSpinMultiplicity(BigInteger.valueOf(cml_m.getSpinMultiplicity()));

		List<CMLAtom> atom = cml_m.getAtoms();

		for (CMLAtom a : atom) {

			Atom atom_jxb = new Atom();

			atom_jxb.setId(a.getId());
			atom_jxb.setElementType(a.getElementType());
			atom_jxb.setX3(a.getX3());
			atom_jxb.setY3(a.getY3());
			atom_jxb.setZ3(a.getZ3());

			atom_array_jxb.getAtom().add(atom_jxb);

		}

		molecule.getAngleOrArgOrArray().add(atom_array_jxb);

		return molecule;
	}

	/**
	 * @author nk510
	 * @param file
	 * @return
	 * @throws Exception
	 *             Because there may exists more than one place in G09 file holding
	 *             information about Geometry, this methods collects information
	 *             about Geometry that is given in final molecule.
	 */
	public CMLMolecule getFinalCMLMolecule(File file) throws Exception {

		FrequencyParser parser = new FrequencyParser();

		parser.set(file);
		parser.parse();

		CompChem cc = (CompChem) parser.get();

		IRCompChemWrapper irccw = new IRCompChemWrapper(cc);

		CMLMolecule cml_molecule = irccw.getFinalMolecule();

		return cml_molecule;

	}
}
