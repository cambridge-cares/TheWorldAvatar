package uk.ac.cam.ceb.como.jaxb.parser.g09;

import java.io.File;
import java.math.BigInteger;
import java.util.List;
import java.util.StringTokenizer;

import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLMolecule;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Atom;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.AtomArray;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FormulaUtility;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 * The Class ParsingGeometry.
 *
 * @author nk510
 *         <p>
 * 		This class implements methods for generating CompChem XML file by
 *         parsing 'Geometry' data from g09 files. Parser class
 *         'FrequencyParser' of @author{pb556} is used to extract 'Geometry'
 *         data (information).
 *         </p>
 */

public class ParsingGeometry {

	/**
	 * 
	 * Gets the geometry from G 09.
	 *
	 * @param file
	 *            <p>
	 * 			It is name of Gaussian file
	 *            </p>
	 * @return the geometry
	 *         <p>
	 *         This is extracted from Gaussian file
	 *         </p>
	 * @throws Exception
	 *             the exception
	 * 
	 */

	public Molecule getGeometryFromG09(File file) throws Exception {

		Molecule molecule = new Molecule();

		AtomArray atomArrayJxb = new AtomArray();

		StringTokenizer weightTokenizer = ParsingAtomicMass.getAtomicWeightString(file);

		List<String> atomicWeightList = ParsingAtomicMass.getListOfAtomicMass(weightTokenizer);

		
		if (atomicWeightList.size() > 1) {

			CMLMolecule cml_m = getFinalCMLMolecule(file);

			/**
			 * 
			 * @author nk510
			 *         <p>
			 * 		Set id, convention, formal charge, spin multiplicity as values of
			 *         corresponding Molecule, and Atom JXB class members, based on parsed
			 *         Gaussian files.
			 *         </p>
			 * 
			 */

			molecule.setId(cml_m.getId());
			molecule.setConvention(cml_m.getConvention());
			molecule.setSpinMultiplicity(BigInteger.valueOf(cml_m.getSpinMultiplicity()));

			molecule.setFormalCharge(BigInteger.valueOf(cml_m.getFormalCharge()));

			List<CMLAtom> atom = cml_m.getAtoms();

			int i = 0;

			for (CMLAtom a : atom) {

				Atom atomJxb = new Atom();

				atomJxb.setId(a.getId());
				atomJxb.setElementType(a.getElementType());
				atomJxb.setX3(a.getX3());
				atomJxb.setY3(a.getY3());
				atomJxb.setZ3(a.getZ3());

				atomJxb.setAtomicMass(atomicWeightList.get(i));

				atomArrayJxb.getAtom().add(atomJxb);

				i++;

			}

			molecule.getAngleOrArgOrArray().add(atomArrayJxb);

		}

		return molecule;
	}

	/**
	 * Gets the geometry from G 09 one atom molecule.
	 *
	 * @param file
	 *            <p>
	 *            It denotes Gaussian file
	 *            </p>
	 * @return Molecule
	 *         <p>
	 * 		It contains atomic mass for one atom. All geometry coordinates are
	 *         set to 0.00.
	 *         </p>
	 * @throws Exception
	 *             the exception
	 */

	public Molecule getGeometryFromG09OneAtomMolecule(File file) throws Exception {

		Molecule molecule = new Molecule();

		AtomArray atomArrayJxb = new AtomArray();

		FormulaUtility formulaUtility = new FormulaUtility();

		StringTokenizer weightTokenizer = ParsingAtomicMass.getAtomicWeightString(file);

		List<String> atomicWeightList = ParsingAtomicMass.getListOfAtomicMass(weightTokenizer);

		Atom atomJxb = new Atom();

		atomJxb.setId("a1");

		/**
		 * @author nk510 Suggested by Dr Daniel Nurkowski (danieln@cmclinnovations.com):
		 *         Set all coordinates 0.00 in case when a molecule has one atom.
		 */
		atomJxb.setX3(0.00);
		atomJxb.setY3(0.00);
		atomJxb.setZ3(0.00);

		/**
		 * @author nk510
		 *         <p>
		 * 		Set element type as atom name from a molecule that has only one atom.
		 *         </p>
		 */
		atomJxb.setElementType(getAtoNameOfOneAtomMolecule(formulaUtility.extractFormulaName(file)));
		
		atomJxb.setAtomicMass(atomicWeightList.get(0));

		atomArrayJxb.getAtom().add(atomJxb);

		molecule.setId("mol-final-2");
		molecule.setConvention("convention:molecular");
		
		/**
		 * @author nk510
		 * Extract formalCharge from Gaussian file (g09).
		 * 
		 */
		molecule.setFormalCharge(BigInteger.valueOf(ParsingCharge.getFormalCharge(file)));
		
		molecule.setSpinMultiplicity(BigInteger.valueOf(ParsingSpinMultiplicity.getSpinMultiplicity(file)));

		molecule.getAngleOrArgOrArray().add(atomArrayJxb);

		return molecule;

	}

	/**
	 * Gets the final CML molecule.
	 *
	 * @author nk510
	 * @param file
	 *            <p>
	 *            It is name of Gaussian file
	 *            </p>
	 * @return the final CML molecule
	 * @throws Exception
	 *             <p>
	 * 			Because there may exists more than one place in G09 file holding
	 *             information about Geometry, this methods collects information
	 *             about Geometry that is given in final molecule.
	 *             </p>
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

	/**
	 * Gets the atom name in one atom molecule.
	 *
	 * @param formula
	 *            <p>
	 * 			It represents a formula name (for example H2O)
	 *            </p>
	 * @return the atom name of one atom molecule
	 */
	public String getAtoNameOfOneAtomMolecule(String formula) {

		EmpiricalFormulaParser parser = new EmpiricalFormulaParser();

		return parser.getAtomName(formula);
	}

}
