package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.eclipse.persistence.exceptions.JAXBException;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.PropertyList;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingElectronicEnergy;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometry;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometryType;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FormulaUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;

/**
 * 
 * The Class GenerateXml.
 *
 * @author nk510 (caresssd@hermes.cam.ac.uk)
 * 
 *         <p>
 * 		Generates CompChem XML files by parsing Gaussian (g09) files. which
 *         are stored in folder 'src/test/resources/g09/' of CoMoOntology.
 *         project. In CoMoOntology project, we use parser implemented by.
 *         @author{pb556} in CoMoIOChemistry, and CoMoEnthalpyEstimation
 *         projects.
 *         </p>
 *         
 */

public class GenerateXml {

	/**
	 * 
	 * The main method.
	 *
	 * @param args
	 *            the arguments.
	 * @throws Exception
	 *             the exception.
	 * @throws JAXBException
	 *             the JAXB exception.
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws XMLStreamException
	 *             the XML stream exception.
	 * @throws FactoryConfigurationError
	 *             the factory configuration error.
	 * @throws javax.xml.bind.JAXBException the JAXBE exception. 
	 * 
	 */

	public static void main(String[] args)

			throws Exception, javax.xml.bind.JAXBException, IOException, XMLStreamException, FactoryConfigurationError {
		
		Utility utility = new FileUtility();

		/**
		 * Gets g09 the file list.
		 *
		 * @author nk510
		 * @param folderPath
		 *            the folder path where Gaussian files are saved.
		 * @return
		 *         <p>
		 * 		Read all files which end with '.g09'. Returns array of these files.
		 *         </p>
		 */

		File[] fileList = utility.getFileList("src/test/resources/g09/", ".log", ".g09");

		for (File gaussianFile : fileList) {

			Module rootModule = new Module();

			String fileName = gaussianFile.getName().replaceAll(".log", "");

			/**
			 * 
			 * @author nk510
			 *         <p>
			 * 		Folder where we save all generated Compchem XML files.
			 *         </p>
			 * 
			 */

			File outputFile = new File("src/test/resources/compchem_xml/" + fileName + ".xml");

			generateRootModule(gaussianFile, outputFile, rootModule);

		}
	}

	/**
	 * Gets the empirical parser.
	 *
	 * @author nk510
	 * @param formulaName
	 *            the formula name.
	 * @return
	 *         <p>
	 * 		Parses formula name and return Composition values of that formula,
	 *         including formula name. It uses
	 *         {@link uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser}
	 *         class.
	 *         </p>
	 *
	 */

	public static Molecule getEmpiricalParser(String formulaName) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();
		
		return empParser.parseModule(formulaName);
	}

	/**
	 * Generate root module.
	 *
	 * @author nk510
	 * @param file
	 *            the file to be parsed.
	 * @param outputfile the output file.
	 * @param rootModule the root module.           
	 *            the root module as instance of class <code>Module</code>.
	 * @return the module as instance of class
	 *         <code>{@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Module}</code>.
	 * @throws Exception
	 *             <p>
	 * 			Generates CompChem XML whole file based on parsing g09 files.
	 *             Currently it supports the following features: Formula, Basis Set,
	 *             Level of Theory, Composition, Frequencies, Rotational Symmetry
	 *             Number, Geometry, Geometry type, Rotational Constants, Spin
	 *             Multiplicity, Atomic mass.
	 *             </p>
	 */

	public static Module generateRootModule(File file, File outputfile, Module rootModule) throws Exception {

		Module initialModule = GenerateCompChemModule.generateInitialModule();

		Module finalModule = GenerateCompChemModule.generateFinalModule();

		Module environmentModule = GenerateCompChemModule.getEnvironmentModule();

		environmentModule.getAny().add(GenerateParameter.getParameterListEnvironmentModule(file));

		FormulaUtility fp = new FormulaUtility();

		ParsingGeometry pg = new ParsingGeometry();
		
		ParsingElectronicEnergy electronicEnergy = new ParsingElectronicEnergy();

		ParsingGeometryType pgt = new ParsingGeometryType();

		initialModule.getAny().add(getEmpiricalParser(fp.extractFormulaName(file)));

		int sumOfAtoms = fp.getSumOfAllAtomNumbers(fp.extractFormulaName(file));

		System.out.println("Summ of all atoms is: " + sumOfAtoms);

		if (sumOfAtoms > 1) {

			Molecule geometryMolecule = new Molecule();

			geometryMolecule = pg.getGeometryFromG09(file);

			finalModule.getAny().add(GenerateProperty.getPropertyListFinalModule(file));

			finalModule.getAny().add(geometryMolecule);

			initialModule.getAny().add(GenerateParameter.getParameterListInitialModule(file, sumOfAtoms));

			GenerateCompChemModule.getRootModule(initialModule, finalModule, environmentModule, rootModule);

		} else {

			/**
			 * 
			 * @author nk510
			 *         <p>
			 * 		Adds 'geometry type' value in
			 *         {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.PropertyList} as a
			 *         instance of class
			 *         {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Property}.
			 *         </p>
			 * 
			 */
			PropertyList propertyListFinalModule = new PropertyList();
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
					.add(pgt.getGeometryTypeFromG09(file.getAbsoluteFile()));

			/**
			 * Adds total energy into Property list for species with one atom. 
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation().add(electronicEnergy.getZeroPointCorrectionProperty(file));
			
			/**
			 * Adds scf energy into Property list for species with one atom.
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation().add(electronicEnergy.getSCFElectronicEnergyProperty(file));
			
			/**
			 * 
			 * @author nk510 <p>Returns an instance of
			 *         {@link uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule} class that
			 *         contains atomic mass number of each atom. </p>
			 * 
			 */
			Molecule geometryMolecule = new Molecule();

			geometryMolecule = pg.getGeometryFromG09OneAtomMolecule(file);

			finalModule.getAny().add(geometryMolecule);

			finalModule.getAny().add(propertyListFinalModule);

			initialModule.getAny().add(GenerateParameter.getParameterListInitialModule(file, sumOfAtoms));

			GenerateCompChemModule.getRootModule(initialModule, finalModule, environmentModule, rootModule);

		}
		
		try {

			JAXBContext context = JAXBContext.newInstance(Module.class);
			Marshaller marshaller = context.createMarshaller();

			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

			marshaller.marshal(rootModule, outputfile);
			marshaller.marshal(rootModule, System.out);

		} catch (JAXBException e) {
			e.printStackTrace();
		}
		return rootModule;
	}
}