package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import javax.xml.stream.FactoryConfigurationError;

import javax.xml.stream.XMLStreamException;
import org.eclipse.persistence.exceptions.JAXBException;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Parameter;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.ParameterList;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.PropertyList;
import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingBasisSet;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingEnvironment;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingFrequencies;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometry;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometryType;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingLevelOfTheory;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingRotationalConstants;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingRotationalSymmetry;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FormulaUtility;

/**
 * The Class GenerateCompChemXML.
 *
 * @author nk510
 * <p>Generates CompChem XML files by parsing Gaussian (g09) files
 *         which are stored in folder 'src/test/resources/g09/' of CoMoOntology
 *         project. In CoMoOntology project, we use parser implemented by
 *         {@author pb556} in CoMoIOChemistry project.</p>
 */

public class GenerateCompChemXML {

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 * @throws Exception the exception
	 * @throws JAXBException the JAXB exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws XMLStreamException the XML stream exception
	 * @throws FactoryConfigurationError the factory configuration error
	 */
	
	public static void main(String[] args)

			throws Exception, javax.xml.bind.JAXBException, IOException, XMLStreamException, FactoryConfigurationError {

		File[] fileList = getFileList("src/test/resources/g09/");


		for (File f : fileList) {

			Module rootModule = new Module();
			
			String fileName = f.getName().replaceAll(".g09","");

			/**
			 * @author nk510
			 * <p>Folder where we save all generated CompChem XML files.</p>
			 */
			
			File outputFile = new File("src/test/resources/compchem_xml/" + fileName + ".xml");

			generateRootModule(f, outputFile, rootModule);

		}
	}

	/**
	 * Gets the file list.
	 *
	 * @author nk510
	 * @param folderPath the folder path
	 * @return <p>Read all files which end with '.g09'. Returns array of these files.</p>
	 */
	
	public static File[] getFileList(String folderPath) {

		File dir = new File(folderPath);

		File[] fileList = dir.listFiles(new FilenameFilter() {
			
			public boolean accept(File dir, String name) {
			
				return name.endsWith(".g09");
			}
		});

		return fileList;
	}

	/**
	 * Gets the root module.
	 *
	 * @author nk510
	 * @param in_module the in module
	 * @param f_module the f module
	 * @param rootModule the root module
	 * @return <p>Returns object variable of Module JAXB class. It contains information
	 *         (DictRef) about job lists, and job.</p>
	 */
	
	public static Module getRootModule(Module in_module, Module f_module, Module env_module, Module rootModule) {

		Module jobListModule = new Module();

		rootModule.getAny().add(jobListModule);

		jobListModule.setDictRef("cc:jobList");

		Module jobModule = new Module();

		jobListModule.getAny().add(jobModule);

		jobModule.setDictRef("cc:job");

		jobModule.getAny().add(in_module);

		jobModule.getAny().add(f_module);
		
		jobModule.getAny().add(env_module);

		return jobModule;
	}

	/**
	 * Gets the empirical parser.
	 *
	 * @author nk510
	 * @param formulaName the formula name
	 * @return <p>Parses formula name and return Composition values of that formula,
	 *         including formula name. It uses EmpiricalFormulaParser class.</p>
	 */
	public static Molecule getEmpiricalParser(String formulaName) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

		return empParser.parseModule(formulaName);

	}

	/**
	 * Generate root module.
	 *
	 * @param file the file
	 * @param rootModule the root module
	 * @return the module
	 * @throws Exception            <p>Generates CompChem XML whole file based on parsing g09 files.
	 *             Currently it supports the following features: Formula,
	 *             Composition, Frequencies, Symmetry Nr, Geometry, Geometry type,
	 *             Rotational Constants, Spin Multiplicity.</p>
	 */

	public static Module generateRootModule(File file, File  outputfile, Module rootModule) throws Exception {

		Module initialModule = GenerateCompChemModule.generateInitialModule();
		Module finalModule = GenerateCompChemModule.generateFinalModule();
		
		Module environmentModule = GenerateCompChemModule.getEnvironmentModule();
		
		Molecule geometryMolecule = new Molecule();

		Molecule initialMolecule = new Molecule();

		PropertyList propertyListFinalModule = new PropertyList();
		
		ParameterList parameterListInitialModule = new ParameterList();
		ParameterList parameterListEnvironmentModule = new ParameterList();
		
		parameterListEnvironmentModule.getParameterOrParameterList().add(ParsingEnvironment.getProgramName(file));
        parameterListEnvironmentModule.getParameterOrParameterList().add(ParsingEnvironment.getProgramVersion(file));
        parameterListEnvironmentModule.getParameterOrParameterList().add(ParsingEnvironment.getRunDateStamp(file));
        
        environmentModule.getAny().add(parameterListEnvironmentModule);
        
		FormulaUtility fp = new FormulaUtility();

		ParsingFrequencies pf = new ParsingFrequencies();

		ParsingRotationalSymmetry prs = new ParsingRotationalSymmetry();

		ParsingRotationalConstants rcp = new ParsingRotationalConstants();

		ParsingGeometry pg = new ParsingGeometry();

		ParsingGeometryType pgt = new ParsingGeometryType();

		initialMolecule = getEmpiricalParser(fp.extractFormulaName(file));

		initialModule.getAny().add(initialMolecule);

		int sumOfAtoms = fp.getSumOfAllAtomNumbers(fp.extractFormulaName(file));

		System.out.println("Summ of all atoms is: " + sumOfAtoms);

		if (sumOfAtoms > 1) {
			/**
			 * @author nk510 
			 * <p>Generates 'Frequencies'.</p>
			 * 
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
					.add(pf.generateFrequenciesFromG09(file.getAbsoluteFile()));

			/**
			 * @author nk510 
			 * <p>Generates 'Rotational symmetry'.</p>
			 * 
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
			.add(prs.generateRotationalSymmetryFromG09(file.getAbsoluteFile()));

			/**
			 * @author nk510 
			 * <p>Generates 'Rotational constants'.</p>
			 * 
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
			.add(rcp.generateRotationalConstantsFromG09(file.getAbsoluteFile()));

			/**
			 * 
			 * @author nk510 
			 * <p>Generates 'Geometry type'.</p>
			 * 
			 */
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
			.add(pgt.getGeometryTypeFromG09(file.getAbsoluteFile()));
			
            Parameter parameterLevelOfTheory = ParsingLevelOfTheory.getLevelOfTheryParameter(file, sumOfAtoms);
            Parameter parameterBasisSet = ParsingBasisSet.getBasisSetParameter(file, sumOfAtoms);
            
			parameterListInitialModule.getParameterOrParameterList().add(parameterLevelOfTheory);
			parameterListInitialModule.getParameterOrParameterList().add(parameterBasisSet);
			

			geometryMolecule = pg.getGeometryFromG09(file);

			finalModule.getAny().add(propertyListFinalModule);

			finalModule.getAny().add(geometryMolecule);

			initialModule.getAny().add(parameterListInitialModule);
			
			getRootModule(initialModule, finalModule, environmentModule,rootModule);

		} else {
			
			Parameter parameterLevelOfTheory = ParsingLevelOfTheory.getLevelOfTheryParameter(file, sumOfAtoms);
			Parameter parameterBasisSet = ParsingBasisSet.getBasisSetParameter(file, sumOfAtoms);
			
			parameterListInitialModule.getParameterOrParameterList().add(parameterLevelOfTheory);
			parameterListInitialModule.getParameterOrParameterList().add(parameterBasisSet);
			
			/**
			 * 
			 * @author nk510 
			 * <p>Adds 'Geometry type' value in PropertyList as a value of
			 *         Property.</p>
			 *         
			 */
			
			propertyListFinalModule.getPropertyOrPropertyListOrObservation()
					.add(pgt.getGeometryTypeFromG09(file.getAbsoluteFile()));
			
			/**
			 * 
			 * @author nk510
			 * Returns an instance of Molecule class that contains atomic mass number of one atom.
			 *  
			 */
			
			geometryMolecule = pg.getGeometryFromG09OneAtomMolecule(file);
			
			finalModule.getAny().add(geometryMolecule);
			
			finalModule.getAny().add(propertyListFinalModule);
			
			initialModule.getAny().add(parameterListInitialModule);
			
			getRootModule(initialModule, finalModule, environmentModule ,rootModule);

		} try {
			
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