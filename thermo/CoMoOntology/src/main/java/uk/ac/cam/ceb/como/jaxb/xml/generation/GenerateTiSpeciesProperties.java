/*
 * 
 */
package uk.ac.cam.ceb.como.jaxb.xml.generation;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.PrintWriter;

import org.xmlcml.cml.element.CMLMolecule;

import uk.ac.cam.ceb.como.io.chem.file.parser.formula.EmpiricalFormulaParser;
import uk.ac.cam.ceb.como.jaxb.parser.g09.ParsingGeometry;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.FormulaUtility;


/**
 * The Class GenerateTiSpeciesProperties.
 *
 * @author nk510
 * This class implements methods for generating the following properties of species:
 * 1. File name.
 * 2. Number of Ti-species observed in Gaussian (g09) file.
 * 3. Spin multiplicity.
 */

public class GenerateTiSpeciesProperties {
	
	/**
	 * The main method.
	 *
	 * @param args the arguments
	 * @throws Exception the exception
	 */
	public static void main(String[] args) throws Exception {
		
		/**
		 * @author nk510
		 * <p>Resulting csv file path</p>
		 */
		
		String csvFilePath = "src/test/resources/Ti-g09/Ti-g09.csv";
		
		File[] fileList = getFileList("src/test/resources/Ti-g09/");
		
		StringBuilder stringBuilder = new StringBuilder();
		
		String columnName = "fileName, Number of Ti-atoms, Spin Multiplicity"; 
		
		stringBuilder.append(columnName+"\n");
		
		PrintWriter pWriter = getPrintWriter(csvFilePath);
		
		pWriter.write(stringBuilder.toString());
		
		for (File f : fileList) {
			
			FormulaUtility fp = new FormulaUtility();					
			
			ParsingGeometry pg = new ParsingGeometry();
			
			CMLMolecule cml_m = pg.getFinalCMLMolecule(f);
			
			String spinMultiplicity = cml_m.getSpinMultiplicity() +"";
			
			
			pWriter.write(f.getName().replaceAll(".g09","").toString()+","+getTiSpeciesAtomNumbers(fp.extractFormulaName(f))+","+spinMultiplicity +","+"\n");
		}
		
		pWriter.close();
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
	 * Gets the prints the writer.
	 *
	 * @param filePath the file path
	 * @return <p>Returns object variable of PrintWriter. It creates csv file where all results of parsing g09 files will be saved.</p>
	 */
	
	public static PrintWriter getPrintWriter(String filePath) {
		
		PrintWriter printWriter = null;
		try {
		    printWriter = new PrintWriter(new File(filePath));
		} catch (FileNotFoundException e) {
		    e.printStackTrace();
		}
		
		return printWriter;
	}
	
	/**
	 * Gets the ti species atom numbers.
	 *
	 * @author nk510
	 * @param formula the formula
	 * @return <p>Returns the number of Ti species atoms for each species (g09).</p>
	 */
	
	public static  String getTiSpeciesAtomNumbers(String formula) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

		return empParser.getTiAtomsCount(formula);
	}
	
}