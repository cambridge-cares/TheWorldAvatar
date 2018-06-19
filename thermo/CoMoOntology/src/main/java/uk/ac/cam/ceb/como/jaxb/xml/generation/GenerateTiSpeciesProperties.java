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
 * 
 * @author nk510
 * This class implements methods for generating the following properties of species:
 *1. File name.
 *2. Number of Ti-species observed in g09 file.
 *3. Spin multiplicity.
 *
 */

public class GenerateTiSpeciesProperties {
	
	public static void main(String[] args) throws Exception {
		
		/**
		 * @author nk510
		 * Resulting csv file path
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
	 * 
	 * @author nk510
	 * @param folderPath
	 * @return Read all files which end with '.g09'. Returns array of these files.
	 * 
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
	 * 
	 * @param filePath
	 * @return
	 * Returns object variable of PrintWriter. It creates csv file where all results of parsing g09 files will be saved.
	 * 
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
	 * 
	 * @author nk510
	 * @param formula
	 * @return Returns the number of Ti species atoms for each species (g09).
	 * 
	 */
	
	public static  String getTiSpeciesAtomNumbers(String formula) {

		EmpiricalFormulaParser empParser = new EmpiricalFormulaParser();

		return empParser.getTiAtomsCount(formula);
	}
	
}