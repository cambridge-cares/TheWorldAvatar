package ontology_creator;

import java.util.Scanner;
import java.util.Set;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;//Don't use XPath for RDF/XML
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.hp.hpl.jena.iri.IRI;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class Tools {

	public static List<List<String>> createTableFromTabDelimited(String fileName) {
		/**
		 * Precondition:
		 * fileName is a valid tab-delimited text file.
		 * 
		 * Postcondition:
		 * 
		 * 
		 */
		File file = new File(fileName);
		System.out.println("Path : " + file.getAbsolutePath());
		List<List<String>> table = new ArrayList<>(); // nested list to represent the values from tab delimited file as a matrix
	
		try {
			Scanner s = new Scanner(file);
	
			while (s.hasNext()) {
				String line = s.nextLine();
				Scanner ss = new Scanner(line);
				ss.useDelimiter("\t");
				List<String> row = new ArrayList<>();
				while (ss.hasNext()) {
					String cell = ss.next();
					row.add(cell);
				}
				ss.close();
				table.add(row);
			}
			s.close();
		}
	
		catch (FileNotFoundException e) {
			e.printStackTrace();
		} // catch block
		return table;
	}

	public static Map<String,String> csvToMap(String fileName) {
		/**
		 * Precondition:
		 * fileName is a valid Comma-Separated Values (CSV) file.
		 * fileName has only 2 columns. Extra columns would be ignored.
		 * 
		 * Postcondition:
		 * returns a Map<String,String>, with the first column as the key and second column as the value.
		 * 
		 *  Assumes the first row is a header */
		Map<String,String> map = new HashMap<String,String>();
		String key;
		String element;
		try {
			File file = new File(fileName);
			Scanner s = new Scanner(file);
			s.nextLine();
			while (s.hasNext()) {
				String line = s.nextLine();
				Scanner ss = new Scanner(line);
				ss.useDelimiter(",");
				key = ss.next();
				element = ss.next();
				map.put(key, element);
				ss.close();
			}
			s.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return map;
	}
	
	public static List<List<String>> csvToList(String fileName) {
		File file = new File(fileName);
		System.out.println("Path : " + file.getAbsolutePath());
		List<List<String>> table = new ArrayList<>(); // nested list to represent the values from CSV file as a matrix
	
		try {
			Scanner s = new Scanner(file);
	
			while (s.hasNext()) {
				String line = s.nextLine();
				Scanner ss = new Scanner(line);
				ss.useDelimiter(",");
				List<String> row = new ArrayList<>();
				while (ss.hasNext()) {
					String cell = ss.next();
					row.add(cell);
				}
				ss.close();
				table.add(row);
			}
			s.close();
		}
	
		catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	
		return table;
	}

	public static List<List<String>> createTableFromCSV(String fileName) {
		/**
		 * Precondition:
		 * fileName is a valid Comma-Separated Values (CSV) file.
		 * 
		 * Postcondition:
		 * returns the content of the CSV file as a list of rows, with each row as a list of the cell contents (a nested list).
		 * Function removes all apostrophe mark (') from the end of the cells.
		 * Function removes all dots (.) from the start of cells.
		 * When a row is of size less than 4 columns, elements of the row will be shifted to the right and zeros will be added to the left of the elements
		 * 
		 */
		
		 
		File file = new File(fileName);
		System.out.println("Path : " + file.getAbsolutePath());
		List<List<String>> table = new ArrayList<>(); // nested list to represent the values from CSV file as a matrix
	
		try {
			Scanner s = new Scanner(file);
	
			while (s.hasNext()) {
				String line = s.nextLine();
				Scanner ss = new Scanner(line);
				ss.useDelimiter(",");
				List<String> row = new ArrayList<>();
				while (ss.hasNext()) {
					String cell = ss.next();
					while (cell.endsWith("'")) {
						cell = cell.substring(0, cell.length() - 1);
					}
					while (cell.startsWith(".")) {
						cell = cell.substring(1);
					}
					row.add(cell);
				}
				ss.close();
				while (row.size()<4 && row.size()>2) {
					row.add(0, "0");
				}
				table.add(row);
			}
			s.close();
		}
	
		catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	
		return table;
	}

	public static void replaceString(String oldString, String newString, String oldFileName, String newFileName) {
		/** 
		 * Precondition: 
		 * oldString is a String
		 * newString is a String
		 * oldFileName is a String pointing to a valid .owl file
		 * newFileName is a String specifying the new filename of a new .owl file
		 * 
		 * Postcondition:
		 * newFileName.owl has been written to the package folder
		 * The newFileName.owl is identical to oldFileName.owl, except that all the occurrence of oldString replaced by newString
		 *  
		 *  */
		
		try {
			// input the file content to the StringBuffer "input"
			BufferedReader file = new BufferedReader(new FileReader(oldFileName));
			String line;
			StringBuffer inputBuffer = new StringBuffer();
	
			while ((line = file.readLine()) != null) {
				inputBuffer.append(line);
				inputBuffer.append('\n');
			}
			String inputStr = inputBuffer.toString();
	
			file.close();
	
			// System.out.println(inputStr); // check that it's inputting correctly
	
			inputStr = inputStr.replace(oldString, newString); // Replacing old instance number with new instance number
	
			// check if the new input is right
	
			// System.out.println("----------------------------------\n" + inputStr);
	
			// write the new String with the replaced line on a new file
			FileOutputStream fileOut = new FileOutputStream(newFileName);
			fileOut.write(inputStr.getBytes());
			fileOut.close();
	
		} catch (Exception e) {
			System.out.println("Problem reading file.");
		}
	}

	public static void replaceString(String oldString, String newString, String fileName) {
		/** 
		 * Precondition: 
		 * oldString is a String
		 * newString is a String
		 * fileName is a String pointing to a valid .owl file
		 * 
		 * Postcondition:
		 * All occurrence of oldString in fileName.owl has been replaced by newString
		 *  
		 */
		
		try {
			// input the file content to the StringBuffer "input"
			BufferedReader file = new BufferedReader(new FileReader(fileName));
			String line;
			StringBuffer inputBuffer = new StringBuffer();
	
			while ((line = file.readLine()) != null) {
				inputBuffer.append(line);
				inputBuffer.append('\n');
			}
	
			String inputStr = inputBuffer.toString();
	
			file.close();
	
			// System.out.println(inputStr); // check that it's inputting correctly
	
			inputStr = inputStr.replace(oldString, newString);
	
			// check if the new input is right
			// System.out.println("----------------------------------\n" + inputStr);
	
			// write the new String with the replaced line OVER the same file
			FileOutputStream fileOut = new FileOutputStream(fileName);
			fileOut.write(inputStr.getBytes());
			fileOut.close();
	
		} catch (Exception e) {
			System.out.println("Problem reading file.");
		}
	}
	
	public static List<String> splitByDots(String string) {
		/**
		 * Precondition:
		 * string is a String with dots delimiting the string into parts
		 * 
		 * Postcondition:
		 * returns the splitted string as a List
		 * 
		 */
		List<String> slices = new ArrayList<String>(Arrays.asList(string.split("\\.")));
		return slices;
	}
	
	public static JenaOWLModel callJena(String fileName) {
		/**
		 * Precondition:
		 * the String fileName is a valid file
		 * 
		 * Postcondition:
		 * returns a JenaOWLModel object
		 * 
		 */
		try {
			System.out.println("Creating JenaOWLModel from " + fileName);
			
			File instancefile=new File(fileName);//Jenaowlmodeltest//newfilename
			String filepath=instancefile.getAbsolutePath();
			FileInputStream fileinputstream= new FileInputStream(filepath);
			Reader in = new InputStreamReader(fileinputstream,"UTF-8");
			
			JenaOWLModel owlModel=ProtegeOWL.createJenaOWLModelFromReader(in);
			
			return owlModel;
			
		} catch (OntologyLoadException e) {
			e.printStackTrace();
			return null;
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return null;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public static void createIRI(JenaOWLModel owlModel) {
		/**
		 * Precondition:
		 * owlmodel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * owlmodel is changed, with every Pipe and Stream now having hasIRI property
		 * 
		 */
		OWLDatatypeProperty hasIRI = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#hasIRI");
		
		List<OWLIndividual> OWLIndividuals = new ArrayList<OWLIndividual>(owlModel.getOWLIndividuals());
		for (int i=0; i<OWLIndividuals.size(); i++) {
			try {
				OWLIndividual obj = OWLIndividuals.get(i);
				if (obj.getLocalName().startsWith("Pipe_")) {
					String PipeName = obj.getLocalName();
					obj.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + PipeName + ".owl#" + PipeName);
				} 
				else if (obj.getLocalName().startsWith("ProcessStream_") || obj.getLocalName().startsWith("UtilityStream_") || obj.getLocalName().startsWith("FeedStream_") || obj.getLocalName().startsWith("WasteStream_") || obj.getLocalName().startsWith("ProductStream_")) {
					String StreamName = obj.getLocalName();
					String StreamNumber = StreamName.substring(StreamName.length()-6);
					String PipeName = "Pipe_" + StreamNumber;
					obj.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + PipeName + ".owl#" + StreamName);
				} 
				else {}
			} catch (ClassCastException e) {}
		}
	}

	public static void saveJena(JenaOWLModel owlModel, String fileName) {
		/**
		 * Precondition:
		 * owlmodel is a valid JenaOWLModel object
		 * 
		 * Postcondition:
		 * fileName.owl is saved to the package folder 
		 * 
		 */
		File instancefile = new File(fileName); // Jenaowlmodeltest//newfilename
		Collection errors = new ArrayList();
		owlModel.save(instancefile.toURI(), FileUtils.langXMLAbbrev, errors,owlModel.getOntModel());
		System.out.println("File " + fileName + " saved with " + errors.size() + " errors.");
	}

	public static void viewTable(List<List<String>> table) {
		/* Helper function to view the table */
		int lineNo = 0;
	
		for (List<String> line : table) {
			int columnNo = 0;
			for (String value : line) {
				System.out.println("Line " + lineNo + " Column " + columnNo + ": " + value);// Viewing Output
				columnNo++;
			}
			lineNo++;
		}
	}

	public static void viewTable(List<List<String>> table, int row_start, int row_end) {
		
		for (int r=row_start; r<=row_end; r++) {
			int columnNo = 0;
			for (String value : table.get(r)) {
				System.out.println("Line " + r + " Column " + columnNo + ": " + value);// Viewing Output
				columnNo++;
			}
		}
	}

	public static List<List<String>> sliceTableByContainer(List<List<String>> table, String container) {
		/* Ignored */
		int startIndex = 0;
		int endIndex = table.size();
	
		for (int r = 0; r <= table.size(); r++) {
			if (table.get(r).get(2).toString().equals(container)) {
				startIndex = r;
				break;
			} else {
				System.out.println("Container " + container + " doesn't exist");
				// throw new FileNotFoundException();
			}
		}
	
		for (int rr = startIndex; rr <= table.size(); rr++) {
			if (!table.get(rr).get(2).equals(container)) {
				endIndex = rr - 1;
				break;
			}
		}
	
		List<List<String>> slicedTable = table.subList(startIndex, endIndex);
		return slicedTable;
	}

	public static boolean containsString(String str, String fileName) {
		boolean isFound = false;
		
		try {
			File file = new File(fileName);
			Scanner s = new Scanner(file);
			while (s.hasNext()) {
				if (s.nextLine().contains(str)) {
					isFound = true;
				}
			}
			s.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		
		return isFound;
	}

	public static void writeListToTXT(List<String> list, String filename) {
		PrintWriter pw;
		try {
			pw = new PrintWriter(new File(filename));
	
			StringBuilder sb = new StringBuilder();
			for (String e : list) {
				sb.append(e);
				sb.append("\n");
			}
	
	        pw.write(sb.toString());
	        pw.close();
	//      System.out.println("done!");
	
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		}
	}

	public static List<String> readTXTtoList(String filename) {
		List<String> list = new ArrayList<String>();
		try {
			File file = new File(filename);
			Scanner s = new Scanner(file);
			while (s.hasNext()) {
				list.add(s.nextLine());
			}
			s.close();
			
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		}
		return list;
	}
}
