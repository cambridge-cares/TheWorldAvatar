package com.cmclinnovations.ontochem.model.converter.json;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 * Parses JSON files containing CompChem data and convert them into 
 * OWL ontologies. 
 * 
 * @author msff2
 *
 */
public class CompChem {
	public static void main(String[] args) throws IOException {
		CompChem cc = new CompChem();
		cc.parseCompChemData("C:/Users/msff2/Documents/Data/compchem/", "Cl2O6_nasa.json");
	}
	
	/**
	 * Parses a JSON file containing CompChem data and convert it
	 * into an OWL ontology.
	 * 
	 * @param filePath
	 * @param fileName
	 * @throws IOException
	 */
	public void parseCompChemData(String filePath, String fileName) throws IOException {
		JsonReader reader = Json.createReader(openSourceFile(filePath.concat(fileName)));
		JsonObject compChemObject = reader.readObject();
		reader.close();
		readMetaData(compChemObject);
		readData(compChemObject);
	}
	
	/**
	 * Reads metadata about the calculated thermo data.
	 * 
	 * @param compChemObject
	 */
	private void readMetaData(JsonObject compChemObject){
		readComment(compChemObject);
		readRunDate(compChemObject);
		readProgramName(compChemObject);
		readBasisSet(compChemObject);
		readProgramVersion(compChemObject);
		readPhase(compChemObject);
		readLevelOfTheory(compChemObject);
	}

	/**
	 * Reads thermo data of the species currently being processed.
	 * 
	 * @param compChemObject
	 */
	private void readData(JsonObject compChemObject){
		readName(compChemObject);
		readAtomicMasses(compChemObject);
		readHighTCoefficient(compChemObject);
		readComposition(compChemObject);
		readLowTCoefficient(compChemObject);
		readTMin(compChemObject);
		readTMid(compChemObject);
		readTMax(compChemObject);
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class. It takes
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	private InputStream openSourceFile(String filePathPlusName) throws IOException {
		return new FileInputStream(filePathPlusName);
	}
	
	/**
	 * Reads the comment
	 * 
	 * @param compChemObject
	 */
	private void readComment(JsonObject compChemObject){
		String comment = compChemObject.getString("Comment");
		System.out.println("Comment   : " + comment);
	}
	
	/**
	 * Reads the date when a program was run to calculate thermo data.
	 * 
	 * @param compChemObject
	 */
	private void readRunDate(JsonObject compChemObject){
		String runDate = compChemObject.getString("runDate");
		System.out.println("runDate   : " + runDate);
	}
	
	/**
	 * Reads the name of the program run to compute thermo data. 
	 * 
	 * @param compChemObject
	 */
	private void readProgramName(JsonObject compChemObject){
		String programName = compChemObject.getString("programName");
		System.out.println("programName   : " + programName);
	}
	
	/**
	 * Reads the name of the basis set used in the calculation.
	 * 
	 * @param compChemObject
	 */
	private void readBasisSet(JsonObject compChemObject){
		String basisSetValue = compChemObject.getString("basisSetValue");
		System.out.println("basisSetValue   : " + basisSetValue);
	}
	
	private void readProgramVersion(JsonObject compChemObject){
		String programVersion = compChemObject.getString("programVersion");
		System.out.println("programVersion   : " + programVersion);		
	}
	
	/**
	 * Reads the phase in which the current species belongs to.
	 * 
	 * @param compChemObject
	 */
	private void readPhase(JsonObject compChemObject){
		String phase = compChemObject.getString("Phase");
		System.out.println("Phase   : " + phase);
	}
	
	/**
	 * Reads the level of theory used to calculate thermo data.
	 * 
	 * @param compChemObject
	 */
	private void readLevelOfTheory(JsonObject compChemObject){
		String levelOfTheory = compChemObject.getString("levelOfTheory");
		System.out.println("LevelOfTheory   : " + levelOfTheory);
	}
	
	/**
	 * Reads the name of the species currently being parsed.
	 * 
	 * @param compChemObject
	 */
	private void readName(JsonObject compChemObject){
		String name = compChemObject.getString("Name");
		System.out.println("Name   : " + name);
	}
	
	/**
	 * Reads the atomic mass of those atoms that participate in the species
	 * currently being processed. 
	 * 
	 * @param compChemObject
	 */
	private void readAtomicMasses(JsonObject compChemObject){
		JsonArray atomicMassesArray = compChemObject.getJsonArray("atomicMasses");
		int i = 1;
		for (JsonValue jsonValue : atomicMassesArray) {
			if(i==1){
				JsonArray atoms = (JsonArray) jsonValue;
				for(JsonValue atom: atoms){
					String atomString = atom.toString();
					atomString = atomString.replace("\"", "");
					System.out.println("atom:"+atomString);
				}
			}
			if(i==2){
				JsonArray atoms = (JsonArray) jsonValue;
				for(JsonValue atom: atoms){
					double atomicMass = Double.parseDouble(atom.toString());
					System.out.println("atomic mass:"+atomicMass);
				}
			}
			i++;
		}
	}
	
	/**
	 * Reads the high temperature coefficient of the species currently 
	 * being processed. 
	 * 
	 * @param compChemObject
	 */
	private void readHighTCoefficient(JsonObject compChemObject){
		JsonArray highTCoeffArray = compChemObject.getJsonArray("highTcoeff");
		for (JsonValue jsonValue : highTCoeffArray) {
				double highTCoeffValue = Double.parseDouble(jsonValue.toString());
				System.out.println("highTCoeff:" + highTCoeffValue);
		}
	}
	
	/**
	 * Reads the composition of the species currently being processed.
	 * 
	 * @param compChemObject
	 */
	private void readComposition(JsonObject compChemObject){
		JsonArray composition = compChemObject.getJsonArray("Composition");
		for (JsonValue jsonValue : composition) {
				String compositionValue = jsonValue.toString();
				System.out.println("Composition:" + compositionValue);
		}
	}
	
	/**
	 * Reads the low temperature coefficient of the species currently 
	 * being processed.
	 * 
	 * @param compChemObject
	 */
	private void readLowTCoefficient(JsonObject compChemObject){
		JsonArray lowTcoeff = compChemObject.getJsonArray("LowTcoeff");
		for (JsonValue jsonValue : lowTcoeff) {
				double lowTcoeffValue = Double.parseDouble(jsonValue.toString());
				System.out.println("lowTcoeffValue:" + lowTcoeffValue);
		}
	}
	
	/**
	 * Reads the temperature below which calculated thermo data is not valid.
	 * 
	 * @param compChemObject
	 */
	private void readTMin(JsonObject compChemObject){
		double tMin = compChemObject.getJsonNumber("Tmin").doubleValue();
		System.out.println("TMin :"+tMin);
	}
	
	/**
	 * Reads the medium temperature value for computing thermo data.
	 * 
	 * @param compChemObject
	 */
	private void readTMid(JsonObject compChemObject){
		int tMid = compChemObject.getJsonNumber("Tmid").intValue();
		System.out.println("Tmid:"+tMid);

	}
	
	/**
	 * Reads the temperature above which calculated thermo data is not valid.
	 * 
	 * @param compChemObject
	 */
	private void readTMax(JsonObject compChemObject){
		int tMax = compChemObject.getJsonNumber("Tmax").intValue();
		System.out.println("Tmax:"+tMax);
	}
}
