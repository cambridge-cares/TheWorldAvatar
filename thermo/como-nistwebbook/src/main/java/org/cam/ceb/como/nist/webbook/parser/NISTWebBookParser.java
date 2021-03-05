package org.cam.ceb.como.nist.webbook.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.cam.ceb.como.nist.webbook.DownloadHTML;
import org.cam.ceb.como.nist.webbook.info.AppearanceEnergy;
import org.cam.ceb.como.nist.webbook.info.IonisationEnergy;
import org.cam.ceb.como.nist.webbook.info.NISTPressure;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import org.cam.ceb.como.nist.webbook.info.NISTTemperature;
import org.cam.ceb.como.nist.webbook.thermochem.NISTDensity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTElementGeometry;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfX;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpyOfXStCondition;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEntropy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTGibbsFreeEnergy;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHeatCapacity;
import org.cam.ceb.como.nist.webbook.thermochem.NISTHenrysLawConstant;
import org.cam.ceb.como.nist.webbook.thermochem.NISTVolume;

public class NISTWebBookParser {
	/**
	 * This file contains information about valence electrons of atoms.
	 */
	private static final String FILE_NAME_ATOM_INFO = "atom.csv";
	/**
	 * A map created to store the type of parsed file vs information extracted from the file. 
	 */
	Map<String, NISTSpeciesInfo> data = new HashMap<String, NISTSpeciesInfo>();
	
	static NISTSDFParser nistSDFParser;
	/**
	 * The default constructor
	 */
	public NISTWebBookParser() {
		data = new HashMap<String, NISTSpeciesInfo>();
		nistSDFParser = new NISTSDFParser();
	}
	
    // Gets the path of resources folder
    private String getFileFromResources(String fileName) throws IOException{
    	ClassLoader classLoader = NISTWebBookParser.class.getClassLoader();
        URL resource = classLoader.getResource(fileName);
        if (resource == null) {
            throw new IllegalArgumentException("file is not found!");
        } else {
            if(new File(resource.getFile()).exists()){
            	return new File(resource.getFile()).getAbsolutePath();
            }else{
            	return null;
            }
        }
    }
	
    /**
     * The main method of this class.
     * 
     * @param args
     * @throws IOException
     */
	public static void main(String[] args) throws IOException{
//		String sourceHTMLFile = "D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\";
//		String sourceStructureFile = "D:\\msff2\\Documents\\Data\\NIST\\download\\";
		String sourceHTMLFile = "C:\\Users\\msff2\\Documents\\c4e-jps-SelfGrowingKB\\Data\\html\\";
		String sourceStructureFile = "C:\\Users\\msff2\\Documents\\c4e-jps-SelfGrowingKB\\Data\\structure\\";
		NISTWebBookParser nistWebBookParser = new NISTWebBookParser();
		nistWebBookParser.parseNISTData(sourceHTMLFile, sourceStructureFile);
	}
	
	/**
	 * 
	 * 
	 * @throws IOException
	 */
	/**
	 * It parses all NIST species provided in the source HTML and</br>
	 * structure (SDF and MOL) file paths and saves extracted data into</br>
	 * an in-memory structure. 
	 * 
	 * @param sourceHTMLFilePath
	 * @param sourceStructureFilePath
	 * @throws IOException
	 */
	public Map<String, NISTSpeciesInfo> parseNISTData(String sourceHTMLFilePath, String sourceStructureFilePath) throws IOException{
		try{
			parseHTML(sourceHTMLFilePath);
			parseSDF(sourceStructureFilePath, getFileFromResources(FILE_NAME_ATOM_INFO));
		}catch(Exception e){
			e.printStackTrace();
		}
		for(String key:data.keySet()){
			NISTSpeciesInfo speciesInfo = data.get(key);
		}
		//display();
		return data;
	}
	
	/**
	 * Displays all property values extracted from NIST Chemistry WebBook. 
	 * 
	 */
	private void display(){
		for(String key:data.keySet()){
			NISTSpeciesInfo speciesInfo = data.get(key);
			DownloadHTML.display(speciesInfo);
			if(speciesInfo.getElectronicEnergy()!=null){
				System.out.println("Electronic energy value:"+speciesInfo.getElectronicEnergy().getValue());
				System.out.println("Electronic energy units:"+speciesInfo.getElectronicEnergy().getUnits());
			}
			System.out.println("Paired Electrons:"+speciesInfo.getPairedElectrons());
			System.out.println("Unpaired Electrons:"+speciesInfo.getUnpairedElectrons());
			System.out.println("Total number of valence Electrons:"+speciesInfo.getElectrons());
			if(speciesInfo.getBondType()!=null){
				for(int i=0;i<speciesInfo.getBondType().size();i++){
					for(String atom:speciesInfo.getBondType().get(i).keySet()){
						System.out.println("Atom "+(i+1)+ " has the following bond type(s):"+speciesInfo.getBondType().get(i).get(atom));
					}
				}
			}
			if(speciesInfo.getName()!=null && !speciesInfo.getName().isEmpty()){
				System.out.println("name:"+speciesInfo.getName());
			}
			if(speciesInfo.gettBoil()!=null){
				System.out.println("Boiling point temperature:"+speciesInfo.gettBoil().getValue());
				System.out.println("Boiling point temperature units:"+speciesInfo.gettBoil().getUnits());
				if(speciesInfo.gettBoil().getReference()!=null && !speciesInfo.gettBoil().getReference().isEmpty()){
					System.out.println("Boiling point temperature reference:"+speciesInfo.gettBoil().getReference());
				}
			}
			if(speciesInfo.gettCritical()!=null){
				for(NISTTemperature t:speciesInfo.gettCritical()){
					System.out.println("Critical temperature value:"+t.getValue());
					System.out.println("Critical temperature units:"+t.getUnits());
					if(t.getMethod()!=null){
						System.out.println("Critical temperature method:"+t.getMethod());
					}
					if(t.getReference()!=null){
						System.out.println("Critical temperature reference:"+t.getReference());
					}
					if(t.getComment()!=null){
						System.out.println("Critical temperature comment:"+t.getComment());
					}
				}
			}
			if(speciesInfo.getpCritical()!=null){
				for(NISTPressure p:speciesInfo.getpCritical()){
					System.out.println("Critical pressure value:"+p.getValue());
					System.out.println("Critical pressure units:"+p.getUnits());
					if(p.getMethod()!=null){
						System.out.println("Critical pressure method:"+p.getMethod());
					}
					if(p.getReference()!=null){
						System.out.println("Critical pressure reference:"+p.getReference());
					}
					if(p.getComment()!=null){
						System.out.println("Critical pressure comment:"+p.getComment());
					}
				}
			}
			if(speciesInfo.getvCritical()!=null){
				for(NISTVolume v:speciesInfo.getvCritical()){
					System.out.println("Critical volume value:"+v.getValue());
					System.out.println("Critical volume units:"+v.getUnits());
					if(v.getMethod()!=null){
						System.out.println("Critical volume method:"+v.getMethod());
					}
					if(v.getReference()!=null){
						System.out.println("Critical volume reference:"+v.getReference());
					}
					if(v.getComment()!=null){
						System.out.println("Critical volume comment:"+v.getComment());
					}
				}
			}
			if(speciesInfo.getρCritical()!=null){
				for(NISTDensity d:speciesInfo.getρCritical()){
					System.out.println("Critical density value:"+d.getValue());
					System.out.println("Critical density units:"+d.getUnits());
					if(d.getMethod()!=null){
						System.out.println("Critical density method:"+d.getMethod());
					}
					if(d.getReference()!=null){
						System.out.println("Critical density reference:"+d.getReference());
					}
					if(d.getComment()!=null){
						System.out.println("Critical density comment:"+d.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfFormationInGas()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfFormationInGas()){
					System.out.println("Enthalpy of formation of gas at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of formation of gas at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of formation of gas at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of formation of gas at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of formation of gas at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfFormationInLiquid()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfFormationInLiquid()){
					System.out.println("Enthalpy of formation of liquid at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of formation of liquid at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of formation of liquid at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of formation of liquid at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of formation of liquid at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfFormationInSolid()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfFormationInSolid()){
					System.out.println("Enthalpy of formation of solid at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of formation of solid at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of formation of solid at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of formation of solid at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of formation of solid at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfCombustionInGas()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfCombustionInGas()){
					System.out.println("Enthalpy of combustion of gas at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of combustion of gas at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of combustion of gas at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of combustion of gas at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of combustion of gas at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfCombustionInLiquid()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfCombustionInLiquid()){
					System.out.println("Enthalpy of combustion of liquid at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of combustion of liquid at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of combustion of liquid at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of combustion of liquid at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of combustion of liquid at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfCombustionInSolid()!=null){
				for(NISTEnthalpy eOf:speciesInfo.getEnthalpyOfCombustionInSolid()){
					System.out.println("Enthalpy of combustion of solid at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of combustion of solid at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of combustion of solid at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of combustion of solid at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of combustion of solid at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyInGasAtStCondition()!=null){
				for(NISTEntropy entropy:speciesInfo.getEntropyInGasAtStCondition()){
					System.out.println("Entropy in gas at stadard conditions value:"+entropy.getValue());
					System.out.println("Entropy in gas at stadard conditions units:"+entropy.getUnits());
					if(entropy.getMethod()!=null){
						System.out.println("Entropy in gas at stadard conditions method:"+entropy.getMethod());
					}
					if(entropy.getReference()!=null){
						System.out.println("Entropy in gas at stadard conditions reference:"+entropy.getReference());
					}
					if(entropy.getComment()!=null){
						System.out.println("Entropy in gas at stadard conditions comment:"+entropy.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyInLiquidAtStCondition()!=null){
				for(NISTEntropy entropy:speciesInfo.getEntropyInLiquidAtStCondition()){
					System.out.println("Entropy in liquid at stadard conditions value:"+entropy.getValue());
					System.out.println("Entropy in liquid at stadard conditions units:"+entropy.getUnits());
					if(entropy.getMethod()!=null){
						System.out.println("Entropy in liquid at stadard conditions method:"+entropy.getMethod());
					}
					if(entropy.getReference()!=null){
						System.out.println("Entropy in liquid at stadard conditions reference:"+entropy.getReference());
					}
					if(entropy.getComment()!=null){
						System.out.println("Entropy in liquid at stadard conditions comment:"+entropy.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyInSolidAtStCondition()!=null){
				for(NISTEntropy entropy:speciesInfo.getEntropyInSolidAtStCondition()){
					System.out.println("Entropy in solid at stadard conditions value:"+entropy.getValue());
					System.out.println("Entropy in solid at stadard conditions units:"+entropy.getUnits());
					if(entropy.getMethod()!=null){
						System.out.println("Entropy in solid at stadard conditions method:"+entropy.getMethod());
					}
					if(entropy.getReference()!=null){
						System.out.println("Entropy in solid at stadard conditions reference:"+entropy.getReference());
					}
					if(entropy.getComment()!=null){
						System.out.println("Entropy in solid at stadard conditions comment:"+entropy.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfVapAtStC()!=null){
				for(NISTEnthalpyOfXStCondition eOf:speciesInfo.getEnthalpyOfVapAtStC()){
					System.out.println("Enthalpy of vaporisation at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of vaporisation at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of vaporisation at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of vaporisation at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of vaporisation at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfSubAtStC()!=null){
				for(NISTEnthalpyOfXStCondition eOf:speciesInfo.getEnthalpyOfSubAtStC()){
					System.out.println("Enthalpy of sublimation at stadard conditions value:"+eOf.getValue());
					System.out.println("Enthalpy of sublimation at stadard conditions units:"+eOf.getUnits());
					if(eOf.getMethod()!=null){
						System.out.println("Enthalpy of sublimation at stadard conditions method:"+eOf.getMethod());
					}
					if(eOf.getReference()!=null){
						System.out.println("Enthalpy of sublimation at stadard conditions reference:"+eOf.getReference());
					}
					if(eOf.getComment()!=null){
						System.out.println("Enthalpy of sublimation at stadard conditions comment:"+eOf.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpiesOfVap()!=null){
				for(NISTEnthalpyOfX e:speciesInfo.getEnthalpiesOfVap()){
					System.out.println("Enthalpy of vaporisation value:"+e.getValue());
					System.out.println("Enthalpy of vaporisation units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Enthalpy of vaporisation temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Enthalpy of vaporisation temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Enthalpy of vaporisation temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Enthalpy of vaporisation method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Enthalpy of vaporisation reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Enthalpy of vaporisation comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getHenrysLawConstant()!=null){
				for(NISTHenrysLawConstant e:speciesInfo.getHenrysLawConstant()){
					System.out.println("Henry's Law Constant value:"+e.getValue());
					System.out.println("Henry's Law Constant units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Henry's Law Constant temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Henry's Law Constant temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Henry's Law Constant units:"+e.getTemperature().getUnits());
						}
					}
					System.out.println("Henry's Law Constant temp dependent constant value:"+e.getTemperatureDependentConstant());
					System.out.println("Henry's Law Constant solunation name:"+e.getSolution());
					if(e.getMethod()!=null){
						System.out.println("Henry's Law Constant calculation method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Henry's Law Constant reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Henry's Law Constant comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpiesOfSub()!=null){
				for(NISTEnthalpyOfX e:speciesInfo.getEnthalpiesOfSub()){
					System.out.println("Enthalpy of sublimation value:"+e.getValue());
					System.out.println("Enthalpy of sublimation units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Enthalpy of sublimation temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Enthalpy of sublimation temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Enthalpy of sublimation temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Enthalpy of sublimation method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Enthalpy of sublimation reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Enthalpy of sublimation comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpiesOfFus()!=null){
				for(NISTEnthalpyOfX e:speciesInfo.getEnthalpiesOfFus()){
					System.out.println("Enthalpy of fusion value:"+e.getValue());
					System.out.println("Enthalpy of fusion units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Enthalpy of fusion temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Enthalpy of fusion temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Enthalpy of fusion temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Enthalpy of fusion method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Enthalpy of fusion reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Enthalpy of fusion comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyOfFusion()!=null){
				for(NISTEntropy e:speciesInfo.getEntropyOfFusion()){
					System.out.println("Entropy of fusion value:"+e.getValue());
					System.out.println("Entropy of fusion units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Entropy of fusion temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Entropy of fusion temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Entropy of fusion temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Entropy of fusion method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Entropy of fusion reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Entropy of fusion comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEnthalpyOfReactions()!=null){
				for(NISTEnthalpy e:speciesInfo.getEnthalpyOfReactions()){
					System.out.println("Enthalpy of reaction equation:"+e.getReaction());
					if(e.getPosTolerance()!=0 && e.getNegTolerance()!=0 && e.getPosTolerance()==e.getNegTolerance()){
						System.out.println("Enthalpy of reaction value:"+e.getValue() + " ± " + e.getPosTolerance());
					}else{
						System.out.println("Enthalpy of reaction value:"+e.getValue());
					}
					System.out.println("Enthalpy of reaction units:"+e.getUnits());
					if(e.getMethod()!=null){
						System.out.println("Enthalpy of reaction method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Enthalpy of reaction reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Enthalpy of reaction comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyOfReactions()!=null){
				for(NISTEntropy e:speciesInfo.getEntropyOfReactions()){
					System.out.println("Entropy of reaction equation:"+e.getReaction());
					if(e.getPosTolerance()!=0 && e.getNegTolerance()!=0 && e.getPosTolerance()==e.getNegTolerance()){
						System.out.println("Entropy of reaction value:"+e.getValue() + " ± " + e.getPosTolerance());
					}else{
						System.out.println("Entropy of reaction value:"+e.getValue());
					}
					System.out.println("Entropy of reaction units:"+e.getUnits());
					if(e.getMethod()!=null){
						System.out.println("Entropy of reaction method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Entropy of reaction reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Entropy of reaction comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getGibbsFreeEnergyOfReactions()!=null){
				for(NISTGibbsFreeEnergy e:speciesInfo.getGibbsFreeEnergyOfReactions()){
					System.out.println("Gibbs free energy of reaction equation:"+e.getReaction());
					if(e.getPosTolerance()!=0 && e.getNegTolerance()!=0 && e.getPosTolerance()==e.getNegTolerance()){
						System.out.println("Gibbs free energy of reaction value:"+e.getValue() + " ± " + e.getPosTolerance());
					}else{
						System.out.println("Gibbs free energy of reaction value:"+e.getValue());
					}
					System.out.println("Gibbs free energy of reaction units:"+e.getUnits());
					if(e.getMethod()!=null){
						System.out.println("Gibbs free energy of reaction method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Gibbs free energy of reaction reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Gibbs free energy of reaction comment:"+e.getComment());
					}
				}
			}			
			if(speciesInfo.getEntropyOfSublimation()!=null){
				for(NISTEntropy e:speciesInfo.getEntropyOfSublimation()){
					System.out.println("Entropy of sublimation value:"+e.getValue());
					System.out.println("Entropy of sublimation units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Entropy of sublimation temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Entropy of sublimation temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Entropy of sublimation temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Entropy of sublimation method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Entropy of sublimation reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Entropy of sublimation comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getEntropyOfVaporisation()!=null){
				for(NISTEntropy e:speciesInfo.getEntropyOfVaporisation()){
					System.out.println("Entropy of vaporisation value:"+e.getValue());
					System.out.println("Entropy of vaporisation units:"+e.getUnits());
					if(e.getTemperature()!=null){
						if(e.getTemperature()!=null && e.getTemperature().getUnits()!=null){
							if(e.getTemperature().getPosTolerance()!=0 && e.getTemperature().getNegTolerance()!=0 && e.getTemperature().getPosTolerance() == e.getTemperature().getNegTolerance()){
								System.out.println("Entropy of vaporisation temperature value:"+e.getTemperature().getValue() + " ± "+ e.getTemperature().getPosTolerance());
							}else{
								System.out.println("Entropy of vaporisation temperature value:"+e.getTemperature().getValue());
							}
							System.out.println("Entropy of vaporisation temperature units:"+e.getTemperature().getUnits());
						}
					}
					if(e.getMethod()!=null){
						System.out.println("Entropy of vaporisation method:"+e.getMethod());
					}
					if(e.getReference()!=null){
						System.out.println("Entropy of vaporisation reference:"+e.getReference());
					}
					if(e.getComment()!=null){
						System.out.println("Entropy of vaporisation comment:"+e.getComment());
					}
				}
			}
			if(speciesInfo.getpTriple()!=null){
				for(NISTPressure p:speciesInfo.getpTriple()){
					System.out.println("Triple point pressure value:"+p.getValue());
					System.out.println("Triple point pressure units:"+p.getUnits());
					if(p.getMethod()!=null){
						System.out.println("Triple point pressure method:"+p.getMethod());
					}
					if(p.getReference()!=null){
						System.out.println("Triple point pressure reference:"+p.getReference());
					}
					if(p.getComment()!=null){
						System.out.println("Triple point pressure comment:"+p.getComment());
					}
				}
			}
			if(speciesInfo.gettTriple()!=null){
				for(NISTTemperature t:speciesInfo.gettTriple()){
					System.out.println("Triple point temperature value:"+t.getValue());
					System.out.println("Triple point temperature units:"+t.getUnits());
					if(t.getMethod()!=null){
						System.out.println("Triple point temperature method:"+t.getMethod());
					}
					if(t.getReference()!=null){
						System.out.println("Triple point temperature reference:"+t.getReference());
					}
					if(t.getComment()!=null){
						System.out.println("Triple point temperature comment:"+t.getComment());
					}
				}
			}
			if(speciesInfo.gettFusion()!=null){
				System.out.println("Fusion (or melting) temperature:"+speciesInfo.gettFusion().getValue());
				System.out.println("Fusion (or melting) temperature units:"+speciesInfo.gettFusion().getUnits());
				if(speciesInfo.gettFusion().getReference()!=null && !speciesInfo.gettFusion().getReference().isEmpty()){
					System.out.println("Fusion (or melting) temperature reference:"+speciesInfo.gettFusion().getReference());
				}
			}
			if(speciesInfo.getPhase()!=null && !speciesInfo.getPhase().trim().isEmpty()){
				System.out.println("Phase:"+speciesInfo.getPhase());
			}
			if(speciesInfo.getAppearanceEnergy()!=null){
				for(AppearanceEnergy appE:speciesInfo.getAppearanceEnergy()){
					System.out.println("Appearance Energy Ion:"+appE.getIon());
					System.out.println("Appearance Energy Value:"+appE.getValue());
					System.out.println("Appearance Energy Units:"+appE.getUnits());
					System.out.println("Appearance Energy Reference:"+appE.getReference());
				}
			}
			if(speciesInfo.getIonisationEnergy()!=null){
				for(IonisationEnergy ionE:speciesInfo.getIonisationEnergy()){
					System.out.println("Ionisation Energy Value:"+ionE.getValue());
					System.out.println("Ionisation Energy Units:"+ionE.getUnits());
					System.out.println("Ionisation Energy Reference:"+ionE.getReference());
					System.out.println("Ionisation Energy Comment:"+ionE.getComment());
				}
			}
			if(speciesInfo.getHeatCapacityOfGas()!=null){
				for(NISTHeatCapacity nhc:speciesInfo.getHeatCapacityOfGas()){
					System.out.println("Gas-Phase Heat Capacity Value:"+nhc.getValue());
					System.out.println("Gas-Phase Heat Capacity Units:"+nhc.getUnits());
					if(nhc.getTemperature()!=null){
						System.out.println("Gas-Phase Heat Capacity Temperature Value:"+nhc.getTemperature().getValue());
						System.out.println("Gas-Phase Heat Capacity Temperature Units:"+nhc.getTemperature().getUnits());
					}
					System.out.println("Gas-Phase Heat Capacity Reference:"+nhc.getReference());
					System.out.println("Gas-Phase Heat Capacity Comment:"+nhc.getComment());
				}
			}
			if(speciesInfo.getHeatCapacityOfLiquid()!=null){
				for(NISTHeatCapacity nhc:speciesInfo.getHeatCapacityOfLiquid()){
					System.out.println("Liquid-Phase Heat Capacity Value:"+nhc.getValue());
					System.out.println("Liquid-Phase Heat Capacity Units:"+nhc.getUnits());
					if(nhc.getTemperature()!=null){
						System.out.println("Liquid-Phase Heat Capacity Temperature Value:"+nhc.getTemperature().getValue());
						System.out.println("Liquid-Phase Heat Capacity Temperature Units:"+nhc.getTemperature().getUnits());
					}
					System.out.println("Liquid-Phase Heat Capacity Reference:"+nhc.getReference());
					System.out.println("Liquid-Phase Heat Capacity Comment:"+nhc.getComment());
				}
			}
			if(speciesInfo.getHeatCapacityOfSolid()!=null){
				for(NISTHeatCapacity nhc:speciesInfo.getHeatCapacityOfSolid()){
					System.out.println("Solid-Phase Heat Capacity Value:"+nhc.getValue());
					System.out.println("Solid-Phase Heat Capacity Units:"+nhc.getUnits());
					if(nhc.getTemperature()!=null){
						System.out.println("Solid-Phase Heat Capacity Temperature Value:"+nhc.getTemperature().getValue());
						System.out.println("Solid-Phase Heat Capacity Temperature Units:"+nhc.getTemperature().getUnits());
					}
					System.out.println("Solid-Phase Heat Capacity Reference:"+nhc.getReference());
					System.out.println("Solid-Phase Heat Capacity Comment:"+nhc.getComment());
				}
			}
			if(speciesInfo.getSpeciesGeometry()!=null){
				for(NISTElementGeometry neg:speciesInfo.getSpeciesGeometry().getValue()){
					System.out.println("Element symbol:"+neg.getElement());
					System.out.println("Element X coordinate:"+neg.getX());
					System.out.println("Element Y coordinate:"+neg.getY());
					System.out.println("Element Z coordinate:"+neg.getZ());
				}
			}
			if(speciesInfo.getDipoleMoment()!=null){
				System.out.println("Species dipole moment value:"+speciesInfo.getDipoleMoment().getValue());
				System.out.println("Species dipole moment units:"+speciesInfo.getDipoleMoment().getUnits());
			}		
			System.out.println(" - - -  - - - -  - - - - - -  - - - - - - - -");
		}
	}
	
	/**
	 * Scans the current HTML file to extract information of interest. 
	 * 
	 * @param htmlFolderPath
	 * @throws Exception
	 */
	public void parseHTML(String htmlFolderPath) throws Exception{
		if(htmlFolderPath!=null && !htmlFolderPath.isEmpty())
		{
			DownloadHTML.parsingHTML(htmlFolderPath, data);
		}
	}
	
	/**
	 * Scans the current SDF file to extract information of interest. 
	 * 
	 * @param sdfFolderPath
	 * @throws Exception
	 */
	public void parseSDF(String sdfFolderPath, String pathToAtoms) throws Exception{
		if(sdfFolderPath!=null && !sdfFolderPath.isEmpty())
		{
			nistSDFParser.setPathToAtoms(pathToAtoms);
			nistSDFParser.parseSDF(sdfFolderPath, data);
		}
	}

}
