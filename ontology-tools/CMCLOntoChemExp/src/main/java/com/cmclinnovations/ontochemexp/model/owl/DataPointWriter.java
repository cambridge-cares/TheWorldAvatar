
package com.cmclinnovations.ontochemexp.model.owl;

import org.apache.commons.lang3.text.WordUtils;
import org.xml.sax.SAXException;
import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * Implements the method that forwards a call to those methods that read
 * DataPoint data from an in-memory temporary storage to pass them to the
 * corresponding PrIMe to OWL conversion methods.
 * 
 * 
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */
///////////////////////////////// NOT IN USE
public class DataPointWriter extends PrimeConverter implements IDataPointWriter {
	int othercount = 0;
	int count = 0;
	int specificationCount = 0;
	int specificationSootYieldCount = 0;
	int ignitionTimeDelayCount = 0;
	int propertyCount=0;
	private String nameSpecification;
	private String oldNameSpecification;
	private String oldDataGroupLabel;
	private String xName;

	
	
	

	public void writer(char ch[], int start, int length) throws SAXException {
		// System.out.println("+++++++++++++++++++++++++++");
		readDataPoint(ch, start, length);
		linkDataPointToDataGroup();
		// System.out.println("+++++++++++++++++++++++++++");
		// dataGroupDataPointXList.add(x);
		// dataGroupDataPoint.setX(dataGroupDataPointXList);
		// dataGroupDataPointList.add(dataGroupDataPoint);
		// dataGroup.setDataPoint(dataGroupDataPointList);
	}
	
 

	
	private void linkDataPointToDataGroup() {
		// System.out.println(ontoChemExpVocabulary.getClassResult());
		try {
			iABoxManagement.addObjectProperty("belongsToDataGroup", "DataPoint", "DataGroup");

		//	System.out.println("//////////////////" + value);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between a copyrigh and an experiment conducted using it.");
		}
	}
	/**
	 * Forwards the call to the methods that first read and then write DataPoint
	 * data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readDataPoint(char ch[], int start, int length) throws SAXException {

		// x1.setValue("ABD");

		// System.out.println("+++++++++++++++++++++++++++x.getNameProperty():" +
		// x.getNameProperty());

		// if(dataGroup.getLabel() != null) {
		// oldDataGroupLabel=dataGroup.getLabel();
		// }

		// if (x.getName() != null && x.getNameProperty().contains("omposition")) {
		// System.out.println("+++#####+++++++++++++++++++x.getNameProperty():" +
		// x.getName());
		// x.setName(x.getName().replaceAll("Concentration", "Composition"));
		// System.out.println("+++#####+++++++++++++++++++x.getNameProperty():" +
		// x.getName());
		// }

		// System.out.println("######+++++++++++++++++++++:" + x.getName());
		// System.out.println("######+++++++++++++++++++++:" + x.getValue());
		
		/**
		if (x.getName() == null && x.getOrigName() != null) {
			x.setName(WordUtils.capitalize(x.getOrigName()));
			x.setDescription(WordUtils.capitalize(x.getOrigName()));
		} 
		if (x.getName() == null) {
 		//	 System.out.println("IN######+++++++++++++++++++++:" + x.getName());
//			 System.out.println("IN######+++++++++++++++++++++:" + x.getValue());
			if (x.getDescription() != null)
			if (x.getDescription().replaceAll(SPACE, UNDERSCORE).contains("Ignition")) {
				//nameSpecification = dataGroup.getLabel().replaceAll("_", "") + "Specification";
				// nameSpecification = WordUtils.capitalize(x.getDescription().replaceAll(SPACE,
				// UNDERSCORE)) + "Specification";
			} else if (x.getName().replaceAll(SPACE, UNDERSCORE).contains("Ignition")) {
				nameSpecification = "IgnitionDelaySpecification";
			} else if (x.getName().replaceAll(SPACE, UNDERSCORE).contains("Speed")) {
				nameSpecification = "LaminarFlameSpeedSpecification";
			} else if (x.getName().replaceAll(SPACE, UNDERSCORE).contains("]/[")) {
				nameSpecification = "CharacteristicTimeSpecification";
			} else if (x.getName().replaceAll(SPACE, UNDERSCORE).contains("Concentration")) {
				nameSpecification = x.getName().replaceAll("Concentration", "").replaceAll("Profile", "").replaceAll("Species", "") + "SpeciesProfileSpecification";
			} else if (dataGroup.getLabel().equalsIgnoreCase("TProfile")
					|| dataGroup.getLabel().equalsIgnoreCase("TemperatureProfile")
					|| dataGroup.getLabel().equalsIgnoreCase("")) {
				nameSpecification = "TemperatureProfileSpecification";
			} else if (dataGroup.getLabel().contains("Profile")) {
				nameSpecification = dataGroup.getLabel().replaceAll("Profile", "").replaceAll("Species", "") + "SpeciesProfileSpecification";
				// System.out.println("##################"+dataGroup.getLabel());
			} else if (dataGroup.getLabel().contains("Max") || dataGroup.getLabel().equalsIgnoreCase("NO")) {
				nameSpecification = dataGroup.getLabel().replaceAll("Max.", "") + "SpeciesMaximumSpecification";
			} else if (dataGroup.getLabel().contains("alibration")) {
				nameSpecification = dataGroup.getLabel() + "Specification";
			} else if (dataGroup.getLabel() != null && dataGroup.getLabel() != "") {
				nameSpecification = dataGroup.getLabel().replaceAll("_", "") + "Specification";
				// System.out.println("+++++++++++++++++++++++++++Specification:" +
				// nameSpecification);
			}

		}*/
		
		/**
		if (dataPointParseStatus.isDataPoint()) {
			String value = multiLineValue.toString();
			// System.out.println("+++++++++++++++++++++++++++x.getName().replaceAll(SPACE,
			// UNDERSCORE):" + value.replaceAll(SPACE, ""));
			int DataPointNumber = x.getDataPointNumber();
			if (value.replaceAll(SPACE, "").matches("-?\\d+(\\.\\d+)?") || value.replaceAll(SPACE, "").contains("e-")
					|| value.replaceAll(SPACE, "").contains("E-")|| value.replaceAll(SPACE, "").contains("e+")|| value.replaceAll(SPACE, "").contains("E+")) {

				// System.out.println("Is a number");
				x.setValue(value.replaceAll(SPACE, ""));
				value = value.replaceAll(SPACE, "");
				String qName = x.getqName();
				
				// System.out.println("@@@@@@@@@@@@@@@x.getqName():"+ x.getqName());
				// System.out.println("@@@@@@@@@@@@@@@x.getDataPointNumber():"+ x.getDataPointNumber());
				// if (qName.equals("x1"))
				// x1.setValue(value.replaceAll(SPACE, ""));
				// if (value != null)
				// System.out.println("dataGroupParseStatus.getDataGroupNumber()++++++++:"+ dataGroupParseStatus.getDataGroupCount());
				// System.out.println("x.getDataPointNumber()++++++++:"+ x.getDataPointNumber());
				// System.out.println("@@@@@@@@@@@@@@@property.getId():"+ property.getId());
				try {
					iABoxManagement.addObjectProperty("belongsToDataGroup", "DataPoint"+"_"+x.getDataPointNumber(), "DataGroup"+"_"+dataGroupParseStatus.getDataGroupCount());
					iABoxManagement.addObjectProperty("hasDataGroup", experimentName, "DataGroup"+"_"+dataGroupParseStatus.getDataGroupCount());
				//	iABoxManagement.addObjectProperty("belongsToExperiment", "DataPoint"+"_"+x.getDataPointNumber(),experimentName);

				} catch (ABoxManagementException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
//				int numberOfVariable = Integer.parseInt(property.getId().replace("x", ""));
				// System.out.println("@@@@@@@@@@@@@@@qName:"+ qName);
				if(qName!=null){
				switch (qName) {
				case "x1":
					// System.out.println("@@@@@@@@@@@@@@@value:"+ value);
					x1.setValue(value);
					// System.out.println("@@@@@@@@@@@@@@@x1.getValue():"+ x1.getValue());
					dataPoint.setX1(x1);
					//System.out.println("@@@@@@@@@@@@@@@x.getDataPointNumber():"+ x.getDataPointNumber());
					
//					 propertyCount = property.getPropertyCount()-(numberOfVariable-1);
					try {
//						if(value.contains("E-") || value.contains("e-")){}
//						else{
							iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
							iABoxManagement.addObjectProperty("hasPropertyOf"+"X1", "DataPoint", "Property");
						 	iABoxManagement.addObjectProperty("hasPropertyOf"+"X1", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
//						}
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x2":
					x2.setValue(value);
					dataPoint.setX2(x2);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-2);
				//	System.out.println("x2++++++++:"+ qName);
				// System.out.println("x2++++++++:"+ x2.getValue());
					// System.out.println("DataPointNumber++++++++:"+ DataPointNumber);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X2", "DataPoint", "Property");
					 	iABoxManagement.addObjectProperty("hasPropertyOf"+"X2", "DataPoint_"+  DataPointNumber, "Property_" +  propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x3":
					x3.setValue(value);
					dataPoint.setX3(x3);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-3);
					
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X3", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X3", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x4":
					x4.setValue(value);
					dataPoint.setX4(x4);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-4);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X4", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X4", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x5":
					x5.setValue(value);
					dataPoint.setX5(x5);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-5);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X5", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X5", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x6":
					x6.setValue(value);
					dataPoint.setX6(x6);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-6);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X6", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X6", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x7":
					x7.setValue(value);
					dataPoint.setX7(x7);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-7);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X7", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X7", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x8":
					x8.setValue(value);
					dataPoint.setX8(x8);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-8);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X8", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X8", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x9":
					x9.setValue(value);
					dataPoint.setX9(x9);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-9);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X9", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X9", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x10":
					x10.setValue(value);
					dataPoint.setX10(x10);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-10);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X10", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X10", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				case "x11":
					x11.setValue(value);
					dataPoint.setX11(x11);
//					propertyCount = property.getPropertyCount()-(numberOfVariable-11);
					try {
						iABoxManagement.addProperty("DataPoint" + "_" + DataPointNumber, "has" + WordUtils.capitalize(qName), value, FLOAT);
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X11", "DataPoint", "Property");
						iABoxManagement.addObjectProperty("hasPropertyOf"+"X11", "DataPoint_"+  DataPointNumber, "Property_" + propertyCount);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					break;
				}
				}

				// System.out.println("+##getValue++++++++++++++++++++++++++:" + x.getValue());
				// System.out.println("+##getqName++++++++++++++++++++++++++:" + x.getqName());
				// if (nameSpecification != null && oldNameSpecification != null &&
				// oldNameSpecification != nameSpecification) count=0;
				// if (nameSpecification != null) oldNameSpecification = nameSpecification;

				if (dataGroup.getLabel() != null && oldDataGroupLabel != null
						&& oldDataGroupLabel != dataGroup.getLabel()) {
					specificationCount = 0; // New Data Group
					count = 0;
					// multiple dataGroup in one XML file
					// dataGroupPropertyUncertainty.setBound(null);
					// System.out.println("+++++++++++++++++++++++++++oldDataGroupLabel:" +
					// oldDataGroupLabel);
					// System.out.println("+++++++++++++++++++++++++++dataGroup.getLabel:" +
					// dataGroup.getLabel());
				}
				if (dataGroup.getLabel() != null)
					oldDataGroupLabel = dataGroup.getLabel();

//				if (nameSpecification != null)
//					createDataPoint();
			}
		}*/
	}

	/**
	 * Following the detection of the DataPoint element, it creates a data property
	 * for each individual in datagroup.
	 * 
	 */
	private void createDataPoint() {
	//	System.out.println("+++++++++++++++++++++++++++:" + x.getName());
		
		

		/**
		if (x.getName() != null)
			try {
				 // System.out.println("+++++++++++++++++++++++++++:" + x.getName());
				// if (nameSpecification.contains("SootYield")) {
				// xName=x.getName();
				// } else
				if (count == 0) {
					xName = x.getName();
					specificationCount = 1;
				}

				if (count == 0 && nameSpecification != null && !nameSpecification.contains("SootYield")) {
					specificationSootYieldCount = 0;
				}
				// System.out.println("1+++++++++++++++x.getName()+++++++++++:" + x.getName());
				// System.out.println("1+++++++++++++++xName+++++++++++:" + xName);
				// System.out.println("1+++++++++++++++ x.getValue()++check+++++++++:" +
				// x.getValue());
				// System.out.println("1++++++++++++++++nameSpecification+++++++++++:" +
				// nameSpecification);
				// System.out.println("1++++++++++++++++count+++++++++++:" + count);
				if (nameSpecification != null && xName.equalsIgnoreCase(x.getName())
						&& nameSpecification.contains("SootYield")) {
					specificationSootYieldCount = specificationSootYieldCount + 1;
					// System.out.println("+++++++++++++++x.getName()+++++++++++:" + x.getName());
					// System.out.println("++++++++++++++Check+++++++++++++:" + x.getValue());
					// System.out.println("+++++++++++++++specificationSootYieldCount+++++++++++:" +
					// specificationSootYieldCount);
					// System.out.println("+++++++++++++++specificationCount+++++++++++:" +
					// specificationCount);
				}
				// System.out.println("+++++++++++++++specificationSootYieldCount+++++++++++:" +
				// specificationSootYieldCount);

				// System.out.println("+++++++++++++++xName+++++++++++:" + xName);
				// System.out.println("+++++++++++++++x.getName()+++++++++++:" + x.getName());
				// System.out.println("+++++++++++++++xName.equalsIgnoreCase(x.getName())+++++++++++:"
				// + xName.equalsIgnoreCase(x.getName()));
				if (x.getName() != null && xName != null && xName.equalsIgnoreCase(x.getName()) && count > 0) {
					specificationCount = specificationCount + 1;
					// System.out.println("+@@@++++++++++++++++++++++++++:" + x.getName());
					// System.out.println("+@@@++++++++++++++++++++++++++:" + x.getValue());

					count = 0; // number of individuals in the specification
				}

				// System.out.println("+++++++++++++++count+++++++++++:" + count);

				count = count + 1;
				//
				//
				// System.out.println("+++++++++++++++++++++++++++:" + x.getValue());
				// System.out.println("+++++++++++++++++++++++++++:" + x.getName());
				// System.out.println("+++++++++++++++++++++++++++:" + x.getUnit());

				// System.out.println("££+++++++++++++++++++++++++++:" + specificationCount);
				// System.out.println("+++++++++++++++++++++++++++:" + X.);

				// if (x.getName().replaceAll(SPACE, UNDERSCORE).contains("Ignition")
				// || x.getName().replaceAll(SPACE, UNDERSCORE).contains("Concentration")
				// || x.getName().replaceAll(SPACE, UNDERSCORE).contains("Speed")) {
				ignitionTimeDelayCount = specificationCount;
				// System.out.println("++++++++++++++++++111+++++++++:" + x.getName());
				if (nameSpecification != null && nameSpecification.contains("SootYield")) {
					iABoxManagement.createIndividual(nameSpecification,
							nameSpecification + specificationSootYieldCount);
				} else {
					iABoxManagement.createIndividual(nameSpecification, nameSpecification + specificationCount);
					// System.out.println("++++++++++++++++++111+++++++++:" + nameSpecification);
				}

				if (x.getName().contains("oncentration")) {
					iABoxManagement.addProperty(nameSpecification + specificationCount,
							"has" + x.getName().replaceAll("Concentration", x.getNameProperty()), x.getValue(), FLOAT);
					iABoxManagement.addProperty(nameSpecification + specificationCount,
							"has" + x.getName().replaceAll("Concentration", x.getNameProperty()) + "Unit", x.getUnit(),
							STRING);
				} else if (nameSpecification.contains("SootYield")) {
					iABoxManagement.addProperty(nameSpecification + specificationSootYieldCount,
							"has" + x.getName().replaceAll("\\*E\\(m\\)", ""), x.getValue(), FLOAT);
					// System.out.println("x.getUnit()+++++++++++++++++++++++++++:" + x.getUnit());
					if (x.getUnit() != null) {
						iABoxManagement.addProperty(dataGroup.getLabel().replace("Case", ""),
								"has" + x.getName() + "Unit", x.getUnit(), STRING);
						
						// System.out.println("dataGroup.getLabel().replace(\"Case\",
						// \"\").replaceAll(\"[0-9]\", \"\").replaceAll(\"_\",
						// \"\")+++++++++++++++++++++++++++:" + dataGroup.getLabel().replace("Case",
						// "").replaceAll("[0-9]", "").replaceAll("_", ""));
						// System.out.println("x.getName()+++++++++++++++++++++++++++:" + x.getName());
					}

					iABoxManagement.createIndividual(nameSpecification.replaceAll("Specification", "")
							.replaceAll("_", ""), dataGroup.getLabel());
					// System.out.println("+++++++++++++++++++++++++++:" + nameSpecification +
					// specificationSootYieldCount);
					iABoxManagement.addObjectProperty(
							"has" + dataGroup.getLabel().replace("Case", "").replaceAll("_", "")
									+ "Specification",
							dataGroup.getLabel(), dataGroup.getLabel().replaceAll("_", "")
									+ "Specification" + specificationSootYieldCount);
					iABoxManagement.addObjectProperty("hasInitialAndPost-shockConditionsSpecification",
							dataGroup.getLabel(), "InitialAndPost-shockConditionsSpecification"
									+ dataGroup.getLabel().replaceAll("SootYield\\*E\\(m\\)Case", ""));

					// iABoxManagement.createIndividual(nameSpecification.replaceAll("Specification",
					// ""),
					// "InitialAndPost-shockConditionsSpecification"+dataGroup.getLabel().replaceAll("SootYield\\*E\\(m\\)Case",
					// ""));

				} else {
					// System.out.println("1+++++++++++++++++++++++++:" + x.getValue());
					// System.out.println("2++++++++++++++++++++++++++:" + x.getName());
					// System.out.println("2++++++++++++++++++++++++++:" + x.getOrigName());
//					 System.out.println("nameSpecification + specificationCount++++++++++++++++++++++++++:" + nameSpecification +
//					 specificationCount);
					// System.out.println("3++++++++++++++++++++++++++:" + x.getUnit());
					// System.out.println("nameSpecification.replaceAll(\"Specification\",
					// \"\")++++++++++++++++++++++++++:" +
					// nameSpecification.replaceAll("Specification", ""));
					 if (x.getName().contains("%"))
						iABoxManagement.addProperty(nameSpecification + specificationCount, "has" + x.getOrigName().replaceAll(SPACE, ""),
									x.getValue(), FLOAT);
					 else
						 iABoxManagement.addProperty(nameSpecification + specificationCount, "has" + x.getName(),
									x.getValue(), FLOAT);

					// iABoxManagement.createIndividual(nameSpecification.replaceAll("Specification",
					// "").replaceAll("_", "").replaceAll("[0-9]", ""), dataGroup.getLabel());
					// System.out.println("+++2####++++++#######++++++" +
					// dataGroupProperty.getDescriptionOrig());
					iABoxManagement.createIndividual(dataGroup.getLabel(), dataGroup.getLabel() + "_1");

					iABoxManagement.addObjectProperty("has" + dataGroup.getLabel() + "Specification",
							dataGroup.getLabel() + "_1",
							dataGroup.getLabel()+ "Specification"
									+ specificationCount);
					// System.out.println("dataGroup.getLabel()+++++++"+ dataGroup.getLabel());

					// System.out.println("nameSpecification+++++++++++++++++++++++++:" +
					// nameSpecification.replaceAll("Specification", "").replaceAll("_",
					// "").replaceAll("[0-9]", ""));

					// if(nameSpecification.contains("SootYield*E(m)"))
					// iABoxManagement.createIndividual(nameSpecification.replaceAll("Specification",
					// ""),
					// "InitialAndPost-shockConditionsSpecification"+dataGroup.getLabel().replaceAll("SootYield\\*E\\(m\\)Case",
					// ""));

					// System.out.println("dataGroup.getLabel().replaceAll++++++++++++++++++++++++++:"
					// + dataGroup.getLabel().replaceAll("SootYield\\*E\\(m\\)Case", ""));

					if (x.getUnit() != null) {
						 if (x.getName().contains("%"))
								iABoxManagement.addProperty(dataGroup.getLabel() + "_1", "has" + x.getOrigName().replaceAll(SPACE, "") + "Unit",
										x.getUnit(), STRING);
						 else {
						iABoxManagement.addProperty(dataGroup.getLabel() + "_1", "has" + x.getName() + "Unit",
								x.getUnit(), STRING);
						}
//						System.out.println("x.getUnit()+++++++++++++++++++++++++++:" + x.getUnit());
//						System.out.println("+++++++++++++++++++++++++++:" + dataGroup.getLabel() + "_1");	
//						System.out.println("+++++++++++++++++++++++++++:" + "has" + x.getName() + "Unit");
					}
				}

				// } else {

				// System.out.println("++++++++++++++++++++++++++x.getUncertaintyName():" +
				// x.getUncertaintyName());
				// System.out.println("++++++++++++++++++++++++++nameSpecification:" +
				// nameSpecification);

				if (x.getBound() != null && x.getUncertaintyName() != null && x.getName() != null
						&& x.getUncertaintyName().replaceAll(SPACE, UNDERSCORE).equalsIgnoreCase(x.getName())) {
					iABoxManagement
							.addProperty(nameSpecification.replaceAll("Specification", "") + "_1",
									ontoChemExpVocabulary.getDataPropertyHasUncertaintyBound() + "for"
											+ x.getUncertaintyName().replaceAll(SPACE, UNDERSCORE),
									x.getBound(), STRING);
					iABoxManagement
							.addProperty(nameSpecification.replaceAll("Specification", "") + "_1",
									ontoChemExpVocabulary.getDataPropertyHasUncertaintyKind() + "for"
											+ x.getUncertaintyName().replaceAll(SPACE, UNDERSCORE),
									x.getKind(), STRING);
					iABoxManagement.addProperty(nameSpecification.replaceAll("Specification", "") + "_1",
							ontoChemExpVocabulary.getDataPropertyHasUncertaintyTransformation() + "for"
									+ x.getUncertaintyName().replaceAll(SPACE, UNDERSCORE),
							x.getTransformation(), FLOAT);
					iABoxManagement
							.addProperty(nameSpecification.replaceAll("Specification", "") + "_1",
									ontoChemExpVocabulary.getDataPropertyHasUncertaintyValue() + "for"
											+ x.getUncertaintyName().replaceAll(SPACE, UNDERSCORE),
									x.getUncertainty(), FLOAT);
				}
			} catch (ABoxManagementException e) {
				logger.error("An individual of dataPoint could not be created.");
			}
		*/
	}

}
