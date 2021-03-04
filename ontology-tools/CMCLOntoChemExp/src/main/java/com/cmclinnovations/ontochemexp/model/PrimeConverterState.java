package com.cmclinnovations.ontochemexp.model;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;

import org.springframework.context.ApplicationContext;
import com.cmclinnovations.ontochemexp.model.configuration.OperationControlConfig;
import com.cmclinnovations.conversion.ICompleteness;

import com.cmclinnovations.ontochemexp.model.configuration.OntoChemExpKBConfig;
import com.cmclinnovations.ontochemexp.model.configuration.OntoChemExpVocabulary;
import com.cmclinnovations.ontochemexp.model.configuration.PrimeVocabulary;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IAdditionalDataItemQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IApparatusQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IBibliographyLinkQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.ICommonPropertiesQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.ICopyrightQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IDataGroupQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IExperimentQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IPreferredKeyQuery;
import com.cmclinnovations.ontochemexp.model.converter.prime.IAdditionalDataItemConverter;
//import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.IExperimentQuery;
import com.cmclinnovations.ontochemexp.model.converter.prime.IApparatusConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.IBibliographyLinkConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.ICommonPropertiesConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.ICopyrightConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.IDataGroupConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.IExperimentConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.IPreferredKeyConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.Experiment;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Apparatus;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Kind;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Mode;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonProperties;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesProperty;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesProperty;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesPropertyComponent;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesPropertyComponentAmount;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesPropertyComponentSpeciesLink;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesPropertyUncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.copyright.Copyright;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroup;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupDataGroupLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupPropertyComponent;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X1;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X10;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X11;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X2;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X3;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X4;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X5;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X6;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X7;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X8;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X9;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.preferred_key.PreferredKey;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Amount;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DataAttributeLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DerivedProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Feature;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Indicator;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Observable;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.PropertyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupDataPoint;
import com.cmclinnovations.ontochemexp.model.owl.IAdditionalDataItemWriter;
import com.cmclinnovations.ontochemexp.model.owl.IApparatusWriter;
import com.cmclinnovations.ontochemexp.model.owl.IBibliographyLinkWriter;
import com.cmclinnovations.ontochemexp.model.owl.ICommonPropertiesWriter;
import com.cmclinnovations.ontochemexp.model.owl.ICopyrightWriter;
import com.cmclinnovations.ontochemexp.model.owl.IDataGroupPropertyWriter;
import com.cmclinnovations.ontochemexp.model.owl.IDataGroupWriter;
import com.cmclinnovations.ontochemexp.model.owl.IDataPointWriter;
import com.cmclinnovations.ontochemexp.model.owl.IExperimentWriter;
import com.cmclinnovations.ontochemexp.model.owl.IPreferredKeyWriter;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.AdditionalDataItemParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.ApparatusParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.BibliographyLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.CommonPropertiesParseStatus;
//import com.cmclinnovations.ontochemexp.model.parse.status.prime.CommonPropertiesPropertyUncertaintyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.CopyrightParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.DataGroupParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.ExperimentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.PreferredKeyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.ApparatusPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.KindParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.ModeParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.CommonPropertiesPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataGroupDataGroupLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataGroupPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataPointParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataPointXParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyComponentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.AmountParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ComponentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.DataAttributeLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.DerivedPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.FeatureParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.IndicatorParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ObservableParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.SpeciesLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.UncertaintyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ValueParseStatus;
//import com.cmclinnovations.ontochemexp.model.parse.status.prime.XParseStatus;
import com.cmclinnovations.ontochemexp.model.parser.prime.CommonPropertiesParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IAdditionalDataItemParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IApparatusParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IBibliographyLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.ICommonPropertiesParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.ICopyrightParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IDataGroupParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IExperimentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.IPreferredKeyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.IApparatusPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.IApparatusPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.IApparatusPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.IKindParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.IModeParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyComponentAmountParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyComponentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyComponentSpeciesLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyComponentUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.ICommonPropertiesPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupDataGroupLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupDataPointXParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyComponentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyDerivedPropertyFeatureIndicatorParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyDerivedPropertyFeatureObservableParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyDerivedPropertyFeatureParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyDerivedPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertySpeciesLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataGroupPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.IDataPointParser;
import com.cmclinnovations.ontology.model.aboxes.ABoxManagement;
import com.cmclinnovations.ontology.model.aboxes.IABoxManagement;

//import de.derivo.sparqldlapi.QueryEngine;

/**
 * This class maintains the current state of all the member variables
 * used in the PrIMe to OWL conversion.
 * 
 * @author msff2
 *
 */
public class PrimeConverterState extends ABoxManagement{

	public static ApplicationContext applicationContext;
	public static PrimeVocabulary primeVocabulary;
	public static OperationControlConfig opCtrl;
	public static OntoChemExpVocabulary ontoChemExpVocabulary;
	public static OntoChemExpKBConfig ontoChemExpKB;
	public static ICompleteness iCompleteness;
	
	public static String experimentName;
	
	
	public static boolean inApparatus = false;
	public static boolean inCommonProperties = false;
	public static boolean inCommonPropertiesPropertyComponent = false;
	public static boolean inDataGroup = false;
	public static boolean indataPoint = false;
	public static boolean inAdditionalDataItem = false;
	public static boolean inPreferredKey =false;
	
	// A member variable created to hold the instance of the class that
	// can store PrIMe experiment's data and metadata. They are then stored to be
	// codified in the experiment OWL ontology that is being created.
	public static Experiment experiment;
	// A member variable created to hold the instance of the class that
	// maintains a link to getters and setters of flags that indicate
	// if PrIMe experiment's elements and attributes have already been parsed.
	public static ExperimentParseStatus experimentParseStatus;
	// A member variable created to hold the instance of the class that
	// includes PrIMe experiment's elements and attributes parser methods.
	public static IExperimentParser iExperimentParser;
	public static IExperimentConverter iExperimentConverter;
	public static IExperimentWriter iExperimentWriter;
	
	public static Copyright copyright;
	public static CopyrightParseStatus copyrightParseStatus;
	public static ICopyrightParser iCopyrightParser;
	public static ICopyrightConverter iCopyrightConverter;	
	public static ICopyrightWriter iCopyrightWriter;
	
	public static BibliographyLink bibliographyLink;
	public static ArrayList <BibliographyLink> bibliographyLinkList;
	public static BibliographyLinkParseStatus bibliographyLinkParseStatus;
	public static IBibliographyLinkParser iBibliographyLinkParser;
	public static IBibliographyLinkConverter iBibliographyLinkConverter;	
	public static IBibliographyLinkWriter iBibliographyLinkWriter;	
	
	public static Apparatus apparatus;
	public static ApparatusParseStatus apparatusParseStatus;
	public static IApparatusParser iApparatusParser;
	public static IApparatusConverter iApparatusConverter;
	public static IApparatusWriter iApparatusWriter;
	
	public static CommonProperties commonProperties;
	public static CommonPropertiesParseStatus commonPropertiesParseStatus;
	public static ICommonPropertiesParser iCommonPropertiesParser;
	public static ICommonPropertiesConverter iCommonPropertiesConverter;	
	public static ICommonPropertiesWriter iCommonPropertiesWriter;

	public static DataGroup dataGroup;
	public static ArrayList<DataGroup> dataGroupList;
	public static DataGroupParseStatus dataGroupParseStatus;
	public static IDataGroupParser iDataGroupParser;
	public static IDataGroupConverter iDataGroupConverter;	
	public static IDataGroupWriter iDataGroupWriter;
	
	public static AdditionalDataItem additionalDataItem;
	public static ArrayList<AdditionalDataItem> additionalDataItemList;
	public static AdditionalDataItemParseStatus additionalDataItemParseStatus;
	public static IAdditionalDataItemParser iAdditionalDataItemParser;
	public static IAdditionalDataItemConverter iAdditionalDataItemConverter;
	public static IAdditionalDataItemWriter iAdditionalDataItemWriter;
	
	public static PreferredKey preferredKey;
	public static PreferredKeyParseStatus preferredKeyParseStatus;
	public static IPreferredKeyConverter iPreferredKeyConverter;
	public static IPreferredKeyParser iPreferredKeyParser;
	public static IPreferredKeyWriter iPreferredKeyWriter;
	
	
	////////////////////////////////////////////////////////////////
	/// Apparatus
	public static Kind kind; 
	public static KindParseStatus kindParseStatus; 
	public static IKindParser iKindParser;
	
	public static Mode mode; 
	public static ArrayList<Mode> modeList;
	public static ModeParseStatus modeParseStatus; 
	public static IModeParser iModeParser;
	
	public static ApparatusProperty apparatusProperty;
	public static ArrayList<ApparatusProperty> apparatusPropertyList;
	public static ApparatusPropertyParseStatus apparatusPropertyParseStatus;
	public static IApparatusPropertyParser iApparatusPropertyParser;
	
	public static Value apparatusPropertyValue;
	public static ValueParseStatus apparatusPropertyValueParseStatus;
	public static IApparatusPropertyValueParser iApparatusPropertyValueParser;
	
	
	public static Uncertainty apparatusPropertyUncertainty;
	public static UncertaintyParseStatus apparatusPropertyUncertaintyParseStatus;
	public static IApparatusPropertyUncertaintyParser iApparatusPropertyUncertaintyParser;
	
	
	////////////////////////////////////////////////////////////////
	/// CommonProperties
	public static CommonPropertiesProperty commonPropertiesProperty;
	public static ArrayList<CommonPropertiesProperty> commonPropertiesPropertyList;
	public static CommonPropertiesPropertyParseStatus commonPropertiesPropertyParseStatus;
	public static ICommonPropertiesPropertyParser iCommonPropertiesPropertyParser;
	
	public static Value commonPropertiesPropertyValue;
	public static ValueParseStatus commonPropertiesPropertyValueParseStatus;
	public static ICommonPropertiesPropertyValueParser iCommonPropertiesPropertyValueParser;
	
	public static Uncertainty commonPropertiesPropertyUncertainty;
	public static UncertaintyParseStatus commonPropertiesPropertyUncertaintyParseStatus;
	public static ICommonPropertiesPropertyUncertaintyParser iCommonPropertiesPropertyUncertaintyParser;
	
	public static Component commonPropertiesPropertyComponent;
	public static ArrayList<Component> commonPropertiesPropertyComponentList;
	public static ComponentParseStatus commonPropertiesPropertyComponentParseStatus;
	public static ICommonPropertiesPropertyComponentParser iCommonPropertiesPropertyComponentParser;
	
	public static SpeciesLink commonPropertiesPropertyComponentSpeciesLink;
	public static SpeciesLinkParseStatus commonPropertiesPropertyComponentSpeciesLinkParseStatus;
	public static ICommonPropertiesPropertyComponentSpeciesLinkParser iCommonPropertiesPropertyComponentSpeciesLinkParser;
	
	public static Amount commonPropertiesPropertyComponentAmount;
	public static AmountParseStatus commonPropertiesPropertyComponentAmountParseStatus;
	public static ICommonPropertiesPropertyComponentAmountParser iCommonPropertiesPropertyComponentAmountParser;
	
	public static Uncertainty commonPropertiesPropertyComponentUncertainty;
	public static UncertaintyParseStatus commonPropertiesPropertyComponentUncertaintyParseStatus;
	public static ICommonPropertiesPropertyComponentUncertaintyParser iCommonPropertiesPropertyComponentUncertaintyParser;
	
	////////////////////////////////////////////////////////////////
	/// DataGroup
	public static DataGroupDataGroupLink dataGroupDataGroupLink;
	public static DataGroupDataGroupLinkParseStatus dataGroupDataGroupLinkParseStatus;
	public static IDataGroupDataGroupLinkParser iDataGroupDataGroupLinkParser;
	
	public static DataGroupProperty dataGroupProperty;
	public static ArrayList<DataGroupProperty> dataGroupPropertyList;
	public static DataGroupPropertyParseStatus dataGroupPropertyParseStatus;
	public static IDataGroupPropertyParser iDataGroupPropertyParser;
	
	public static SpeciesLink dataGroupPropertySpeciesLink;
	public static SpeciesLinkParseStatus dataGroupPropertySpeciesLinkParseStatus;
	public static IDataGroupPropertySpeciesLinkParser iDataGroupPropertySpeciesLinkParser;
	
	public static DataGroupPropertyComponent dataGroupPropertyComponent;
	public static DataGroupPropertyComponentParseStatus dataGroupPropertyComponentParseStatus;
	public static IDataGroupPropertyComponentParser iDataGroupPropertyComponentParser;
	
	public static Value dataGroupPropertyValue;
	public static ValueParseStatus dataGroupPropertyValueParseStatus;
	public static IDataGroupPropertyValueParser iDataGroupPropertyValueParser;
	
	public static Uncertainty dataGroupPropertyUncertainty;
	public static UncertaintyParseStatus dataGroupPropertyUncertaintyParseStatus;
	public static IDataGroupPropertyUncertaintyParser iDataGroupPropertyUncertaintyParser;
	
	public static DerivedProperty dataGroupPropertyDerivedProperty;
	public static DerivedPropertyParseStatus dataGroupPropertyDerivedPropertyParseStatus;
	public static IDataGroupPropertyDerivedPropertyParser iDataGroupPropertyDerivedPropertyParser;
	
	public static Feature dataGroupPropertyDerivedPropertyFeature;
	public static FeatureParseStatus dataGroupPropertyDerivedPropertyFeatureParseStatus;
	public static IDataGroupPropertyDerivedPropertyFeatureParser iDataGroupPropertyDerivedPropertyFeatureParser;
	
	public static Indicator dataGroupPropertyDerivedPropertyFeatureIndicator;
	public static ArrayList<Indicator> dataGroupPropertyDerivedPropertyFeatureIndicatorList;
	public static IndicatorParseStatus dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus;
	public static IDataGroupPropertyDerivedPropertyFeatureIndicatorParser iDataGroupPropertyDerivedPropertyFeatureIndicatorParser;
	
	public static PropertyLink dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink; 
	public static PropertyLinkParseStatus dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus; 
	
	public static DataAttributeLink dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink;
	public static DataAttributeLinkParseStatus dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus; 
	
	public static Observable dataGroupPropertyDerivedPropertyFeatureObservable; 
	public static ObservableParseStatus dataGroupPropertyDerivedPropertyFeatureObservableParseStatus; 
	public static IDataGroupPropertyDerivedPropertyFeatureObservableParser iDataGroupPropertyDerivedPropertyFeatureObservableParser;

	public static DataGroupDataPoint  dataGroupDataPoint;
	public static ArrayList<DataGroupDataPoint> dataGroupDataPointList;
	public static DataPointParseStatus dataGroupDataPointParseStatus;
	public static IDataPointParser iDataPointParser;
	
	public static DataGroupDataPointX  x;
	public static ArrayList<DataGroupDataPointX> xList;
	public static DataPointXParseStatus dataGroupDataPointXParseStatus;
	public static IDataGroupDataPointXParser iDataGroupDataPointXParser;
	
	public static X1 x1;
	public static X2 x2;
	public static X3 x3;
	public static X4 x4;
	public static X5 x5;
	public static X6 x6;
	public static X7 x7;
	public static X8 x8;
	public static X9 x9;
	public static X10 x10;
	public static X11 x11;
	
	public static DataPointXParseStatus x1ParseStatus;
	public static DataPointXParseStatus x2ParseStatus;
	public static DataPointXParseStatus x3ParseStatus;
	public static DataPointXParseStatus x4ParseStatus;
	public static DataPointXParseStatus x5ParseStatus;
	public static DataPointXParseStatus x6ParseStatus;
	public static DataPointXParseStatus x7ParseStatus;
	public static DataPointXParseStatus x8ParseStatus;
	public static DataPointXParseStatus x9ParseStatus;
	public static DataPointXParseStatus x10ParseStatus;
	public static DataPointXParseStatus x11ParseStatus;
	
	public static UncertaintyParseStatus xUncertaintyParseStatus;
	public static Uncertainty xUncertainty;
	public static ArrayList<Uncertainty> xUncertaintyList;
	
	public static SpeciesLink dataGroupPropertyComponentSpeciesLink;
	public static SpeciesLinkParseStatus dataGroupPropertyComponentSpeciesLinkParseStatus;
	
	public static long experimentInstanceId;
	public static long apparatusID;
	public static long copyrightID;
	public static long bibliographyLinkID;
	public static long additionalDataItemID;
	public static long preferredKeyID;
	public static long commonPropertiesID;
	public static long dataGroupID;
	
	public static int additionalDataItemCount;
	public static int apparatusPropertyCount;
	public static int modeCount;
	public static int bibliographyLinkCount;
	public static int commonPropertiesPropertyCount;
	public static int componentCount;
	public static int dataGroupCount;
	public static int dataGroupPropertyCount;
	public static int indicatorCount;
	public static int dataGroupDataPointCount;
	public static int dataGroupDataPointXCount;
	public static int xUncertaintyCount;
	
	
	public static final String EMPTY = "";
	public static final String HASH = "#";
	public static final String SPACE = " ";
	public static final String COLON = ":";
	public static final String UNDERSCORE = "_";
	public static final String BACKSLASH = "/";
	public static final String HYPHEN = "-";
	public static final String FRONTSLASH = "\\";
	public static final String FLOAT = "float";
	public static final String INTEGER = "integer";
	public static final String STRING = "string";
	
	
	public static IABoxManagement iABoxManagement;	
	
	public static IInitPrimeConverter initPrimeConverter;
	
	
	public static final String RDFS = "rdfs";
	public static final String RDFS_LABEL = "label";
	public static final String RDFS_COMMENT = "comment";
	public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF = "rdf";
	public static final String RDF_TYPE = "type";
	public static final String RDF_URL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";	
	public static final String OWL = "owl";
	public static final String OWL_VERSIONINFO = "versionInfo";
	public static final String OWL_URL = "http://www.w3.org/2002/07/owl#";
	public static final String DUBLIN_CORE = "dc";
	public static final String DUBLIN_CORE_ID = "identifier";
	public static final String DUBLIN_CORE_URL = "http://purl.org/dc/elements/1.1/";
	
	public static boolean needsToCreateExperiment = true;
	// owltoPrime converter related variables 
	public static String owlFileName;
//    public static QueryEngine engine;
//	public static IExperimentQuery iExperimentQuery;
    
	//================OWL to PrIMe conversion: Global================//
	//================Variable Declaration Started=================//
	public static IExperimentQuery iExperimentQuery;
	public static IAdditionalDataItemQuery iAdditionalDataItemQuery;
	public static IBibliographyLinkQuery iBibliographyLinkQuery;
	public static ICopyrightQuery iCopyrightQuery;
	public static IPreferredKeyQuery iPreferredKeyQuery;
	public static IApparatusQuery iApparatusQuery;
	public static ICommonPropertiesQuery iCommonPropertiesQuery;
	public static IDataGroupQuery iDataGroupQuery;
	
}