package com.cmclinnovations.ontochemexp.model;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import com.cmclinnovations.ontochemexp.model.configuration.OperationControlConfig;
import com.cmclinnovations.conversion.Completeness;
import com.cmclinnovations.ontochemexp.model.configuration.DimensionalQuantityMapping;
import com.cmclinnovations.ontochemexp.model.configuration.OntoChemExpKBConfig;
import com.cmclinnovations.ontochemexp.model.configuration.OntoChemExpVocabulary;
import com.cmclinnovations.ontochemexp.model.configuration.PrimeVocabulary;
import com.cmclinnovations.ontochemexp.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.AdditionalDataItemQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.ApparatusQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.BibliographyLinkQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.CommonPropertiesQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.CopyrightQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.DataGroupQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.ExperimentQuery;
import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.PreferredKeyQuery;
import com.cmclinnovations.ontochemexp.model.converter.prime.AdditionalDataItemConverter;
//import com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql.ExperimentQuery;
import com.cmclinnovations.ontochemexp.model.converter.prime.ApparatusConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.BibliographyLinkConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.CommonPropertiesConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.CopyrightConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.DataGroupConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.ExperimentConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.PreferredKeyConverter;
import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
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
import com.cmclinnovations.ontochemexp.model.owl.AdditionalDataItemWriter;
import com.cmclinnovations.ontochemexp.model.owl.ApparatusWriter;
import com.cmclinnovations.ontochemexp.model.owl.BibliographyLinkWriter;
import com.cmclinnovations.ontochemexp.model.owl.CommonPropertiesWriter;
import com.cmclinnovations.ontochemexp.model.owl.CopyrightWriter;
import com.cmclinnovations.ontochemexp.model.owl.DataGroupWriter;
import com.cmclinnovations.ontochemexp.model.owl.DataPointWriter;
import com.cmclinnovations.ontochemexp.model.owl.ExperimentWriter;
import com.cmclinnovations.ontochemexp.model.owl.PreferredKeyWriter;
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
import com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.property.ApparatusPropertyUncertaintyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.apparatus.property.ApparatusPropertyValueParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.CommonPropertiesPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property.CommonPropertiesPropertyComponentAmountParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property.CommonPropertiesPropertyComponentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property.CommonPropertiesPropertyComponentSpeciesLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property.CommonPropertiesPropertyUncertaintyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.common_properties.property.CommonPropertiesPropertyValueParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataGroupDataGroupLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataGroupPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataPointParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataPointXParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyComponentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyDerivedPropertyFeatureObservableParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyDerivedPropertyFeatureParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyDerivedPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertySpeciesLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.property.DataGroupPropertyValueParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.AmountParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ComponentParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.DataAttributeLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.DerivedPropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.FeatureParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.IndicatorParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ObservableParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.PropertyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.SpeciesLinkParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.UncertaintyParseStatus;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.property.ValueParseStatus;
import com.cmclinnovations.ontochemexp.model.parser.prime.AdditionalDataItemParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.ApparatusParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.BibliographyLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.CommonPropertiesParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.CopyrightParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.DataGroupParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.ExperimentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.PreferredKeyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.ApparatusPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.ApparatusPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.ApparatusPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.KindParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.apparatus.ModeParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyComponentAmountParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyComponentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyComponentSpeciesLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyComponentUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.common_properties.CommonPropertiesPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupDataGroupLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupDataPointXParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyComponentParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyDerivedPropertyFeatureIndicatorParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyDerivedPropertyFeatureObservableParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyDerivedPropertyFeatureParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyDerivedPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertySpeciesLinkParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyUncertaintyParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataGroupPropertyValueParser;
import com.cmclinnovations.ontochemexp.model.parser.prime.data_group.DataPointParser;
import com.cmclinnovations.ontology.model.aboxes.ABoxManagement;

/**
 * Implemented the following methods of the IInitPrimeConverter interface:</br>
 * 1. the init method;</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class InitPrimeConverter extends PrimeConverter implements IInitPrimeConverter {
	public void init(long instanceSerialID) {

		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (opCtrl == null) {
			opCtrl = applicationContext.getBean(OperationControlConfig.class);
		}
		if (ontoChemExpVocabulary == null) {
			ontoChemExpVocabulary = applicationContext.getBean(OntoChemExpVocabulary.class);
		}
		if (primeVocabulary == null) {
			primeVocabulary = applicationContext.getBean(PrimeVocabulary.class);
		}
		if (ontoChemExpKB == null) {
			ontoChemExpKB = applicationContext.getBean(OntoChemExpKBConfig.class);
		}
		if (dimensionalQuantityMapping == null) {
			dimensionalQuantityMapping = applicationContext.getBean(DimensionalQuantityMapping.class);
		}

		if (iABoxManagement == null) {
			iABoxManagement = new ABoxManagement();
		}
		if (iCompleteness == null) {
			iCompleteness = new Completeness();
		}

		experimentName = EMPTY;
		needsToCreateExperiment = true;
		
		inApparatus = false;
		inCommonProperties = false;
		inCommonPropertiesPropertyComponent = false;
		inDataGroup = false;
		indataPoint = false;
		inAdditionalDataItem = false;
		inPreferredKey =false;

		experiment = new Experiment();
		experimentParseStatus = new ExperimentParseStatus();
		iExperimentParser = new ExperimentParser();
		iExperimentConverter = new ExperimentConverter();
		iExperimentWriter = new ExperimentWriter();

		copyright = new Copyright();
		copyrightParseStatus = new CopyrightParseStatus();
		iCopyrightParser = new CopyrightParser();
		iCopyrightConverter = new CopyrightConverter();
		iCopyrightWriter = new CopyrightWriter();

		bibliographyLink = new BibliographyLink();
		bibliographyLinkList = new ArrayList<BibliographyLink>();
		bibliographyLinkParseStatus = new BibliographyLinkParseStatus();
		iBibliographyLinkParser = new BibliographyLinkParser();
		iBibliographyLinkConverter = new BibliographyLinkConverter();
		iBibliographyLinkWriter = new BibliographyLinkWriter();

		apparatus = new Apparatus();
		apparatusParseStatus = new ApparatusParseStatus();
		iApparatusParser = new ApparatusParser();
		iApparatusConverter = new ApparatusConverter();
		iApparatusWriter = new ApparatusWriter();

		commonProperties = new CommonProperties();
		commonPropertiesParseStatus = new CommonPropertiesParseStatus();
		iCommonPropertiesParser = new CommonPropertiesParser();
		iCommonPropertiesConverter = new CommonPropertiesConverter();
		iCommonPropertiesWriter = new CommonPropertiesWriter();

		dataGroup = new DataGroup();
		dataGroupList = new ArrayList<DataGroup>();
		dataGroupParseStatus = new DataGroupParseStatus();
		iDataGroupParser = new DataGroupParser();
		iDataGroupConverter = new DataGroupConverter();
		iDataGroupWriter = new DataGroupWriter();
		
		additionalDataItem = new AdditionalDataItem();
		additionalDataItemList = new ArrayList<AdditionalDataItem>();
		additionalDataItemParseStatus = new AdditionalDataItemParseStatus();
		iAdditionalDataItemParser = new AdditionalDataItemParser();
		iAdditionalDataItemConverter = new AdditionalDataItemConverter();
		iAdditionalDataItemWriter = new AdditionalDataItemWriter();

		preferredKey = new PreferredKey();
		preferredKeyParseStatus = new PreferredKeyParseStatus();
		iPreferredKeyParser = new PreferredKeyParser();
		iPreferredKeyConverter = new PreferredKeyConverter();
		iPreferredKeyWriter = new PreferredKeyWriter();

		
		
		kind = new Kind();
		kindParseStatus = new KindParseStatus();
		iKindParser = new KindParser();

		mode = new Mode();
		modeList = new ArrayList<Mode>();
		modeParseStatus = new ModeParseStatus();
		iModeParser = new ModeParser();
		
		apparatusProperty = new ApparatusProperty();
		apparatusPropertyList = new ArrayList<ApparatusProperty>();
		apparatusPropertyParseStatus = new ApparatusPropertyParseStatus();
		iApparatusPropertyParser = new ApparatusPropertyParser();		
		
		apparatusPropertyValue = new Value();
		apparatusPropertyValueParseStatus = new ValueParseStatus();
		iApparatusPropertyValueParser = new ApparatusPropertyValueParser();
		
		apparatusPropertyUncertainty = new Uncertainty();
		apparatusPropertyUncertaintyParseStatus = new UncertaintyParseStatus();
		iApparatusPropertyUncertaintyParser = new ApparatusPropertyUncertaintyParser();
		
		
		
		commonPropertiesProperty = new CommonPropertiesProperty();
		commonPropertiesPropertyList = new ArrayList<CommonPropertiesProperty>();
		commonPropertiesPropertyParseStatus = new CommonPropertiesPropertyParseStatus();
		iCommonPropertiesPropertyParser = new CommonPropertiesPropertyParser();
		
		commonPropertiesPropertyValue = new Value();
		commonPropertiesPropertyValueParseStatus = new ValueParseStatus();
		iCommonPropertiesPropertyValueParser = new CommonPropertiesPropertyValueParser();
		
		commonPropertiesPropertyUncertainty = new Uncertainty();
		commonPropertiesPropertyUncertaintyParseStatus = new UncertaintyParseStatus();
		iCommonPropertiesPropertyUncertaintyParser = new CommonPropertiesPropertyUncertaintyParser();
		
		commonPropertiesPropertyComponent = new Component();
		commonPropertiesPropertyComponentList = new ArrayList<Component>();
		commonPropertiesPropertyComponentParseStatus = new ComponentParseStatus();
		iCommonPropertiesPropertyComponentParser = new CommonPropertiesPropertyComponentParser();
		
		commonPropertiesPropertyComponentSpeciesLink = new SpeciesLink();
		commonPropertiesPropertyComponentSpeciesLinkParseStatus = new SpeciesLinkParseStatus();
		iCommonPropertiesPropertyComponentSpeciesLinkParser = new CommonPropertiesPropertyComponentSpeciesLinkParser();
		
		commonPropertiesPropertyComponentAmount = new Amount();
		commonPropertiesPropertyComponentAmountParseStatus = new AmountParseStatus();
		iCommonPropertiesPropertyComponentAmountParser = new CommonPropertiesPropertyComponentAmountParser();
		
		commonPropertiesPropertyComponentUncertainty = new Uncertainty();
		commonPropertiesPropertyComponentUncertaintyParseStatus = new UncertaintyParseStatus();
		iCommonPropertiesPropertyComponentUncertaintyParser = new CommonPropertiesPropertyComponentUncertaintyParser();
		
		
		
		dataGroupDataGroupLink = new DataGroupDataGroupLink();
		dataGroupDataGroupLinkParseStatus = new DataGroupDataGroupLinkParseStatus();
		iDataGroupDataGroupLinkParser = new DataGroupDataGroupLinkParser();
		
		dataGroupProperty = new DataGroupProperty();
		dataGroupPropertyList = new ArrayList<DataGroupProperty>();
		dataGroupPropertyParseStatus = new DataGroupPropertyParseStatus();
		iDataGroupPropertyParser = new DataGroupPropertyParser();
		
		dataGroupPropertySpeciesLink = new SpeciesLink();
		dataGroupPropertySpeciesLinkParseStatus = new SpeciesLinkParseStatus();
		iDataGroupPropertySpeciesLinkParser = new DataGroupPropertySpeciesLinkParser();
	
		dataGroupPropertyComponent = new DataGroupPropertyComponent();
		dataGroupPropertyComponentParseStatus = new DataGroupPropertyComponentParseStatus();
		iDataGroupPropertyComponentParser = new DataGroupPropertyComponentParser();
		
		dataGroupPropertyValue = new Value();
		dataGroupPropertyValueParseStatus = new ValueParseStatus();
		iDataGroupPropertyValueParser = new DataGroupPropertyValueParser();
		
		dataGroupPropertyUncertainty = new Uncertainty();
		dataGroupPropertyUncertaintyParseStatus = new UncertaintyParseStatus();
		iDataGroupPropertyUncertaintyParser = new DataGroupPropertyUncertaintyParser();
		
		dataGroupPropertyDerivedProperty = new DerivedProperty();
		dataGroupPropertyDerivedPropertyParseStatus = new DerivedPropertyParseStatus();
		iDataGroupPropertyDerivedPropertyParser = new DataGroupPropertyDerivedPropertyParser();
		
		dataGroupPropertyDerivedPropertyFeature = new Feature();
		dataGroupPropertyDerivedPropertyFeatureParseStatus = new FeatureParseStatus();
		iDataGroupPropertyDerivedPropertyFeatureParser = new DataGroupPropertyDerivedPropertyFeatureParser();
		
		dataGroupPropertyDerivedPropertyFeatureIndicator = new Indicator();
		dataGroupPropertyDerivedPropertyFeatureIndicatorList = new ArrayList<Indicator>();
		dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus = new IndicatorParseStatus();
		iDataGroupPropertyDerivedPropertyFeatureIndicatorParser = new DataGroupPropertyDerivedPropertyFeatureIndicatorParser();
		
		dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink = new PropertyLink();
		dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus = new PropertyLinkParseStatus();
		
		dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink = new DataAttributeLink();
		dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus = new DataAttributeLinkParseStatus();
		
		dataGroupPropertyDerivedPropertyFeatureObservable = new Observable();
		dataGroupPropertyDerivedPropertyFeatureObservableParseStatus = new ObservableParseStatus();
		iDataGroupPropertyDerivedPropertyFeatureObservableParser = new DataGroupPropertyDerivedPropertyFeatureObservableParser();
		
		dataGroupDataPoint = new DataGroupDataPoint();
		dataGroupDataPointList = new ArrayList<DataGroupDataPoint>();
		dataGroupDataPointParseStatus = new DataPointParseStatus();
		iDataPointParser = new DataPointParser();
		
		x = new DataGroupDataPointX();
		xList = new ArrayList<DataGroupDataPointX>();
		dataGroupDataPointXParseStatus = new DataPointXParseStatus();
		iDataGroupDataPointXParser = new DataGroupDataPointXParser();
		
		xDQMap = new HashMap<String, String>();
		currentDQInstance = new String();
		
		x1 = new X1();
		x2 = new X2();
		x3 = new X3();
		x4 = new X4();
		x5 = new X5();
		x6 = new X6();
		x7 = new X7();
		x8 = new X8();
		x9 = new X9();
		x10 = new X10();
		x11 = new X11();
		
		x1ParseStatus = new DataPointXParseStatus();
		x2ParseStatus = new DataPointXParseStatus();
		x3ParseStatus = new DataPointXParseStatus();
		x4ParseStatus = new DataPointXParseStatus();
		x5ParseStatus = new DataPointXParseStatus();
		x6ParseStatus = new DataPointXParseStatus();
		x7ParseStatus = new DataPointXParseStatus();
		x8ParseStatus = new DataPointXParseStatus();
		x9ParseStatus = new DataPointXParseStatus();
		x10ParseStatus = new DataPointXParseStatus();
		x11ParseStatus = new DataPointXParseStatus();
		
		xUncertaintyParseStatus = new UncertaintyParseStatus();
		xUncertainty = new Uncertainty();
		xUncertaintyList = new ArrayList<Uncertainty>();
		
		dataGroupPropertyComponentSpeciesLink = new SpeciesLink();
		dataGroupPropertyComponentSpeciesLinkParseStatus = new SpeciesLinkParseStatus();
		
		
		instanceSerialID = System.nanoTime(); // comment out this line when doing JUnit test
		experimentInstanceId = instanceSerialID;
		apparatusID = instanceSerialID+1;
		copyrightID = instanceSerialID+2;
		bibliographyLinkID = instanceSerialID+3;
		additionalDataItemID = instanceSerialID+4;
		preferredKeyID = instanceSerialID+5;
		commonPropertiesID = instanceSerialID+6;
		dataGroupID = instanceSerialID+7;
		
		additionalDataItemCount = 0;
		apparatusPropertyCount = 0;
		modeCount = 0;
		bibliographyLinkCount=0;
		commonPropertiesPropertyCount = 0;
		componentCount = 0;
		dataGroupCount = 0;
		dataGroupPropertyCount = 1000;
		indicatorCount = 0;
		dataGroupDataPointCount = 10000;
		dataGroupDataPointXCount = 0;
		xUncertaintyCount = 1000;
		
		
		reasonerFactory = new StructuralReasonerFactory();
		queryResult = new ArrayList<String>();
		//================OWL to PrIMe conversion: Global================//
		//================Variable Initialisation Started===============//
		// Creates an instance of the Experiment Query class.
		iExperimentQuery = new ExperimentQuery();
		iAdditionalDataItemQuery = new AdditionalDataItemQuery();
		iBibliographyLinkQuery = new BibliographyLinkQuery();
		iCopyrightQuery = new CopyrightQuery();
		iPreferredKeyQuery = new PreferredKeyQuery();
		iApparatusQuery = new ApparatusQuery();
		iCommonPropertiesQuery = new CommonPropertiesQuery();
		iDataGroupQuery = new DataGroupQuery();
		
		
		
	}
}
