package com.cmclinnovations.prime.species.model;

import java.util.ArrayList;

import org.springframework.context.ApplicationContext;

import com.cmclinnovations.conversion.ICompleteness;
import com.cmclinnovations.ontology.model.aboxes.ABoxManagement;
import com.cmclinnovations.ontology.model.aboxes.IABoxManagement;
import com.cmclinnovations.prime.species.model.configuration.OntoPrimeSpeciesKBConfig;
import com.cmclinnovations.prime.species.model.configuration.OntoPrimeSpeciesVocabulary;
import com.cmclinnovations.prime.species.model.configuration.OperationControlConfig;
import com.cmclinnovations.prime.species.model.configuration.PrimeSpeciesVocabulary;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.IChemicalCompositionQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.IChemicalIdentifierQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.IChemicalSpeciesQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.IPreferredKeyQuery;
import com.cmclinnovations.prime.species.model.converter.species.IAdditionalDataItemConverter;
import com.cmclinnovations.prime.species.model.converter.species.IBibliographyLinkConverter;
import com.cmclinnovations.prime.species.model.converter.species.IChemicalCompositionConverter;
import com.cmclinnovations.prime.species.model.converter.species.IChemicalIdentifierConverter;
import com.cmclinnovations.prime.species.model.converter.species.IChemicalSpeciesConverter;
import com.cmclinnovations.prime.species.model.converter.species.IContentConverter;
import com.cmclinnovations.prime.species.model.converter.species.ICopyrightConverter;
import com.cmclinnovations.prime.species.model.converter.species.IPreferredKeyConverter;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.ChemicalSpecies;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.bibliography.BibliographyLink;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.Atom;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.ChemicalComposition;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.Coal;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.Component;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.Amount;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.SpeciesLink;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.Uncertainty;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier.ChemicalIdentifier;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier.Name;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.content.Content;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.copyright.Copyright;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.preferred_key.PreferredKey;
import com.cmclinnovations.prime.species.model.owl.IAdditionalDataItemWriter;
import com.cmclinnovations.prime.species.model.owl.IBibliographyLinkWriter;
import com.cmclinnovations.prime.species.model.owl.IChemicalCompositionWriter;
import com.cmclinnovations.prime.species.model.owl.IChemicalIdentifierWriter;
import com.cmclinnovations.prime.species.model.owl.IChemicalSpeciesWriter;
import com.cmclinnovations.prime.species.model.owl.IContentWriter;
import com.cmclinnovations.prime.species.model.owl.ICopyrightWriter;
import com.cmclinnovations.prime.species.model.owl.IPreferredKeyWriter;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.AdditionalDataItemParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.AmountParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.AtomParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.BibliographyLinkParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.ChemicalCompositionParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.ChemicalIdentifierParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.ChemicalSpeciesParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.CoalParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.ComponentParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.ContentParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.CopyrightParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.NameParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.PreferredKeyParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.SpeciesLinkParseStatus;
import com.cmclinnovations.prime.species.model.parse.status.chemicalspecies.UncertaintyParseStatus;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IAdditionalDataItemParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IAmountParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IAtomParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IBibliographyLinkParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IChemicalCompositionParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IChemicalIdentifierParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IChemicalSpeciesParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ICoalParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IComponentParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IContentParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ICopyrightParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.INameParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IPreferredKeyParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ISpeciesLinkParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.IUncertaintyParser;

public class PrimeSpeciesConverterState extends ABoxManagement {
	public static ApplicationContext applicationContext;
	public static PrimeSpeciesVocabulary primeSpeciesVocabulary;
	public static OperationControlConfig opCtrl;
	public static OntoPrimeSpeciesVocabulary ontoPrimeSpeciesVocabulary;
	public static OntoPrimeSpeciesKBConfig ontoPrimeSpeciesKB;
	public static ICompleteness iCompleteness;
	public static IABoxManagement iABoxManagement;
	public static IInitPrimeSpeciesConverter initPrimeSpeciesConverter;
	
	public static String chemicalSpeciesName;
	
	public static ChemicalSpecies chemicalSpecies;
	public static ChemicalSpeciesParseStatus chemicalSpeciesParseStatus;
	public static IChemicalSpeciesParser iChemicalSpeciesParser;
	public static IChemicalSpeciesConverter iChemicalSpeciesConverter;
	public static IChemicalSpeciesWriter iChemicalSpeciesWriter;
	
	public static Copyright copyright;
	public static CopyrightParseStatus copyrightParseStatus;
	public static ICopyrightParser iCopyrightParser;
	public static ICopyrightConverter iCopyrightConverter;
	public static ICopyrightWriter iCopyrightWriter;

	public static Content content;
	public static ContentParseStatus contentParseStatus;
	public static IContentParser iContentParser;
	public static IContentConverter iContentConverter;
	public static IContentWriter iContentWriter;

	public static BibliographyLink bibliographyLink;
	public static BibliographyLinkParseStatus bibliographyLinkParseStatus;
	public static IBibliographyLinkParser iBibliographyLinkParser;
	public static IBibliographyLinkConverter iBibliographyLinkConverter;
	public static IBibliographyLinkWriter iBibliographyLinkWriter;

	public static PreferredKey preferredKey;
	public static PreferredKeyParseStatus preferredKeyParseStatus;
	public static IPreferredKeyParser iPreferredKeyParser;
	public static IPreferredKeyConverter iPreferredKeyConverter;
	public static IPreferredKeyWriter iPreferredKeyWriter;

	public static ChemicalIdentifier chemicalIdentifier;
	public static ChemicalIdentifierParseStatus chemicalIdentifierParseStatus;
	public static IChemicalIdentifierParser iChemicalIdentifierParser;
	public static IChemicalIdentifierConverter iChemicalIdentifierConverter;
	public static IChemicalIdentifierWriter iChemicalIdentifierWriter;

	public static ChemicalComposition chemicalComposition;
	public static ChemicalCompositionParseStatus chemicalCompositionParseStatus;
	public static IChemicalCompositionParser iChemicalCompositionParser;
	public static IChemicalCompositionConverter iChemicalCompositionConverter;
	public static IChemicalCompositionWriter iChemicalCompositionWriter;

	public static AdditionalDataItem additionalDataItem;
	public static AdditionalDataItemParseStatus additionalDataItemParseStatus;
	public static IAdditionalDataItemParser iAdditionalDataItemParser;
	public static IAdditionalDataItemConverter iAdditionalDataItemConverter;
	public static IAdditionalDataItemWriter iAdditionalDataItemWriter;

	public static Name name;
	public static ArrayList<Name> nameList;
	public static NameParseStatus nameParseStatus;
	public static INameParser iNameParser;
//	public static INameConverter iNameConverter;
//	public static INameWriter iNameWriter;

	public static Atom atom;
	public static ArrayList<Atom> atomList;
	public static AtomParseStatus atomParseStatus;
	public static IAtomParser iAtomParser;
//	public static IAtomConverter iAtomConverter;
//	public static IAtomWriter iAtomWriter;

	public static Component component;
	public static ComponentParseStatus componentParseStatus;
	public static IComponentParser iComponentParser;
//	public static IComponentConverter iComponentConverter;
//	public static IComponentWriter iComponentWriter;

	public static Coal coal;
	public static CoalParseStatus coalParseStatus;
	public static ICoalParser iCoalParser;
//	public static ICoalConverter iCoalConverter;
//	public static ICoalWriter iCoalWriter;

	public static SpeciesLink speciesLink;
	public static SpeciesLinkParseStatus speciesLinkParseStatus;
	public static ISpeciesLinkParser iSpeciesLinkParser;
//	public static ISpeciesLinkConverter iSpeciesLinkConverter;
//	public static ISpeciesLinkWriter iSpeciesLinkWriter;

	public static Amount amount;
	public static AmountParseStatus amountParseStatus;
	public static IAmountParser iAmountParser;
//	public static IAmountConverter iAmountConverter;
//	public static IAmountWriter iAmountWriter;

	public static Uncertainty uncertainty;
	public static UncertaintyParseStatus uncertaintyParseStatus;
	public static IUncertaintyParser iUncertaintyParser;
//	public static IUncertaintyConverter iUncertaintyConverter;
//	public static IUncertaintyWriter iUncertaintyWriter;
	
	
//	public static long chemicalSpeciesInstanceID = System.nanoTime();
//	public static long empiricalFormulaInstanceID = System.nanoTime();
	public static long chemicalSpeciesInstanceID;
	public static long empiricalFormulaInstanceID;
	
	
	public static int elementCount;
	
	public static boolean empiricalFormulaCreated;
	
	
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
	
	
	public static final String SKOS = "skos";
	public static final String SKOS_URL = "http://www.w3.org/2004/02/skos/core#";
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
	
	public static boolean needsToCreateChemicalSpecies = true;
	// owltoPrime converter related variables 
	public static String owlFileName;
	
	//================OWL to PrIMe conversion: Global================//
	//================Variable Declaration Started=================//
	public static IChemicalSpeciesQuery iChemicalSpeciesQuery;
	public static IPreferredKeyQuery iPreferredKeyQuery;
	public static IChemicalIdentifierQuery iChemicalIdentifierQuery;
	public static IChemicalCompositionQuery iChemicalCompositionQuery;
	
}
