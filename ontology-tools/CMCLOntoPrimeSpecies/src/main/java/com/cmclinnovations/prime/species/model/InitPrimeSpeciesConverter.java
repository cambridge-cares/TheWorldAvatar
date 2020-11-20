package com.cmclinnovations.prime.species.model;

import java.util.ArrayList;

import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.prime.species.model.configuration.OperationControlConfig;
import com.cmclinnovations.prime.species.model.configuration.SpringConfiguration;
import com.cmclinnovations.conversion.Completeness;
import com.cmclinnovations.ontology.model.aboxes.ABoxManagement;
import com.cmclinnovations.prime.species.model.configuration.OntoPrimeSpeciesKBConfig;
import com.cmclinnovations.prime.species.model.configuration.OntoPrimeSpeciesVocabulary;
import com.cmclinnovations.prime.species.model.configuration.PrimeSpeciesVocabulary;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.ChemicalCompositionQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.ChemicalIdentifierQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.ChemicalSpeciesQuery;
import com.cmclinnovations.prime.species.model.converter.owl.query.sparql.PreferredKeyQuery;
import com.cmclinnovations.prime.species.model.converter.species.AdditionalDataItemConverter;
import com.cmclinnovations.prime.species.model.converter.species.BibliographyLinkConverter;
import com.cmclinnovations.prime.species.model.converter.species.ChemicalCompositionConverter;
import com.cmclinnovations.prime.species.model.converter.species.ChemicalIdentifierConverter;
import com.cmclinnovations.prime.species.model.converter.species.ChemicalSpeciesConverter;
import com.cmclinnovations.prime.species.model.converter.species.ContentConverter;
import com.cmclinnovations.prime.species.model.converter.species.CopyrightConverter;
import com.cmclinnovations.prime.species.model.converter.species.PreferredKeyConverter;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;
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
import com.cmclinnovations.prime.species.model.owl.AdditionalDataItemWriter;
import com.cmclinnovations.prime.species.model.owl.BibliographyLinkWriter;
import com.cmclinnovations.prime.species.model.owl.ChemicalCompositionWriter;
import com.cmclinnovations.prime.species.model.owl.ChemicalIdentifierWriter;
import com.cmclinnovations.prime.species.model.owl.ChemicalSpeciesWriter;
import com.cmclinnovations.prime.species.model.owl.ContentWriter;
import com.cmclinnovations.prime.species.model.owl.CopyrightWriter;
import com.cmclinnovations.prime.species.model.owl.PreferredKeyWriter;
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
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.AdditionalDataItemParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.AmountParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.AtomParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.BibliographyLinkParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ChemicalCompositionParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ChemicalIdentifierParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ChemicalSpeciesParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.CoalParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ComponentParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.ContentParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.CopyrightParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.NameParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.PreferredKeyParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.SpeciesLinkParser;
import com.cmclinnovations.prime.species.model.parser.chemicalspecies.UncertaintyParser;

public class InitPrimeSpeciesConverter extends PrimeSpeciesConverter implements IInitPrimeSpeciesConverter {
	public void init(long instanceSerialID) {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (opCtrl == null) {
			opCtrl = applicationContext.getBean(OperationControlConfig.class);
		}
		if (ontoPrimeSpeciesVocabulary == null) {
			ontoPrimeSpeciesVocabulary = applicationContext.getBean(OntoPrimeSpeciesVocabulary.class);
		}
		if (primeSpeciesVocabulary == null) {
			primeSpeciesVocabulary = applicationContext.getBean(PrimeSpeciesVocabulary.class);
		}
		if (ontoPrimeSpeciesKB == null) {
			ontoPrimeSpeciesKB = applicationContext.getBean(OntoPrimeSpeciesKBConfig.class);
		}
		if (iABoxManagement == null) {
			iABoxManagement = new ABoxManagement();
		}
		if (iCompleteness == null) {
			iCompleteness = new Completeness();
		}
		
		chemicalSpeciesName = EMPTY;
		needsToCreateChemicalSpecies = true;
		
		chemicalSpecies = new ChemicalSpecies();
		chemicalSpeciesParseStatus = new ChemicalSpeciesParseStatus();
		iChemicalSpeciesParser = new ChemicalSpeciesParser();
		iChemicalSpeciesConverter = new ChemicalSpeciesConverter();
		iChemicalSpeciesWriter = new ChemicalSpeciesWriter();
		
		copyright = new Copyright();
		copyrightParseStatus = new CopyrightParseStatus();
		iCopyrightParser = new CopyrightParser();
		iCopyrightConverter = new CopyrightConverter();
		iCopyrightWriter = new CopyrightWriter();

		content = new Content();
		contentParseStatus = new ContentParseStatus();
		iContentParser = new ContentParser();
		iContentConverter = new ContentConverter();
		iContentWriter = new ContentWriter();

		bibliographyLink = new BibliographyLink();
		bibliographyLinkParseStatus = new BibliographyLinkParseStatus();
		iBibliographyLinkParser = new BibliographyLinkParser();
		iBibliographyLinkConverter = new BibliographyLinkConverter();
		iBibliographyLinkWriter = new BibliographyLinkWriter();

		preferredKey = new PreferredKey();
		preferredKeyParseStatus = new PreferredKeyParseStatus();
		iPreferredKeyParser = new PreferredKeyParser();
		iPreferredKeyConverter = new PreferredKeyConverter();
		iPreferredKeyWriter = new PreferredKeyWriter();

		chemicalIdentifier = new ChemicalIdentifier();
		chemicalIdentifierParseStatus = new ChemicalIdentifierParseStatus();
		iChemicalIdentifierParser = new ChemicalIdentifierParser();
		iChemicalIdentifierConverter = new ChemicalIdentifierConverter();
		iChemicalIdentifierWriter = new ChemicalIdentifierWriter();

		chemicalComposition = new ChemicalComposition();
		chemicalCompositionParseStatus = new ChemicalCompositionParseStatus();
		iChemicalCompositionParser = new ChemicalCompositionParser();
		iChemicalCompositionConverter = new ChemicalCompositionConverter();
		iChemicalCompositionWriter = new ChemicalCompositionWriter();

		additionalDataItem = new AdditionalDataItem();
		additionalDataItemParseStatus = new AdditionalDataItemParseStatus();
		iAdditionalDataItemParser = new AdditionalDataItemParser();
		iAdditionalDataItemConverter = new AdditionalDataItemConverter();
		iAdditionalDataItemWriter = new AdditionalDataItemWriter();

		name = new Name();
		nameList = new ArrayList<Name>();
		nameParseStatus = new NameParseStatus();
		iNameParser = new NameParser();
//		iNameConverter = new NameConverter();
//		iNameWriter = new NameWriter();

		atom = new Atom();
		atomList = new ArrayList<Atom>();
		atomParseStatus = new AtomParseStatus();
		iAtomParser = new AtomParser();
//		iAtomConverter = new AtomConverter();
//		iAtomWriter = new AtomWriter();

		component = new Component();
		componentParseStatus = new ComponentParseStatus();
		iComponentParser = new ComponentParser();
//		iComponentConverter = new ComponentConverter();
//		iComponentWriter = new ComponentWriter();

		coal = new Coal();
		coalParseStatus = new CoalParseStatus();
		iCoalParser = new CoalParser();
//		iCoalConverter = new CoalConverter();
//		iCoalWriter = new CoalWriter();

		speciesLink = new SpeciesLink();
		speciesLinkParseStatus = new SpeciesLinkParseStatus();
		iSpeciesLinkParser = new SpeciesLinkParser();
//		iSpeciesLinkConverter = new SpeciesLinkConverter();
//		iSpeciesLinkWriter = new SpeciesLinkWriter();

		amount = new Amount();
		amountParseStatus = new AmountParseStatus();
		iAmountParser = new AmountParser();
//		iAmountConverter = new AmountConverter();
//		iAmountWriter = new AmountWriter();

		uncertainty = new Uncertainty();
		uncertaintyParseStatus = new UncertaintyParseStatus();
		iUncertaintyParser = new UncertaintyParser();
//		iUncertaintyConverter = new UncertaintyConverter();
//		iUncertaintyWriter = new UncertaintyWriter();
		
		
		instanceSerialID = System.nanoTime(); // comment out this line when doing JUnit test
		chemicalSpeciesInstanceID = instanceSerialID;
		empiricalFormulaInstanceID = instanceSerialID;
		
		elementCount = 0;
		
		empiricalFormulaCreated = false;
		
		reasonerFactory = new StructuralReasonerFactory();
		queryResult = new ArrayList<String>();
		//================OWL to PrIMe conversion: Global================//
		//================Variable Initialisation Started===============//
		iChemicalSpeciesQuery = new ChemicalSpeciesQuery();
		iPreferredKeyQuery = new PreferredKeyQuery();
		iChemicalIdentifierQuery = new ChemicalIdentifierQuery();
		iChemicalCompositionQuery = new ChemicalCompositionQuery();

		
	}
}
