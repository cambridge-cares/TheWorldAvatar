package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class ContentParser extends PrimeSpeciesConverter implements IContentParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemContent())) {
			contentParseStatus.setContent(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemContent())) {
			String source = attributes.getValue(primeSpeciesVocabulary.getAttribSource());
			if (source != null) {
				content.setSource(source);
				contentParseStatus.setSource(true);
				contentParseStatus.setContent(true);
			}

			String copyrighted = attributes.getValue(primeSpeciesVocabulary.getAttribCopyrighted());
			if (copyrighted != null) {
				content.setCopyrighted(copyrighted);
				contentParseStatus.setCopyrighted(true);
				contentParseStatus.setContent(true);
			}

			String bibliography = attributes.getValue(primeSpeciesVocabulary.getAttribBibliography());
			if (bibliography != null) {
				content.setBibliography(bibliography);
				contentParseStatus.setBibliography(true);
				contentParseStatus.setContent(true);
			}
		}
	}
}
