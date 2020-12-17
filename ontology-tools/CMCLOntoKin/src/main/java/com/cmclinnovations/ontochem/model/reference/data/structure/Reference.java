package com.cmclinnovations.ontochem.model.reference.data.structure;

import java.util.List;

/**
 * Represents the model of a reference.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class Reference {

	private String title;
	
	private List<Creator> contributor;
	
	private PublicationSpecification publicationSpecification;
	
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<Creator> getContributor() {
		return contributor;
	}

	public void setContributor(List<Creator> contributor) {
		this.contributor = contributor;
	}

	public PublicationSpecification getPublicationSpecification() {
		return publicationSpecification;
	}

	public void setPublicationSpecification(PublicationSpecification publicationSpecification) {
		this.publicationSpecification = publicationSpecification;
	}
}
