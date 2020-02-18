package com.cmclinnovations.jps.kg.object.model;

public class JobRequest {
	private Job job;
	private String speciesIRI;
	public Job getJob() {
		return job;
	}
	public void setJob(Job job) {
		this.job = job;
	}
	public String getSpeciesIRI() {
		return speciesIRI;
	}
	public void setSpeciesIRI(String speciesIRI) {
		this.speciesIRI = speciesIRI;
	}
}
