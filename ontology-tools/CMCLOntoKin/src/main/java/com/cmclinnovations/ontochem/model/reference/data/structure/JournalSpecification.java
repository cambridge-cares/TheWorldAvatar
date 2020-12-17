package com.cmclinnovations.ontochem.model.reference.data.structure;
/**
 * Represents the model of a journal specification with respect to a reference.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class JournalSpecification extends PublicationSpecification{
	private Journal journal;
	private int volume;
	public Journal getJournal() {
		return journal;
	}
	public void setJournal(Journal journal) {
		this.journal = journal;
	}
	public int getVolume() {
		return volume;
	}
	public void setVolume(int volume) {
		this.volume = volume;
	}
}
