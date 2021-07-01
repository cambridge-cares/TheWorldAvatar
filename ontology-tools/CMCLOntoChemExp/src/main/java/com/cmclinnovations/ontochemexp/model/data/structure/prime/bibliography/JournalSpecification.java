package com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography;

public class JournalSpecification extends PublicationSpecification {
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
