/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.reference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @author pb556
 */
public class NISTReferences {

    protected List<String> authors = new ArrayList<String>();
    protected String id = "";
    protected String titel = "";
    protected String journal = "";
    protected int year = 0;
    protected int vol = 0;
    protected int pagesFrom = 0;
    protected int pagesTo = 0;

    public NISTReferences(String id) {
        this.id = id;
    }
    
    public void setAuthors(List<String> authors) {
        this.authors = authors;
    }
    
    public boolean addAuthor(String author) {
        return authors.add(author);
    }

    public boolean addAllAuthors(Collection<String> authors) {
        return this.authors.addAll(authors);
    }

    public void clearAuthors() {
        authors.clear();
    }

    public boolean removeAuthor(String author) {
        return authors.remove(author);
    }

    public boolean removeAllAuthors(Collection<String> authors) {
        return this.authors.removeAll(authors);
    }

    public List<String> getAuthors() {
        return authors;
    }

    public void setTitel(String titel) {
        this.titel = titel;
    }

    public void setJournal(String journal) {
        this.journal = journal;
    }

    public void setYear(int year) {
        this.year = year;
    }

    public void setVolume(int vol) {
        this.vol = vol;
    }

    public void setPages(int from, int to) {
        pagesFrom = from;
        pagesTo = to;
    }
    
    public String getId() {
        return id;
    }

    public String getTitel() {
        return titel;
    }

    public String getJournal() {
        return journal;
    }

    public int getYear() {
        return year;
    }

    public int getVolume() {
        return vol;
    }

    public int getPagesFrom() {
        return pagesFrom;
    }

    public int getPagesTo() {
        return pagesTo;
    }
}
