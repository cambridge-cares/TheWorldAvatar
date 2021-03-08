/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.parser;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class NISTSpeciesList {

    protected ArrayList<NISTSpeciesId> species = new ArrayList<NISTSpeciesId>();
    private static Logger logger = Logger.getLogger(NISTSpeciesList.class);
    private int currentIndex = 0;
    private int startIndex = 0;
    private int endIndex = -1;

    public NISTSpeciesList() {
    }

    public boolean add(NISTSpeciesId id) {
        return species.add(id);
    }

    public boolean add(int index, NISTSpeciesId id) {
        if (index < 0 || index > species.size()) {
            logger.warn("Invalid index defined.");
            return false;
        }
        species.add(index, id);
        return true;
    }

    public boolean addAll(Collection<NISTSpeciesId> ids) {
        return species.addAll(ids);
    }

    public boolean addAll(int index, Collection<NISTSpeciesId> ids) {
        if (index < 0 || index > species.size()) {
            logger.warn("Invalid index defined.");
            return false;
        }
        return species.addAll(index, ids);
    }

    // remove
    public boolean remove(NISTSpeciesId id) {
        return species.remove(id);
    }

    public NISTSpeciesId remove(int index) {
        if (index < 0 || index > species.size()) {
            logger.warn("Invalid index defined.");
            return null;
        }
        return species.remove(index);
    }

    public boolean removeAll(Collection<NISTSpeciesId> ids) {
        return species.removeAll(ids);
    }
    
    public NISTSpeciesId set(int index, NISTSpeciesId id) {
        return species.set(index, id);
    }
    
    public List<NISTSpeciesId> get() {
        return species;
    }
    
    public NISTSpeciesId get(int index) {
        if (index < 0 || index >= species.size()) {
            logger.warn("Invalid index defined.");
            return null;
        }
        return species.get(index);
    }
    
    public void setStartIndex(int index) {
        if (index < 0 || index >= species.size()) {
            logger.warn("Invalid index defined.");
            return;
        }
        startIndex = index;
    }
    
    public void setEndIndex(int index) {
        if (index > species.size()) {
            logger.warn("Invalid index defined.");
            return;
        }
        endIndex = index;
    }
    
    public int getCurrentIndex() {
        return currentIndex;
    }
    
    public int getStartIndex() {
        return startIndex;
    }
    
    public int getEndIndex() {
        return endIndex;
    }
    
    public NISTSpeciesId next() {
        NISTSpeciesId id = null;
        if (currentIndex < species.size() && (currentIndex <= endIndex || endIndex < 0)) {
            id = species.get(currentIndex);
            currentIndex++;
        }
        return id;
    }
    
    public NISTSpeciesId previous() {
        NISTSpeciesId id = null;
        if (currentIndex >= 0 && currentIndex >= startIndex) {
            id = species.get(currentIndex);
            currentIndex--;
        }
        return id;
    }
}
