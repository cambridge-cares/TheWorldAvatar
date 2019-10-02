/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.reference;

import org.cam.ceb.como.nist.webbook.parser.NISTParser;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.StringList;
import org.cam.ceb.como.nist.webbook.parser.NISTHTMLReaderHelper;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author pb556
 */
public class NISTReferencesReader extends NISTParser {

    protected List<NISTReferences> references = new ArrayList<NISTReferences>();

    @Override
    public Object get() {
        return references;
    }

    @Override
    public void parseSection(StringList body) {
        try {
            StringList referencesStr = extractReferenceBlock(body);
            List<StringList> listRefs = extractReferences(referencesStr);
            references = parseReferences(listRefs);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(NISTReferencesReader.class.getName()).log(Level.SEVERE, "File not found!", ex);
        } catch (IOException ex) {
            Logger.getLogger(NISTReferencesReader.class.getName()).log(Level.SEVERE, "File could not be read!", ex);
        }
    }

    protected StringList extractReferenceBlock(StringList body) throws FileNotFoundException, IOException {
        // going through the StringList and identify the starting point of the references
        return NISTHTMLReaderHelper.extractHTMLContent(new File(path),
                "<h2><a id=\"Refs\" name=\"Refs\">References</a></h2>",
                "<h2><a id=\"Notes\" name=\"Notes\">Notes / Error Report</a></h2>");
    }

    protected List<StringList> extractReferences(StringList references) {
        List<StringList> refList = new ArrayList<StringList>();
        int ctr = 1;
        StringList ref = NISTHTMLReaderHelper.extractHTMLContent(references, "<a id=\"ref-" + ctr + "\" name=\"ref-" + ctr + "\">", "</p><p>");
        // based on the year choose the newest!!!
        while (ref.size() > 0) {
            refList.add(ref);
            ctr++;
            ref = NISTHTMLReaderHelper.extractHTMLContent(references, "<a id=\"ref-" + ctr + "\" name=\"ref-" + ctr + "\">", "</p><p>");
        }
        return refList;
    }

    protected List<NISTReferences> parseReferences(List<StringList> refs) {
        List<NISTReferences> references = new ArrayList<NISTReferences>();
        for (int i = 0; i < refs.size(); i++) {
            if (refs.get(i).size() > 4) {
                // reference
                NISTReferences newRef = null;

                try {
                    // 100-09-4
                    
                    // identify the individual lines!
                    
                    
                    String[] data = NISTHTMLReaderHelper.removeTags(refs.get(i).get(3)).split(",");
                    newRef = new NISTReferences(NISTHTMLReaderHelper.removeTags(refs.get(i).get(0)));
                    List<String> authors = Arrays.asList(NISTHTMLReaderHelper.removeTags(refs.get(i).get(1)).split(";"));
                    for (int j = 0; j < authors.size(); j++) {
                        String a = authors.get(j).trim();
                        if (a.endsWith(",")) {
                            a = a.substring(0, a.length() - 1);
                        }
                        authors.set(j, a);
                    }
                    newRef.setAuthors(authors);
                    newRef.setTitel(NISTHTMLReaderHelper.removeTags(refs.get(i).get(2)).trim().substring(0, NISTHTMLReaderHelper.removeTags(refs.get(i).get(2)).trim().length() - 1));
                    
                    newRef.setYear(Integer.parseInt(data[1].trim()));
                    newRef.setJournal(data[0].trim());
                    if (data.length > 3) {
                        newRef.setVolume(Integer.parseInt(data[2].trim()));
                        data[3] = data[3].replace(". [all data]", "");
                        newRef.setPages(Integer.parseInt(data[3].trim().split("-")[0]), Integer.parseInt(data[3].trim().split("-")[1]));
                    }
                } catch (Exception e) {
                    newRef = new NISTReferences(NISTHTMLReaderHelper.removeTags(refs.get(i).get(0)));
                    List<String> authors = Arrays.asList(NISTHTMLReaderHelper.removeTags(refs.get(i).get(1)).split(";"));
                    for (int j = 0; j < authors.size(); j++) {
                        String a = authors.get(j).trim();
                        if (a.endsWith(",")) {
                            a = a.substring(0, a.length() - 1);
                        }
                        authors.set(j, a);
                    }
                    newRef.setAuthors(authors);
                    newRef.setTitel(NISTHTMLReaderHelper.removeTags(refs.get(i).get(2)).trim());
                } finally {
                    if (newRef != null) {
                        references.add(newRef);
                    }
                }
            } else {
                System.out.println(path);
            }
        }
        return references;
    }
}
