/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.parser.g09;

import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.Archive;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.GaussianHelper;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.JobSection;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemElementUtils;
import uk.ac.cam.ceb.como.compchem.info.ComputedInfoImpl;
import uk.ac.cam.ceb.como.compchem.info.MolecularInfoImpl;
import java.util.List;
import java.util.regex.Matcher;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
/**
 * 
 * @author nk510
 * Implements methods for parsing job section and archive.
 */
public class GeometryParser extends GaussianParser {
	
    private Logger logger = Logger.getLogger(GeometryParser.class);

    @Override
    public void parseSection() throws Exception {
    	
        JobSection jobsec = this.getGeometryJobSection(this.jSections);

        String archiveString = GaussianHelper.extractArchive(jobsec);

        // parse archive
        Archive archive = this.parseArchive(archiveString);
        //BasicInfoImpl basicInfo = (BasicInfoImpl) archive;
        MolecularInfoImpl molecularInfo = this.getMolecularInfo(jobsec);
        ComputedInfoImpl computedInfo = this.getComputedInfo(jobsec);

        List<CMLMolecule> molecules = this.parseMolecules(jobsec);
        CMLModule jobMod = CompChemElementUtils.addJob(((CompChem) (this.obj)).getFirstJobListModule());

        this.extractInitModule(jobMod, archive, molecularInfo, computedInfo, molecules);
        this.extractFinalModule(jobMod, archive, molecularInfo, computedInfo, molecules);
    }

    @Override
    public Archive parseArchive(String archiveStr) throws Exception {
        Matcher matcher = Archive.ARCHIVE_PATTERN.matcher(archiveStr);
        if (matcher.find()) {
            Archive arc = new Archive();
            arc.setJobType(matcher.group(1));
            arc.setMethod(matcher.group(2));
            arc.setBasis(matcher.group(3));
            arc.setEmpiricalFormula(matcher.group(4));
            arc.setRoute(matcher.group(5));
            arc.setTitle(matcher.group(6));
            return arc;
        } else {
            logger.warn("Archive is not found for a given String : " + archiveStr);
            return null;
        }
    }

    @Override
    public void clear() throws Exception {
        this.obj = null;
    }

    @Override
    public Object get() throws Exception {
        return this.obj;
    }
}
