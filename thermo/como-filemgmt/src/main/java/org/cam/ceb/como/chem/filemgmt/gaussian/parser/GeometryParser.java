/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.Archive;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.GaussianHelper;
import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.JobSection;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemElementUtils;
import gigadot.chom.compchem.info.ComputedInfoImpl;
import gigadot.chom.compchem.info.MolecularInfoImpl;
import java.util.List;
import java.util.regex.Matcher;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class GeometryParser extends GaussianParser {

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
}
