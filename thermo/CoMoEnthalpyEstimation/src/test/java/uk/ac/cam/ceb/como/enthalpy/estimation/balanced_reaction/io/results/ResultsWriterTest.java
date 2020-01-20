/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsDataList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsData;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ResultsWriterTest {
    
    protected File res = new File("test_data/res/test_writting.res");
    
    // make use of mock reactions
    @Test
    public void resultsWriterTest() throws Exception {
        ResultsData data1 = MockResultsData.getC2H4ResultsData("dataSet1", "This is data set 1", 2);
        ResultsData data2 = MockResultsData.getC2H4ResultsData("dataSet2", "And another data set", 10);
        ResultsData data3 = MockResultsData.getC2H4ResultsData("dataSet3", "Why not another one?", 1);
        ResultsData data4 = MockResultsData.getC2H4ResultsData("dataSet4", "This is data set 5", 5);
        
        ResultsDataList data = new ResultsDataList();
        data.add(data1);
        data.add(data2);
        data.add(data3);
        data.add(data4);
        
        ResultsWriter writer = new ResultsWriter(res);
        writer.set(data);
        writer.overwrite(true);
        writer.write();
        
        // check if the file is written and if the lines are correct!!!
        
        // line-by-line (use bytereader)
        ArrayList<String> lines = new ArrayList<String>();
        lines.add("\"dataSet1\", \"C2H4\", \"-206315.5126569827\", \"52.47\", \"{[47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304]}\", \"This is data set 1\"");
        lines.add("\"dataSet2\", \"C2H4\", \"-206315.5126569827\", \"52.47\", \"{[47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.67526202582289]}\", \"And another data set\"");
        lines.add("\"dataSet3\", \"C2H4\", \"-206315.5126569827\", \"52.47\", \"{[47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304]}\", \"Why not another one?\"");
        lines.add("\"dataSet4\", \"C2H4\", \"-206315.5126569827\", \"52.47\", \"{[47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304][47.15847580437082, 47.87408015986438, 53.55611207222097, 42.19204824739137, 44.675262025939304]}\", \"This is data set 5\"");
        
        // check
        List<String> rLines = FileUtils.readLines(res);
        assert (lines.size() == rLines.size());
        
        for (String l : lines) {
            boolean identified = false;
            for (String lcmp : rLines) {
                if (l.trim().compareToIgnoreCase(lcmp.trim()) == 0) {
                    identified = true;
                    break;
                }
            }
            //assert (identified);
        }
    }
    
    @After
    public void cleanUp() {
        res.deleteOnExit();
        res.deleteOnExit();
    }
}
