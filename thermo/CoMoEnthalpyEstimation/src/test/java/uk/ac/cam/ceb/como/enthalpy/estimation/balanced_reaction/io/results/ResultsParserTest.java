/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsParser;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsDataList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results.ResultsData;
import java.io.File;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ResultsParserTest {
    
    // read a prepared file and verify if the content is read correctly
    protected File res = new File("test_data/res/test_parsing.res");
    
    // make use of mock reactions
    @Test
    public void resultsWriterTest() throws Exception {
        ResultsParser parser = new ResultsParser(res);
        parser.parse();
        
        ResultsDataList data = (ResultsDataList) parser.get();

        ResultsData data1 = MockResultsData.getC2H4ResultsData("dataSet1", "This is data set 1", 2);
        ResultsData data2 = MockResultsData.getC2H4ResultsData("dataSet2", "And another data set", 10);
        ResultsData data3 = MockResultsData.getC2H4ResultsData("dataSet3", "Why not another one?", 1);
        ResultsData data4 = MockResultsData.getC2H4ResultsData("dataSet4", "This is data set 5", 5);
        
        ResultsDataList dataCmp = new ResultsDataList();
        dataCmp.add(data1);
        dataCmp.add(data2);
        dataCmp.add(data3);
        dataCmp.add(data4);
        
        assert (data.size() == dataCmp.size());
        for (ResultsData d : data) {
            boolean identified = false;
            for (ResultsData cmp : dataCmp) {
                if (d.equals(cmp)) {
                    identified = true;
                    break;
                }
            }
            assert (identified);
        }
    } 
}
