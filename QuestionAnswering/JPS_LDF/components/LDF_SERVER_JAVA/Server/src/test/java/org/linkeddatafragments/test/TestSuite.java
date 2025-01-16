package org.linkeddatafragments.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import org.linkeddatafragments.test.datasource.HdtDataSourceTest;
import org.linkeddatafragments.test.datasource.JenaTDBDataSourceTest;

/**
 *
 * @author <a href="mailto:bart.hanssens@fedict.be">Bart Hanssens</a>
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
   HdtDataSourceTest.class,
   JenaTDBDataSourceTest.class
})
public class TestSuite {
    
}
