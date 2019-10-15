/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.pool;

import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class CSVParserWriterTest {
    
    @Test
    public void ISDAppendTest() throws IOException {
        File csv = new File("test_data/test_isdcsvwriter.csv");
        FileUtils.deleteQuietly(csv);
        FileUtils.write(csv, "");

        CSVWriter writer = new SpeciesPoolWriter(csv);

        ArrayList<Species> dataSpecies = new ArrayList<Species>();
        dataSpecies.add(MockSpecies.getC2H4());
        dataSpecies.add(MockSpecies.getC2H4O());
        dataSpecies.add(MockSpecies.getC2H6());
        dataSpecies.add(MockSpecies.getC2H6O());
        dataSpecies.add(MockSpecies.getC3H6());
        dataSpecies.add(MockSpecies.getC3H8());

        writer.set(dataSpecies, false);
        writer.append();

        List<String> lines = FileUtils.readLines(csv);
        assert (lines.size() == dataSpecies.size());

        // need to use the parser
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false));
        }

        // append 2 more and check once again!
        ArrayList<Species> additionalSpecies = new ArrayList<Species>();
        additionalSpecies.add(MockSpecies.getCH4O());

        writer.set(additionalSpecies, false);
        writer.append();

        parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size() + additionalSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        additionalSpecies = new ArrayList<Species>();
        additionalSpecies.add(MockSpecies.getCH4());

        writer.set(additionalSpecies, true);
        writer.append();

        parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size() + additionalSpecies.size());
        assert (parser.getSpeciesOfInterest().size() == 1);
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getCH4(), true));
        }

        FileUtils.deleteQuietly(csv);
    }
    
     @Test
    public void ISDWriteTest() throws IOException {
        File csv = new File("test_data/test_isdcsvwriter.csv");
        FileUtils.deleteQuietly(csv);

        CSVWriter writer = new SpeciesPoolWriter(csv);

        ArrayList<Species> dataSpecies = new ArrayList<Species>();
        dataSpecies.add(MockSpecies.getC2H4());
        dataSpecies.add(MockSpecies.getC2H4O());
        dataSpecies.add(MockSpecies.getC2H6());
        dataSpecies.add(MockSpecies.getC2H6O());
        dataSpecies.add(MockSpecies.getC3H6());
        dataSpecies.add(MockSpecies.getC3H8());
        dataSpecies.add(MockSpecies.getCH4());
        dataSpecies.add(MockSpecies.getCH4O());

        writer.set(dataSpecies, false);
        writer.write();

        List<String> lines = FileUtils.readLines(csv);
        assert (lines.size() == dataSpecies.size());

        // need to use the parser
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        FileUtils.deleteQuietly(csv);
    }

    @Test
    public void ISGWriteTest() throws IOException {
        File csv = new File("test_data/test_isdcsvwriter.csv");
        FileUtils.deleteQuietly(csv);

        CSVWriter writer = new SpeciesPoolWriter(csv);

        ArrayList<Species> dataSpecies = new ArrayList<Species>();
        dataSpecies.add(MockSpecies.getC2H4());
        dataSpecies.add(MockSpecies.getC2H4O());
        dataSpecies.add(MockSpecies.getC2H6());
        dataSpecies.add(MockSpecies.getC2H6O());
        dataSpecies.add(MockSpecies.getC3H6());
        dataSpecies.add(MockSpecies.getC3H8());
        dataSpecies.add(MockSpecies.getCH4());
        dataSpecies.add(MockSpecies.getCH4O());

        writer.set(dataSpecies, false);
        writer.write();

        List<String> lines = FileUtils.readLines(csv);
        assert (lines.size() == dataSpecies.size());

        // need to use the parser
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        FileUtils.deleteQuietly(csv);
    }

    @Test
    public void ISGAppendTest() throws IOException {
        File csv = new File("test_data/test_isdcsvwriter.csv");
        FileUtils.deleteQuietly(csv);

        CSVWriter writer = new SpeciesPoolWriter(csv);

        ArrayList<Species> dataSpecies = new ArrayList<Species>();
        dataSpecies.add(MockSpecies.getC2H4());
        dataSpecies.add(MockSpecies.getC2H4O());
        dataSpecies.add(MockSpecies.getC2H6());
        dataSpecies.add(MockSpecies.getC2H6O());
        dataSpecies.add(MockSpecies.getC3H6());
        dataSpecies.add(MockSpecies.getC3H8());

        writer.set(dataSpecies, false);
        writer.append();

        List<String> lines = FileUtils.readLines(csv);
        assert (lines.size() == dataSpecies.size());

        // need to use the parser
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false));
        }

        // append 2 more and check once again!
        ArrayList<Species> additionalSpecies = new ArrayList<Species>();
        additionalSpecies.add(MockSpecies.getCH4O());

        writer.set(additionalSpecies, false);
        writer.append();

        parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size() + additionalSpecies.size());
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }

        additionalSpecies = new ArrayList<Species>();
        additionalSpecies.add(MockSpecies.getCH4());

        writer.set(additionalSpecies, true);
        writer.append();

        parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == dataSpecies.size() + additionalSpecies.size());
        assert (parser.getSpeciesOfInterest().size() == 1);
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getCH4(), true));
        }

        FileUtils.deleteQuietly(csv);
    }

    @Test
    public void refISDParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/ref.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 8);
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
    }

    @Test
    public void refISGParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/ref.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 8);
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
    }

    @Test
    public void refMissingISDParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/ref_missing.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 6);
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
    }

    @Test
    public void refMissingISGParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/ref_missing.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 6);
        assert (parser.getSpeciesOfInterest().isEmpty());
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
    }

    @Test
    public void soiISDParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/soi.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().isEmpty());
        assert (parser.getSpeciesOfInterest().size() == 2);
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getC2H6O(), true)
                    || s.equals(MockSpecies.getCH4(), true));
        }
    }

    @Test
    public void soiISGParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/soi.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().isEmpty());
        assert (parser.getSpeciesOfInterest().size() == 2);
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getC2H6O(), true)
                    || s.equals(MockSpecies.getCH4(), true));
        }
    }

    @Test
    public void mixISDParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/mix.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 8);
        assert (parser.getSpeciesOfInterest().size() == 2);
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getC2H6O(), true)
                    || s.equals(MockSpecies.getCH4(), true));
        }
    }

    @Test
    public void mixISGParserTest() throws FileNotFoundException, IOException {
        File csv = new File("test_data/csv/mix.csv");
        CSVParser parser = new SpeciesPoolParser(csv);
        parser.parse();
        assert (parser.getRefSpecies().size() == 8);
        assert (parser.getSpeciesOfInterest().size() == 2);
        for (Species s : parser.getRefSpecies()) {
            assert (s.equals(MockSpecies.getC2H4(), false)
                    || s.equals(MockSpecies.getC2H4O(), false)
                    || s.equals(MockSpecies.getC2H6(), false)
                    || s.equals(MockSpecies.getC2H6O(), false)
                    || s.equals(MockSpecies.getC3H8(), false)
                    || s.equals(MockSpecies.getC3H6(), false)
                    || s.equals(MockSpecies.getCH4(), false)
                    || s.equals(MockSpecies.getCH4O(), false));
        }
        for (Species s : parser.getSpeciesOfInterest()) {
            assert (s.equals(MockSpecies.getC2H6O(), true)
                    || s.equals(MockSpecies.getCH4(), true));
        }
    }
}
