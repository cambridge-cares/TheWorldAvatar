package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UnitMapperTest {

    @Test
    void testGetUnitSyntax_ExistingUnit() {
        assertEquals("kwatth", UnitMapper.getUnitSyntax("kwh"));
        assertEquals("celsius", UnitMapper.getUnitSyntax("°c"));
        assertEquals("percent", UnitMapper.getUnitSyntax("%"));
        assertEquals("", UnitMapper.getUnitSyntax("null"));
    }

    @Test
    void testGetUnitSyntax_VaryingCapitalisedUnits() {
        assertEquals("kwatth", UnitMapper.getUnitSyntax("kWh"));
        assertEquals("kwatth", UnitMapper.getUnitSyntax("KWH"));
        assertEquals("celsius", UnitMapper.getUnitSyntax("°C"));
        assertEquals("", UnitMapper.getUnitSyntax("NULL"));
    }

    @Test
    void testGetUnitSyntax_NonExistingUnit() {
        assertEquals("unknownUnit", UnitMapper.getUnitSyntax("unknownUnit"));
        assertEquals("", UnitMapper.getUnitSyntax(""));
    }
}