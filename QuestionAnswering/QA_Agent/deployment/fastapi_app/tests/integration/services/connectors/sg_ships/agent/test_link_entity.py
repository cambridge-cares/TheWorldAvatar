import pytest

from services.connectors.sg_ships.agent.link_entity import ShipIdentifier, ShipLinker


@pytest.fixture(scope="class")
def ship_linker(redis_client):
    yield ShipLinker(
        redis_client=redis_client,
        ships=[
            ShipIdentifier(
                IRI="http://example.com/ships/1",
                MMSI="518100368",
                name="Ship: WHITE RABBIT",
            ),
            ShipIdentifier(
                IRI="http://example.com/ships/2",
                MMSI="518998488",
                name="Ship: PAIZA ONE",
            ),
            ShipIdentifier(
                IRI="http://example.com/ships/3",
                MMSI="525119038",
                name="Ship: PRIMA TANGGUH LVI",
            ),
        ],
    )


class TestShipLinker:
    def test_linkEntity(self, ship_linker: ShipLinker):
        actual = ship_linker.lookup_by_name("White Rabbit")
        expected = [
            ShipIdentifier(
                IRI="http://example.com/ships/1",
                MMSI="518100368",
                name="Ship: WHITE RABBIT",
            )
        ]
        assert actual == expected

    def test_lookupByMMSI(self, ship_linker: ShipLinker):
        actual = ship_linker.lookup_by_mmsi("525119038")
        expected = ShipIdentifier(
            IRI="http://example.com/ships/3",
            MMSI="525119038",
            name="Ship: PRIMA TANGGUH LVI",
        )
        assert actual == expected
