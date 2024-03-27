import os

import pytest

from model.aggregate import AggregateOperator
from model.constraint import (
    UnaryComparativeConstraint,
    CompoundComparativeConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
    ComparativeOperator,
)
from services.core.kg import KgClient
from fastapi_app.services.connectors.sg_land_lots.model import PlotAttrKey
from services.connectors.sg_land_lots.agent import PlotConstraints, SGLandLotsAgent


@pytest.fixture
def agent():
    # TODO: use dedicated Blazegraph server for testing
    ontop_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS_ONTOP"))
    bg_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS"))
    yield SGLandLotsAgent(
        ontop_client=ontop_client,
        bg_client=bg_client,
    )


class TestSingporeLandLotAgent:
    def test_findPlotIri(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
            gross_plot_ratio=CompoundComparativeConstraint(
                logical_operator=LogicalOperator.AND,
                constraints=[
                    UnaryComparativeConstraint(operator=ComparativeOperator.GT, operand=1),
                    UnaryComparativeConstraint(operator=ComparativeOperator.LT, operand=3),
                ],
            ),
            plot_area=CompoundComparativeConstraint(
                logical_operator=LogicalOperator.OR,
                constraints=[
                    UnaryComparativeConstraint(
                        operator=ComparativeOperator.GT, operand=400
                    ),
                    UnaryComparativeConstraint(
                        operator=ComparativeOperator.LT, operand=50
                    ),
                ],
            ),
            gross_floor_area=CompoundComparativeConstraint(
                constraints=[
                    UnaryComparativeConstraint(
                        operator=ComparativeOperator.GT, operand=1000
                    ),
                ]
            ),
        )

        # Act
        iris = agent.find_plot_iris(plot_args)

        # Assert
        assert "https://www.theworldavatar.com/kg/landplot/94739" in iris

    def test_findPlotIri_greatestPlotArea(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
            plot_area=ExtremeValueConstraint.MAX,
            num=2,
        )

        # Act
        iris = agent.find_plot_iris(plot_args)

        # Assert
        assert len(iris) == 2

    def test_lookupPlotAttributes(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
            gross_floor_area=CompoundComparativeConstraint(
                constraints=[
                    UnaryComparativeConstraint(
                        operator=ComparativeOperator.LT, operand=50
                    ),
                ]
            ),
        )

        # Act
        data = agent.lookup_plot_attribute(
            plot_args, attr_key=PlotAttrKey.PLOT_AREA
        )

        # Assert
        assert data.vars == ["IRI", "PlotArea"]
        assert all(
            "IRI" in binding and "PlotArea" in binding for binding in data.bindings
        )

    def test_computeAggregatePlotAttributes(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075"
        )
        attr_agg = (PlotAttrKey.PLOT_AREA, AggregateOperator.AVG)

        # Act
        data = agent.compute_aggregate_plot_attribute(
            constraints=plot_args, attr_agg=attr_agg
        )

        # Assert
        assert data.vars == [
            "PlotAreaNumericalValueAVG",
        ]
        assert all(
            "PlotAreaNumericalValueAVG" in binding
            for binding in data.bindings
        )
