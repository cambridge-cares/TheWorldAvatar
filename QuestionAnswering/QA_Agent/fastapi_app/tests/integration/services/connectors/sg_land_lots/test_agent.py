import os

import pytest
from model.aggregate import AggregateOperator

from model.constraint import (
    AtomicNumericalConstraint,
    CompoundNumericalConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
    NumericalOperator,
)
from services.connectors.sg_land_lots.constants import PlotAttrKey
from services.kg_client import KgClient
from services.connectors.sg_land_lots.agent import PlotConstraints, SGLandLotsAgent


@pytest.fixture
def agent(redis_client, embedder):
    ontop_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS_ONTOP"))
    bg_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS"))
    yield SGLandLotsAgent(
        ontop_client=ontop_client,
        bg_client=bg_client,
        redis_client=redis_client,
        embedder=embedder,
    )


class TestSingporeLandLotAgent:
    def test_findPlotIri(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
            gross_plot_ratio=CompoundNumericalConstraint(
                logical_operator=LogicalOperator.AND,
                constraints=[
                    AtomicNumericalConstraint(operator=NumericalOperator.GT, operand=1),
                    AtomicNumericalConstraint(operator=NumericalOperator.LT, operand=3),
                ],
            ),
            plot_area=CompoundNumericalConstraint(
                logical_operator=LogicalOperator.OR,
                constraints=[
                    AtomicNumericalConstraint(
                        operator=NumericalOperator.GT, operand=400
                    ),
                    AtomicNumericalConstraint(
                        operator=NumericalOperator.LT, operand=50
                    ),
                ],
            ),
            gross_floor_area=CompoundNumericalConstraint(
                constraints=[
                    AtomicNumericalConstraint(
                        operator=NumericalOperator.GT, operand=1000
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
        print(iris)

    def test_lookupPlotAttributes(self, agent: SGLandLotsAgent):
        # Arrange
        plot_args = PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_618ab6ea-3d41-4841-95ef-369f000e5075",
            gross_floor_area=CompoundNumericalConstraint(
                constraints=[
                    AtomicNumericalConstraint(
                        operator=NumericalOperator.LT, operand=50
                    ),
                ]
            ),
        )

        # Act
        data = agent.lookup_plot_attributes(
            plot_args, attr_keys=[PlotAttrKey.PLOT_AREA]
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
        attr_aggs = [
            (PlotAttrKey.PLOT_AREA, AggregateOperator.AVG),
            (PlotAttrKey.GROSS_PLOT_RATIO, AggregateOperator.MIN),
        ]

        # Act
        data = agent.compute_aggregate_plot_attributes(
            plot_constraints=plot_args, attr_aggs=attr_aggs
        )

        # Assert
        assert data.vars == [
            "PlotAreaNumericalValueAVG",
            "GrossPlotRatioNumericalValueMIN",
        ]
        assert all(
            "PlotAreaNumericalValueAVG" in binding
            and "GrossPlotRatioNumericalValueMIN" in binding
            for binding in data.bindings
        )
