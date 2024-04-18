from services.connectors.retrieval.agent.node_store import (
    IncomingLink,
    Node,
    NodeAttr,
    NodeDoc,
    NodeStore,
    OutgoingLink,
)


class TestNodeStore:
    def test_linearizeNode(self):
        # Arrange
        node_doc = NodeDoc(
            node=Node(IRI="http://test.org/1", label="one"),
            outgoing_links=[
                OutgoingLink(
                    relation="http://test.org/isSiblingOf",
                    tail=Node(IRI="http://test.org/2", label="two"),
                )
            ],
            incoming_links=[
                IncomingLink(
                    head=Node(IRI="http://test.org/3", label="three"),
                    relation="http://test.org/isParentOf",
                )
            ],
            attributes=[
                NodeAttr(relation="http://test.org#length", literal="10"),
                NodeAttr(relation="http://test.org#age", literal="20"),
            ],
        )
        expected = """one length 10
one age 20
one isSiblingOf two
three isParentOf one"""

        # Act
        actual = NodeStore.linearize_doc(node_doc)

        # Assert
        assert actual == expected
