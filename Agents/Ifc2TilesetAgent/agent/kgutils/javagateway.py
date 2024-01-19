"""
# Author: qhouyee #

This module creates and starts the JAVA resource gateway objects to the TWA base library.
"""

# Third party imports
from py4jps.resources import JpsBaseLib


jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
