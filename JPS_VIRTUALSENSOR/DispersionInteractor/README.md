# Dispersion Interactor
Contains three API routes 

## InitialiseSimulation
URL: http://localhost:3838/dispersion-interactor/InitialialiseSimulation

Parameters:
1) ewkt
    - EWKT literal of the location to simulate, needs to be a rectangle, e.g. SRID=32630;POLYGON((1 1,2 1,2 2,1 2,1 1))
    - If submitting from the command line, spaces need to be replaced by %20
2) nx
    - number of cells in the x direction
3) ny
    - number of cells in the y direction

This route creates 1 dispersion derivation in the knowledge graph and ensures that there is one weather station within the simulation polygon.

If there are no errors, this should return the IRI of the created derivation, e.g. {"derivation": "http://derivation1"}.

## UpdateShipsAndSimulationTime
URL: http://localhost:3838/dispersion-interactor/UpdateShipsAndSimulationTime

1) This route calls the ShipInputAgent to add 1 timestep worth of data to the ships in the knowledge graph.
2) The second thing this route triggers is to make the dispersion derivations out-of-date by updating the timestamp of one of the derivation's inputs - SimulationTime, this is the real time at which the simulation is performed.

## TriggerUpdateDispersion
URL: http://localhost:3838/dispersion-interactor/TriggerUpdateDispersion

Parameter:
1) derivation
    - IRI of the derivation to update (output from InitialiseSimulation)

Updates the given derivation if it is out-of-date.