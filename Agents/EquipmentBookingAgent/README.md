# Equipment Booking Agent
The Equipment Booking Agent is a standalone application for booking equipment through a browser-based UI. This will require bookable assets as well as eligible persons to be instantiated and annotated with booking intervals. 

## 1. Instructions

### 1.1 Setup
Add individual configuration parameters to conf/equipment_booking_agent_conf.py:
- SPARQL_QUERY_ENDPOINT: URL address of available SPARQL endpoint where assets and users are instantiated
- SPARQL_UPDATE_ENDPOINT: URL address of available SPARQL endpoint where bookings will be instantiated
- KG_USERNAME: if required, credentials to access the SPARQL endpoint
- KG_PASSWORD: if required, credentials to access the SPARQL endpoint

### 1.2 Debugging
This agent can be built via pip. All dependencies listed in setup.py will be built accordingly
```bash
# Building the editable development version
pip install -e .
```

Running entry_point.py spins up the agent on http://localhost:5000, waiting for the debugger to attach. 
```bash
# Spinning up agent via entry point
python .\equipmentbookingagent\entry_point.py
```

The terminal now shows messages related to the agent usage. A Debugger is started with a PIN displayed. This can be used in an Interactive Console, accessible under http://localhost:5000/console.

### 1.3 Deployment
First, custom changes to entry_point.py can be made. Most importantly, debug mode needs to be removed. The version used for debugging as described above will look like this:
```python
if __name__ == '__main__':
    app = create_app()
    app.run(debug=True)
```
Instead, the production version should look something like this (replace host IP and port number accordingly):
```python
if __name__ == '__main__':
    app = create_app()
    app.run(host="0.0.0.0", port=8000, debug=False)
```

The agent can be built via pip.
```bash
# Building the non-editable production package
pip install .
```

Running entry_point.py spins up the agent on http://localhost:5000. 
```bash
# Spinning up agent via entry point
python .\equipmentbookingagent\entry_point.py
```


### 1.4 Usage
Go to 'Book Equipment', select user, select equipment, and booking interval. If equipment is not available at the selected interval, you will be informed.
Go to 'Overview' for a  daily list of all booked assets in 15 minute increments.
Go to 'Edit bookings' for a list of all active bookings for a certain equipment where they can be deleted.
Go to 'Add user' to add a new lab user that can make bookings.

The 'Back' button on each page brings you back to the main menu.


## 2. Data model
**Legend**
Prefix | Namespace
--- | ---
[bot](https://w3c-lbd-cg.github.io/bot/) | `https://w3id.org/bot#`
[fibo](https://github.com/edmcouncil/fibo/tree/master/FND/AgentsAndPeople) | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/`
[ontoam](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontoassetmanagement) | `https://www.theworldavatar.com/kg/ontoassetmanagement/`
[ontobim](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontobim) | `https://www.theworldavatar.com/kg/ontobim/`
[ontodevice](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontodevice) | `https://www.theworldavatar.com/kg/ontodevice/`
[ontolab](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontolab) | `https://www.theworldavatar.com/kg/ontolab/`
[ontotechsystem](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontotechnicalsystem) | `https://www.theworldavatar.com/kg/ontotechnicalsystem/`
[ontotimeseries](https://github.com/TheWorldAvatar/ontology/tree/main/ontology/ontotimeseries) | `https://www.theworldavatar.com/kg/ontotimeseries/`
[time](https://www.w3.org/TR/owl-time/) | `http://www.w3.org/2006/time#`


### 2.1 Requirements
A bookable asset is required to which a booking system can be attached to. These can be of type ontodevice:Device, ontotechsystem:TechnicalSystem, or bot:Element. One example is shown in *Figure 1*. In the utils folder, utility scripts are available to add a booking system to an asset or combine existing devices to a system so they can be booked together.

*Figure 1. Bookable asset example ABox*
```mermaid
    erDiagram 
    "RoomRepresentation_Instance" {rdf-type ontobim-IfcRoomRepresentation}
    "RoomRepresentation_Instance" {rdfs-label High_Pressure_Catalysis_Facility}
    "Room_Instance" ||--o{ "RoomRepresentation_Instance" : "bim:hasIfcRepresentation"
    "Room_Instance" {rdf-type ontobim-Room}
    "Room_Instance" ||--o{ "Asset_Instance" : "bot:containsElement"
    "Asset_Instance" {rdfs-label Vigor_Glove_Box_Sg1200}
    "Asset_Instance" {rdf-type ontodevice-Device}
    "Asset_Instance" {rdf-type ontolab-GloveBox}
    "Person_Instance" {rdf-type fibo-Person}
    "Asset_Instance" ||--o{ "Person_Instance" : "oam:assignedTo"
    "Asset_Instance" ||--o{ "BookingSystem_Instance" : "oam:hasBookingSystem"
    "BookingSystem_Instance" {rdf-type oam-BookingSystem}
    "BookingSystem_Instance" {rdfs-label GloveBox_Lab1}
    "Person_Instance" ||--o{ "PersonName_Instance" : "omg:hasName"
    "PersonName_Instance" {rdf-type fibo-PersonName}
    "PersonName_Instance" {fibo-FirstName Max}
    "PersonName_Instance" {fibo-LastName Mustermann}
    "PersonName_Instance" {rdfs-label Max_Mustermann}
```

An eligible lab user is required to carry out the booking of an asset. These are of type fibo:Person and have been assigned the role of lab user. One example of a lab user is shown in *Figure 2*, additional lab users can be added via the user interface.

*Figure 2. Eligible user example ABox*
```mermaid
    erDiagram 
    "Person_Instance" ||--o{ "PersonName_Instance" : "omg:hasName"
    "PersonName_Instance" {rdf-type fibo-PersonName}
    "PersonName_Instance" {rdfs-label Erika_Musterfrau}
    "Person_Instance" ||--o{ "Role_Instance" : "omg:playsRole"
    "Role_Instance" {rdf-type omg-FunctionalRole}
    "Role_Instance" {rdfs-label Lab_User}
```

### 2.2 Operating principle
Adding a booking to the system attaches a new booking instance to the booking system instance of the asset in question. This will look similar to the example in *Figure 3*.

*Figure 3. Booking example ABox* 
```mermaid
    erDiagram 
    "Asset_Instance" ||--o{ "BookingSystem_Instance" : "oam:hasBookingSystem"
    "BookingSystem_Instance" {rdf-type oam-BookingSystem}
    "BookingSystem_Instance" {rdfs-label GloveBox_Lab1}
    "Booking_Instance" {rdf-type oam-Booking}
    "BookingSystem_Instance" ||--o{ "Booking_Instance" : "oam:hasBooking"
    "Booking_Instance" ||--o{ "Person_Instance" : "oam:hasBooker"
    "Person_Instance" {rdf-type fibo-Person}
    "BookingPeriod_Instance" {rdf-type oam-BookingPeriod}
    "Booking_Instance" ||--o{ "BookingPeriod_Instance" : "oam:hasBookingPeriod"
    "BookingPeriod_Instance" ||--o{ "PeriodBeginning_Instance" : "time:hasBeginning"
    "BookingPeriod_Instance" ||--o{ "PeriodEnd_Instance" : "time:hasEnd"
    "PeriodBeginning_Instance" {rdf-type time-Instant}
    "PeriodEnd_Instance" {rdf-type time-Instant}
    "TimePosition1_Instance" {rdf-type time-TimePosition}
    "TimePosition1_Instance" {time-numericPosition _1708344000}
    "TimePosition1_Instance" {time-hasTRS Unix_time}
    "TimePosition2_Instance" {rdf-type time-TimePosition}
    "TimePosition2_Instance" {time-numericPosition _1708545600}
    "TimePosition2_Instance" {time-hasTRS Unix_time}
    "PeriodBeginning_Instance" ||--o{ "TimePosition1_Instance" : "time:numericPosition"
    "PeriodEnd_Instance" ||--o{ "TimePosition2_Instance" : "time:numericPosition"
```