package uk.ac.cam.cares.jps.agent.historicalntuenergy;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.util.*;
import org.json.JSONObject;
import org.json.JSONArray;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


public class CourseInstantiator{

    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);
    /**
     * Tbox Prefixes
     */
    public static final String classSchedulePrefix = "https://www.theworldavatar.com/kg/ontoclassschedule/";
    public static final String botPrefix = "https://w3id.org/bot#";
    public static final String bimPrefix = "https://www.theworldavatar.com/kg/ontobim/";
    public static final String timePrefix = "http://www.w3.org/2006/time#";


    /**
     * Classes
     */
    public static final String botBuilding = botPrefix + "Building";
    public static final String storey = botPrefix + "Storey";
    public static final String Room = "https://www.theworldavatar.com/kg/ontobim#Room";
    public static final String LectureTheatre = bimPrefix + "LectureTheatre";
    public static final String Laboratory = bimPrefix + "Laboratory";
    public static final String TutorialRoom = bimPrefix + "TutorialRoom";
    public static final String SeminarRoom = bimPrefix + "SeminarRoom";
    public static final String Session = classSchedulePrefix + "Session";
    public static final String Course = classSchedulePrefix + "Course";
    public static final String LaboratorySession = classSchedulePrefix + "Laboratory";
    public static final String TutorialSession = classSchedulePrefix + "Tutorial";
    public static final String LectureSession = classSchedulePrefix + "Lecture";
    public static final String SeminarSession = classSchedulePrefix + "Seminar";

    /**
     * Predicates
     */
    private static final String subClassOf = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
    private static final String equivalent = "http://www.w3.org/2002/07/owl#equivalentClass";
    private static final String hasStorey = botPrefix + "hasStorey";
    private static final String hasFacility = bimPrefix + "hasFacility";
    private static final String hasCapacity = bimPrefix + "hasCapacity";
    private static final String hasSession = classSchedulePrefix + "hasSession";
    private static final String hasDayOfWeek = classSchedulePrefix + "hasDayOfWeek";
    private static final String hasStartTime = classSchedulePrefix + "hasStartTime";
    private static final String hasEndTime = classSchedulePrefix + "hasEndTime";
    private static final String isPartOfCourse = classSchedulePrefix + "isPartOfCourse";
    private static final String hasCourseCode = classSchedulePrefix + "hasCourseCode";
    private static final String hasCourseName = classSchedulePrefix + "hasCourseName";
    private static final String hasCourseRemark = classSchedulePrefix + "hasCourseRemark";

    /**
     * Individuals
     */
    private static final String Monday = timePrefix + "Monday";
    private static final String Tuesday = timePrefix + "Tuesday";
    private static final String Wednesday = timePrefix + "Wednesday";
    private static final String Thursday = timePrefix + "Thursday";
    private static final String Friday = timePrefix + "Friday";
    private static final String Saturday = timePrefix + "Saturday";
    private static final String Sunday = timePrefix + "Sunday";

    HashMap<String, List<String>> NTUClassSchedule = new HashMap<String, List<String>>();

    RemoteStoreClient kbClient;
    public CourseInstantiator(RemoteStoreClient kbClient, JSONArray classSchedule){
        this.kbClient = kbClient;
        // pop up class schedule into the hashmap
        for (int i = 0; i < classSchedule.length(); i++) {
            LOGGER.info("poping up class schedule: " + i);
            JSONObject jsonObject = classSchedule.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!NTUClassSchedule.containsKey(key)) {
                    NTUClassSchedule.put(key, new ArrayList<>());
                }
                NTUClassSchedule.get(key).add(value);
            }
        }
    }

    public void instantiateTriples(){

        /**
         * Instantiate all courses
         */
        Map<String, String> uniqueClasses = new HashMap<>();
        Set<String> uniqueRooms = new HashSet<>();

        // Loop through the data and populate the unique class map
        for (int entry=0; entry < NTUClassSchedule.get("ClassCode").size(); entry++){
            String classCode = NTUClassSchedule.get("ClassCode").get(entry);
            String className = NTUClassSchedule.get("ClassName").get(entry);
            if (!uniqueClasses.containsKey(classCode) || !uniqueClasses.get(classCode).equals(className)) {
                uniqueClasses.put(classCode, className);
            }
        }

        // For each unique course, instantiate its triples
        for (Map.Entry<String, String> entry : uniqueClasses.entrySet()) {
            String courseIRI = classSchedulePrefix + "NTU_Course_" + entry.getKey();
            TriplePattern courseIsType = iri(courseIRI).isA(iri(Course));

            TriplePattern courseHasCode = iri(courseIRI).has(iri(hasCourseCode), entry.getKey());
            TriplePattern courseHasName = iri(courseIRI).has(iri(hasCourseName), entry.getValue());
            System.out.println("Instantiating ClassCode: " + entry.getKey() + ", ClassName: " + entry.getValue());
            InsertDataQuery courseInsertion = Queries.INSERT_DATA(courseIsType, courseHasCode, courseHasName);
            kbClient.executeUpdate(courseInsertion.getQueryString());
        }

        // Loop through the data and populate the unique room
        for (int entry=0; entry < NTUClassSchedule.get("VENUE").size(); entry++){
            String room = NTUClassSchedule.get("VENUE").get(entry);
            uniqueRooms.add(room);
        }

        // For each unique room, instantiate its triples
        for(String room : uniqueRooms){
            room = room.replace(" ", "_" );
            System.out.println("Instantiating Room: " + room);
            String roomIRI = bimPrefix + "NTU_Room_" + room;
            TriplePattern roomIsType = iri(roomIRI).isA(iri(Room));
            String building = "https://www.theworldavatar.com/kg/ontopowsys/NTU_Building_SPMS";
            TriplePattern buildingHasFacilityRoom = iri(building).has(iri(hasFacility), iri(roomIRI));

            InsertDataQuery roomInsertion = Queries.INSERT_DATA(roomIsType, buildingHasFacilityRoom);
            kbClient.executeUpdate(roomInsertion.getQueryString());
        }

        TriplePattern sessionIsType;
        TriplePattern sessionHasDayOfWeek;
        //For each session, instantiate its triples
        for (int entry=0; entry < NTUClassSchedule.get("TYPE").size(); entry++){

            String sessionType = NTUClassSchedule.get("TYPE").get(entry);
            String sessionIRI = classSchedulePrefix + "Session_" + NTUClassSchedule.get("INDEX").get(entry);

            switch(sessionType){
                case "SEM":
                    sessionIsType = iri(sessionIRI).isA(iri(SeminarSession));
                    break;
                case "LEC/STUDIO":
                    sessionIsType = iri(sessionIRI).isA(iri(LectureSession));
                    break;
                case "TUT":
                    sessionIsType = iri(sessionIRI).isA(iri(TutorialSession));
                    break;
                case "LAB":
                    sessionIsType = iri(sessionIRI).isA(iri(LaboratorySession));
                    break;
                default:
                    sessionIsType = null;
                    break;
            }
            TriplePattern sessionSubClassOf = iri(sessionIRI).has(iri(subClassOf), iri(Session));

            String courseIRI = classSchedulePrefix + "NTU_Course_" + NTUClassSchedule.get("ClassCode").get(entry);
            TriplePattern sessionIsPartOfCourse = iri(sessionIRI).has(iri(isPartOfCourse), iri(courseIRI));
            String dayOfWeek = NTUClassSchedule.get("DAY").get(entry);

            switch(dayOfWeek) {
                case "MON":
                    sessionHasDayOfWeek= iri(sessionIRI).has(iri(hasDayOfWeek), iri(Monday));
                    break;
                case "TUE":
                    sessionHasDayOfWeek= iri(sessionIRI).has(iri(hasDayOfWeek), iri(Tuesday));
                    break;
                case "WED":
                    sessionHasDayOfWeek= iri(sessionIRI).has(iri(hasDayOfWeek), iri(Wednesday));
                    break;
                case "THU":
                    sessionHasDayOfWeek = iri(sessionIRI).has(iri(hasDayOfWeek), iri(Thursday));
                    break;
                case "FRI":
                    sessionHasDayOfWeek = iri(sessionIRI).has(iri(hasDayOfWeek), iri(Friday));
                    break;
                case "SAT":
                    sessionHasDayOfWeek = iri(sessionIRI).has(iri(hasDayOfWeek), iri(Saturday));
                    break;
                case "SUN":
                    sessionHasDayOfWeek = iri(sessionIRI).has(iri(hasDayOfWeek), iri(Sunday));
                    break;
                default:
                    sessionHasDayOfWeek = null;
                    break;
            }

            String timeRange = NTUClassSchedule.get("TIME").get(entry);
            String[] parts = timeRange.split("-");
            String startTime = formatToXSDTime(parts[0]);
            String endTime = formatToXSDTime(parts[1]);
            TriplePattern sessionHasStartTime = iri(sessionIRI).has(iri(hasStartTime), startTime);
            TriplePattern sessionHasEndTime = iri(sessionIRI).has(iri(hasEndTime), endTime);

            String room = NTUClassSchedule.get("VENUE").get(entry);
            String roomIRI = bimPrefix + "Room_" + room;
            TriplePattern roomHasSession = iri(roomIRI).has(iri(hasSession), iri(sessionIRI));


            InsertDataQuery classInsertion = Queries.INSERT_DATA(sessionIsType, sessionSubClassOf, sessionIsPartOfCourse, sessionHasDayOfWeek, sessionHasStartTime, sessionHasEndTime, roomHasSession);
            kbClient.executeUpdate(classInsertion.getQueryString());
        }
    }

    private static String formatToXSDTime(String time) {
        // Extract hour and minute
        String hour = time.substring(0, 2);
        String minute = time.substring(2, 4);

        // Return in xsd:time format
        return hour + ":" + minute + ":00";
    }

}