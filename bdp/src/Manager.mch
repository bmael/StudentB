/* Manager
 * Author: Maël
 * Creation date: 27/11/2013
 */
MACHINE
    Manager

SETS
    STUDENTS
    ; MODULES
    ; PROJECTS

ABSTRACT_VARIABLES
    students
    , moduleOfStudent
    , moduleOfProjectOfStudent
    , modules
    , projects
    , moduleOfProject
    , projectOfStudent

INVARIANT
    students <: STUDENTS
    & modules <: MODULES
    & projects <: PROJECTS
    & moduleOfStudent : students <-> modules
    & moduleOfProject : projects --> modules
    & projectOfStudent : students <-> projects
    // & card(f(students)) = card(g(f(students))) with f function binding students to their projects and g function binding project to their module

    & moduleOfProjectOfStudent : students +-> moduleOfProject
//    & moduleOfProjectOfStudent : students -->> (projects -->> modules) 
//    & moduleOfProject = ran(moduleOfProjectOfStudent) 

INITIALISATION
   students := {}
   || modules := {}
   || projects := {}
   || moduleOfStudent := {}
   || moduleOfProject := {}
   || projectOfStudent := {}
   || moduleOfProjectOfStudent := {}

OPERATIONS
    // Subscribe a student to the formation
    subscribe ( ss ) =
    PRE
        ss : STUDENTS
        & ss /: students
    THEN
        students := students \/ { ss }
    END
    ;

    // Unsubscribe a student of a formation
    unsubscribe ( ss ) =
    PRE
        ss : STUDENTS
        & ss : students
        & ss : dom ( moduleOfStudent )
        & ss : dom ( projectOfStudent )
        & ss : dom ( moduleOfProjectOfStudent )
    THEN
        students := students - { ss }
        || moduleOfStudent := { ss } <<| moduleOfStudent
        || projectOfStudent := { ss } <<| projectOfStudent
        || moduleOfProjectOfStudent := { ss } <<| moduleOfProjectOfStudent
    END
    ;

    // Subscribe a student to a module
    subscribeToModule ( ss , mm ) =
    PRE
        ss : STUDENTS
        & ss : students
        & mm : MODULES
        & mm : modules
        & ss /: dom ( moduleOfStudent )
    THEN
        moduleOfStudent ( ss ) := mm
    END
    ;

    // Unsubscribe a student of a module
    unsubscribeOfModule ( ss , mm ) =
    PRE
        ss : STUDENTS
        & ss : students
        & mm : MODULES
        & mm : modules
        & ss : dom ( moduleOfStudent )
        & mm : ran ( moduleOfStudent )
    THEN
        moduleOfStudent := moduleOfStudent - { ( ss |-> mm ) }
        || projectOfStudent := { ss } <<| projectOfStudent
    END
    ;

    // Subscribe a student to a project
    subscribeToProject ( ss , pp ) =
    PRE
        ss : STUDENTS
        & ss : students
        & pp : PROJECTS
        & pp : projects
        & ss /: dom ( projectOfStudent )
    THEN
        projectOfStudent ( ss ) := pp
    END
    ;

    // Unsubscribe a student of a project
    unsubscribeOfProject ( ss , pp ) =
    PRE
        ss : STUDENTS
        & ss : students
        & pp : PROJECTS
        & pp : projects
        & ss : dom ( projectOfStudent )
    THEN
        projectOfStudent := projectOfStudent - { ( ss |-> pp ) }
    END
    ;

    //Create a module
    createModule ( mm ) =
    PRE
        mm : MODULES
        & mm /: modules
    THEN
        modules := modules \/ { mm }
    END
    ;

    //Create a project
    createProject ( pp , mm ) =
    PRE
        pp : PROJECTS
        & pp /: projects
        & mm : MODULES
        & mm : modules
    THEN
        projects := projects \/ { pp }
        || moduleOfProject ( pp ) := mm
    END
    ;

    // Get the student's groups
    res <-- getGroups ( ss ) =
    PRE
        ss : STUDENTS
        & ss : students
    THEN
         ANY
            sgss
        WHERE
            sgss <: FIN ( STUDENTS )
            & sgss = { sgi | sgi <: STUDENTS & # pi . ( pi : PROJECTS & pi : projectOfStudent [ { ss } ] & sgi = projectOfStudent ~ [ { pi } ] ) }
        THEN
            res := sgss
        END
    END
    ;

    // Get the student's project set and count number of projects.
    seti , nb <-- getProjects ( ss ) =
    PRE
        ss : STUDENTS
        & ss : students
    THEN
        seti := projectOfStudent [ { ss } ]
        || nb := card ( projectOfStudent [ { ss } ] )
    END
   /* ;
    
   set <-- haveMoreThanNumProjects(num) =
    PRE
        num : INTEGER
        & num > 0
    THEN
         set := {ss | ss:STUDENTS & card(getGroups[{ss}]) > num}
    END*/

END
