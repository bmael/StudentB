Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Manager))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Manager))==(Machine(Manager));
  Level(Machine(Manager))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Manager)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Manager))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Manager))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Manager))==(?);
  List_Includes(Machine(Manager))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Manager))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Manager))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Manager))==(?);
  Context_List_Variables(Machine(Manager))==(?);
  Abstract_List_Variables(Machine(Manager))==(?);
  Local_List_Variables(Machine(Manager))==(projectOfStudent,moduleOfProject,projects,modules,moduleOfProjectOfStudent,moduleOfStudent,students);
  List_Variables(Machine(Manager))==(projectOfStudent,moduleOfProject,projects,modules,moduleOfProjectOfStudent,moduleOfStudent,students);
  External_List_Variables(Machine(Manager))==(projectOfStudent,moduleOfProject,projects,modules,moduleOfProjectOfStudent,moduleOfStudent,students)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Manager))==(?);
  Abstract_List_VisibleVariables(Machine(Manager))==(?);
  External_List_VisibleVariables(Machine(Manager))==(?);
  Expanded_List_VisibleVariables(Machine(Manager))==(?);
  List_VisibleVariables(Machine(Manager))==(?);
  Internal_List_VisibleVariables(Machine(Manager))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Manager))==(btrue);
  Gluing_List_Invariant(Machine(Manager))==(btrue);
  Expanded_List_Invariant(Machine(Manager))==(btrue);
  Abstract_List_Invariant(Machine(Manager))==(btrue);
  Context_List_Invariant(Machine(Manager))==(btrue);
  List_Invariant(Machine(Manager))==(students <: STUDENTS & modules <: MODULES & projects <: PROJECTS & moduleOfStudent: students <-> modules & moduleOfProject: projects --> modules & projectOfStudent: students <-> projects & moduleOfProjectOfStudent: students +-> moduleOfProject)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Manager))==(btrue);
  Abstract_List_Assertions(Machine(Manager))==(btrue);
  Context_List_Assertions(Machine(Manager))==(btrue);
  List_Assertions(Machine(Manager))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Manager))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Manager))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Manager))==(students,modules,projects,moduleOfStudent,moduleOfProject,projectOfStudent,moduleOfProjectOfStudent:={},{},{},{},{},{},{});
  Context_List_Initialisation(Machine(Manager))==(skip);
  List_Initialisation(Machine(Manager))==(students:={} || modules:={} || projects:={} || moduleOfStudent:={} || moduleOfProject:={} || projectOfStudent:={} || moduleOfProjectOfStudent:={})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Manager))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Manager))==(btrue);
  List_Constraints(Machine(Manager))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Manager))==(subscribe,unsubscribe,subscribeToModule,unsubscribeOfModule,subscribeToProject,unsubscribeOfProject,createModule,createProject,getGroups,getProjects);
  List_Operations(Machine(Manager))==(subscribe,unsubscribe,subscribeToModule,unsubscribeOfModule,subscribeToProject,unsubscribeOfProject,createModule,createProject,getGroups,getProjects)
END
&
THEORY ListInputX IS
  List_Input(Machine(Manager),subscribe)==(ss);
  List_Input(Machine(Manager),unsubscribe)==(ss);
  List_Input(Machine(Manager),subscribeToModule)==(ss,mm);
  List_Input(Machine(Manager),unsubscribeOfModule)==(ss,mm);
  List_Input(Machine(Manager),subscribeToProject)==(ss,pp);
  List_Input(Machine(Manager),unsubscribeOfProject)==(ss,pp);
  List_Input(Machine(Manager),createModule)==(mm);
  List_Input(Machine(Manager),createProject)==(pp,mm);
  List_Input(Machine(Manager),getGroups)==(ss);
  List_Input(Machine(Manager),getProjects)==(ss)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Manager),subscribe)==(?);
  List_Output(Machine(Manager),unsubscribe)==(?);
  List_Output(Machine(Manager),subscribeToModule)==(?);
  List_Output(Machine(Manager),unsubscribeOfModule)==(?);
  List_Output(Machine(Manager),subscribeToProject)==(?);
  List_Output(Machine(Manager),unsubscribeOfProject)==(?);
  List_Output(Machine(Manager),createModule)==(?);
  List_Output(Machine(Manager),createProject)==(?);
  List_Output(Machine(Manager),getGroups)==(res);
  List_Output(Machine(Manager),getProjects)==(seti,nb)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Manager),subscribe)==(subscribe(ss));
  List_Header(Machine(Manager),unsubscribe)==(unsubscribe(ss));
  List_Header(Machine(Manager),subscribeToModule)==(subscribeToModule(ss,mm));
  List_Header(Machine(Manager),unsubscribeOfModule)==(unsubscribeOfModule(ss,mm));
  List_Header(Machine(Manager),subscribeToProject)==(subscribeToProject(ss,pp));
  List_Header(Machine(Manager),unsubscribeOfProject)==(unsubscribeOfProject(ss,pp));
  List_Header(Machine(Manager),createModule)==(createModule(mm));
  List_Header(Machine(Manager),createProject)==(createProject(pp,mm));
  List_Header(Machine(Manager),getGroups)==(res <-- getGroups(ss));
  List_Header(Machine(Manager),getProjects)==(seti,nb <-- getProjects(ss))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Manager),subscribe)==(ss: STUDENTS & ss/:students);
  List_Precondition(Machine(Manager),unsubscribe)==(ss: STUDENTS & ss: students & ss: dom(moduleOfStudent) & ss: dom(projectOfStudent) & ss: dom(moduleOfProjectOfStudent));
  List_Precondition(Machine(Manager),subscribeToModule)==(ss: STUDENTS & ss: students & mm: MODULES & mm: modules & ss/:dom(moduleOfStudent));
  List_Precondition(Machine(Manager),unsubscribeOfModule)==(ss: STUDENTS & ss: students & mm: MODULES & mm: modules & ss: dom(moduleOfStudent) & mm: ran(moduleOfStudent));
  List_Precondition(Machine(Manager),subscribeToProject)==(ss: STUDENTS & ss: students & pp: PROJECTS & pp: projects & ss/:dom(projectOfStudent));
  List_Precondition(Machine(Manager),unsubscribeOfProject)==(ss: STUDENTS & ss: students & pp: PROJECTS & pp: projects & ss: dom(projectOfStudent));
  List_Precondition(Machine(Manager),createModule)==(mm: MODULES & mm/:modules);
  List_Precondition(Machine(Manager),createProject)==(pp: PROJECTS & pp/:projects & mm: MODULES & mm: modules);
  List_Precondition(Machine(Manager),getGroups)==(ss: STUDENTS & ss: students);
  List_Precondition(Machine(Manager),getProjects)==(ss: STUDENTS & ss: students)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Manager),getProjects)==(ss: STUDENTS & ss: students | seti,nb:=projectOfStudent[{ss}],card(projectOfStudent[{ss}]));
  Expanded_List_Substitution(Machine(Manager),getGroups)==(ss: STUDENTS & ss: students | @sgss.(sgss <: FIN(STUDENTS) & sgss = {sgi | sgi <: STUDENTS & #pi.(pi: PROJECTS & pi: projectOfStudent[{ss}] & sgi = projectOfStudent~[{pi}])} ==> res:=sgss));
  Expanded_List_Substitution(Machine(Manager),createProject)==(pp: PROJECTS & pp/:projects & mm: MODULES & mm: modules | projects,moduleOfProject:=projects\/{pp},moduleOfProject<+{pp|->mm});
  Expanded_List_Substitution(Machine(Manager),createModule)==(mm: MODULES & mm/:modules | modules:=modules\/{mm});
  Expanded_List_Substitution(Machine(Manager),unsubscribeOfProject)==(ss: STUDENTS & ss: students & pp: PROJECTS & pp: projects & ss: dom(projectOfStudent) | projectOfStudent:=projectOfStudent-{ss|->pp});
  Expanded_List_Substitution(Machine(Manager),subscribeToProject)==(ss: STUDENTS & ss: students & pp: PROJECTS & pp: projects & ss/:dom(projectOfStudent) | projectOfStudent:=projectOfStudent<+{ss|->pp});
  Expanded_List_Substitution(Machine(Manager),unsubscribeOfModule)==(ss: STUDENTS & ss: students & mm: MODULES & mm: modules & ss: dom(moduleOfStudent) & mm: ran(moduleOfStudent) | moduleOfStudent,projectOfStudent:=moduleOfStudent-{ss|->mm},{ss}<<|projectOfStudent);
  Expanded_List_Substitution(Machine(Manager),subscribeToModule)==(ss: STUDENTS & ss: students & mm: MODULES & mm: modules & ss/:dom(moduleOfStudent) | moduleOfStudent:=moduleOfStudent<+{ss|->mm});
  Expanded_List_Substitution(Machine(Manager),unsubscribe)==(ss: STUDENTS & ss: students & ss: dom(moduleOfStudent) & ss: dom(projectOfStudent) & ss: dom(moduleOfProjectOfStudent) | students,moduleOfStudent,projectOfStudent,moduleOfProjectOfStudent:=students-{ss},{ss}<<|moduleOfStudent,{ss}<<|projectOfStudent,{ss}<<|moduleOfProjectOfStudent);
  Expanded_List_Substitution(Machine(Manager),subscribe)==(ss: STUDENTS & ss/:students | students:=students\/{ss});
  List_Substitution(Machine(Manager),subscribe)==(students:=students\/{ss});
  List_Substitution(Machine(Manager),unsubscribe)==(students:=students-{ss} || moduleOfStudent:={ss}<<|moduleOfStudent || projectOfStudent:={ss}<<|projectOfStudent || moduleOfProjectOfStudent:={ss}<<|moduleOfProjectOfStudent);
  List_Substitution(Machine(Manager),subscribeToModule)==(moduleOfStudent(ss):=mm);
  List_Substitution(Machine(Manager),unsubscribeOfModule)==(moduleOfStudent:=moduleOfStudent-{ss|->mm} || projectOfStudent:={ss}<<|projectOfStudent);
  List_Substitution(Machine(Manager),subscribeToProject)==(projectOfStudent(ss):=pp);
  List_Substitution(Machine(Manager),unsubscribeOfProject)==(projectOfStudent:=projectOfStudent-{ss|->pp});
  List_Substitution(Machine(Manager),createModule)==(modules:=modules\/{mm});
  List_Substitution(Machine(Manager),createProject)==(projects:=projects\/{pp} || moduleOfProject(pp):=mm);
  List_Substitution(Machine(Manager),getGroups)==(ANY sgss WHERE sgss <: FIN(STUDENTS) & sgss = {sgi | sgi <: STUDENTS & #pi.(pi: PROJECTS & pi: projectOfStudent[{ss}] & sgi = projectOfStudent~[{pi}])} THEN res:=sgss END);
  List_Substitution(Machine(Manager),getProjects)==(seti:=projectOfStudent[{ss}] || nb:=card(projectOfStudent[{ss}]))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Manager))==(?);
  Inherited_List_Constants(Machine(Manager))==(?);
  List_Constants(Machine(Manager))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Manager),STUDENTS)==(?);
  Context_List_Enumerated(Machine(Manager))==(?);
  Context_List_Defered(Machine(Manager))==(?);
  Context_List_Sets(Machine(Manager))==(?);
  List_Valuable_Sets(Machine(Manager))==(STUDENTS,MODULES,PROJECTS);
  Inherited_List_Enumerated(Machine(Manager))==(?);
  Inherited_List_Defered(Machine(Manager))==(?);
  Inherited_List_Sets(Machine(Manager))==(?);
  List_Enumerated(Machine(Manager))==(?);
  List_Defered(Machine(Manager))==(STUDENTS,MODULES,PROJECTS);
  List_Sets(Machine(Manager))==(STUDENTS,MODULES,PROJECTS);
  Set_Definition(Machine(Manager),MODULES)==(?);
  Set_Definition(Machine(Manager),PROJECTS)==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Manager))==(?);
  Expanded_List_HiddenConstants(Machine(Manager))==(?);
  List_HiddenConstants(Machine(Manager))==(?);
  External_List_HiddenConstants(Machine(Manager))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Manager))==(btrue);
  Context_List_Properties(Machine(Manager))==(btrue);
  Inherited_List_Properties(Machine(Manager))==(btrue);
  List_Properties(Machine(Manager))==(STUDENTS: FIN(INTEGER) & not(STUDENTS = {}) & MODULES: FIN(INTEGER) & not(MODULES = {}) & PROJECTS: FIN(INTEGER) & not(PROJECTS = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Manager),subscribe)==(?);
  List_ANY_Var(Machine(Manager),unsubscribe)==(?);
  List_ANY_Var(Machine(Manager),subscribeToModule)==(?);
  List_ANY_Var(Machine(Manager),unsubscribeOfModule)==(?);
  List_ANY_Var(Machine(Manager),subscribeToProject)==(?);
  List_ANY_Var(Machine(Manager),unsubscribeOfProject)==(?);
  List_ANY_Var(Machine(Manager),createModule)==(?);
  List_ANY_Var(Machine(Manager),createProject)==(?);
  List_ANY_Var(Machine(Manager),getGroups)==(Var(sgss) == SetOf(SetOf(atype(STUDENTS,?,?))));
  List_ANY_Var(Machine(Manager),getProjects)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Manager)) == (STUDENTS,MODULES,PROJECTS | ? | projectOfStudent,moduleOfProject,projects,modules,moduleOfProjectOfStudent,moduleOfStudent,students | ? | subscribe,unsubscribe,subscribeToModule,unsubscribeOfModule,subscribeToProject,unsubscribeOfProject,createModule,createProject,getGroups,getProjects | ? | ? | ? | Manager);
  List_Of_HiddenCst_Ids(Machine(Manager)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Manager)) == (?);
  List_Of_VisibleVar_Ids(Machine(Manager)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Manager)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Manager)) == (Type(STUDENTS) == Cst(SetOf(atype(STUDENTS,"[STUDENTS","]STUDENTS")));Type(MODULES) == Cst(SetOf(atype(MODULES,"[MODULES","]MODULES")));Type(PROJECTS) == Cst(SetOf(atype(PROJECTS,"[PROJECTS","]PROJECTS"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Manager)) == (Type(projectOfStudent) == Mvl(SetOf(atype(STUDENTS,?,?)*atype(PROJECTS,?,?)));Type(moduleOfProject) == Mvl(SetOf(atype(PROJECTS,?,?)*atype(MODULES,?,?)));Type(projects) == Mvl(SetOf(atype(PROJECTS,?,?)));Type(modules) == Mvl(SetOf(atype(MODULES,?,?)));Type(moduleOfProjectOfStudent) == Mvl(SetOf(atype(STUDENTS,?,?)*(atype(PROJECTS,?,?)*atype(MODULES,?,?))));Type(moduleOfStudent) == Mvl(SetOf(atype(STUDENTS,?,?)*atype(MODULES,?,?)));Type(students) == Mvl(SetOf(atype(STUDENTS,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Manager)) == (Type(getProjects) == Cst(SetOf(atype(PROJECTS,?,?))*btype(INTEGER,?,?),atype(STUDENTS,?,?));Type(getGroups) == Cst(SetOf(SetOf(atype(STUDENTS,?,?))),atype(STUDENTS,?,?));Type(createProject) == Cst(No_type,atype(PROJECTS,?,?)*atype(MODULES,?,?));Type(createModule) == Cst(No_type,atype(MODULES,?,?));Type(unsubscribeOfProject) == Cst(No_type,atype(STUDENTS,?,?)*atype(PROJECTS,?,?));Type(subscribeToProject) == Cst(No_type,atype(STUDENTS,?,?)*atype(PROJECTS,?,?));Type(unsubscribeOfModule) == Cst(No_type,atype(STUDENTS,?,?)*atype(MODULES,?,?));Type(subscribeToModule) == Cst(No_type,atype(STUDENTS,?,?)*atype(MODULES,?,?));Type(unsubscribe) == Cst(No_type,atype(STUDENTS,?,?));Type(subscribe) == Cst(No_type,atype(STUDENTS,?,?)));
  Observers(Machine(Manager)) == (Type(getProjects) == Cst(SetOf(atype(PROJECTS,?,?))*btype(INTEGER,?,?),atype(STUDENTS,?,?));Type(getGroups) == Cst(SetOf(SetOf(atype(STUDENTS,?,?))),atype(STUDENTS,?,?)))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == VALIDATION_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
