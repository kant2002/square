namespace App

open Feliz
open Feliz.Router
open Feliz.UseElmish
open Elmish

module AppLoader =
    type Msg =
        | DoNothing
        | HoverRelationship of string
        | HoverProperty of string
        | UnHover

    type HoverState =
        | NotHovered
        | Relationship of string
        | Property of string

    type State = {
        highligted: HoverState
    }
    let init () = 
        { highligted = NotHovered }, Cmd.none

    let update msg state =
        match msg with
        | DoNothing -> 
            state, Cmd.none
        | HoverRelationship relation -> 
            { state with highligted = Relationship relation }, Cmd.none
        | HoverProperty relation -> 
            { state with highligted = Property relation }, Cmd.none
        | UnHover -> 
            { state with highligted = NotHovered }, Cmd.none

    let render state dispatch = 
        let relationship name  =
            Html.span [
                match state.highligted with
                | Relationship r ->
                    prop.onMouseOver (fun _ -> dispatch (HoverRelationship name))
                    prop.onMouseOut (fun _ -> dispatch UnHover)
                | _ -> 
                    prop.onMouseOver (fun _ -> dispatch (HoverRelationship name))
                    prop.onMouseOut (fun _ -> dispatch UnHover)
                prop.className "relationship"
                prop.style [
                    style.cursor.pointer
                    style.color (if state.highligted = Relationship name then "blue" else "black")
                    //style.fontWeight (if state.highligted = Relationship name then 700 else 400)
                    style.fontWeight 700
                ]
                prop.text name
            ] 
        let property name  =
            Html.span [
                match state.highligted with
                | Property r ->
                    prop.onMouseOver (fun _ -> dispatch (HoverRelationship name))
                    prop.onMouseOut (fun _ -> dispatch UnHover)
                | _ -> 
                    prop.onMouseOver (fun _ -> dispatch (HoverRelationship name))
                    prop.onMouseOut (fun _ -> dispatch UnHover)
                prop.className "property"
                prop.style [
                    style.cursor.pointer
                    style.color (if state.highligted = Relationship name then "blue" else "black")
                    //style.fontWeight (if state.highligted = Relationship name then 700 else 400)
                    //style.fontWeight 700
                    //style.fontStyle.italic
                ]
                prop.text name
            ] 
        let relationshipProperties name (properties: string seq) =
            Html.span [
                prop.children [
                    relationship name
                    Html.text " ( "
                    for p in properties do
                        property p
                        Html.text " "
                    Html.text ")\n"
                ]
            ]
        let propertyDescription name (desc: string) = 
            Html.li [
                prop.children [
                    property name
                    Html.text " - "
                    Html.text desc
                ]
            ]
        let section (code: int) (name: string) =
            let header = sprintf "Q%d. %s." code name
            Html.h3 [
                Html.a [
                    prop.name (sprintf "name-%d" code)
                ]
                Html.text header
            ]
        let sectionRef (code: int) (name: string) =
            let header = sprintf "Q%d. %s." code name
            Html.a [
                prop.href (sprintf "#name-%d" code)
                prop.text header
            ]
        Html.div [
            prop.className "container"
            prop.children [                    
                Html.h1 [
                    prop.className "text-center"
                    prop.text "Use power of Square to follow Codd's lead"
                ]
                Html.p [
                    prop.children [
                        Html.text "The all samples in this website will operate on the original "
                        Html.a [
                            prop.href "https://disciplinas.uvv.br/assets/disciplinas/bd1/square_1973.pdf"
                            prop.text "Square database schema"
                        ]
                        Html.text ":"
                    ]
                ]
                Html.pre [
                    //prop.className "text-center"
                    relationshipProperties "EMP" ["NAME"; "SAL"; "MGR"; "DEPT"]
                    relationshipProperties "SALES" ["DEPT"; "ITEM"; "VOL"]
                    relationshipProperties "SUPPLY" ["COMP"; "DEPT"; "ITEM"; "VOL"]
                    relationshipProperties "LOC" ["DEPT"; "FLOOR"]
                    relationshipProperties "CLASS" ["ITEM"; "TYPE"]
                ]
                Html.p [
                    Html.text @"As can be easiely seen, the schema is very simple. It has only 5 relations. 
                    The " 
                    relationship "EMP"
                    Html.text @" relation has the following attributes: NAME, SAL, MGR, DEPT. 
                    The " 
                    relationship "SALES"
                    Html.text @" relation has the following attributes: DEPT, ITEM, VOL. 
                    The " 
                    relationship "SUPPLY"
                    Html.text @" relation has the following attributes: COMP, DEPT, ITEM, VOL. 
                    The " 
                    relationship "LOC"
                    Html.text @" relation has the following attributes: DEPT, FLOOR. 
                    The " 
                    relationship "CLASS"
                    Html.text @" relation has the following attributes: ITEM, TYPE." 
                ]
                React.fragment [                    
                    Html.p [
                        prop.text @"Following properties coded: " 
                    ]
                    Html.ul [
                        prop.children [
                            propertyDescription "NAME" "The name of the employee."
                            propertyDescription "SAL" "The salary of the employee."
                            propertyDescription "MGR" "The manager of the employee."
                            propertyDescription "DEPT" "The department code."
                            propertyDescription "ITEM" "The SKU of item."
                            propertyDescription "VOL" "The volume of the item sold."
                            propertyDescription "COMP" "The company that supplies the item."
                            propertyDescription "FLOOR" "The floor of the department."
                            propertyDescription "TYPE" "The type of the item."
                        ]
                    ]
                ]
                React.fragment [                    
                    Html.p [
                        prop.text @"List of examples: " 
                    ]
                    Html.ul [
                        prop.children [
                            Html.li [ sectionRef 1 "Find the names of employees in the Toy Department" ]
                            Html.li [ sectionRef 2 "Find the average salary of employees in the Shoe Department" ]
                            Html.li [ sectionRef 3 "Find those items sold by departments on the second floor" ]
                            Html.li [ sectionRef 4 "Find the salary of Anderson's manager" ]
                            Html.li [ sectionRef 5 "Find the names of employees who make more than their managers" ]
                            Html.li [ sectionRef 6 "List the name and salary of all managers who manage more than ten employees" ]
                            Html.li [ sectionRef 7 "Find those companies, each of which supplies every item" ]
                            Html.li [ sectionRef 8 "Find the volume of guns sold by the Toy Department" ]
                            Html.li [ sectionRef 9 "List the names and managers of employees in the Shoe Department with a salary greater than 10000" ]
                            Html.li [ sectionRef 10 "Find the names of those employees who make more than any employee in the Shoe Department" ]
                        ]
                    ]
                ]
                React.fragment [  
                    section 1 "Find the names of employees in the Toy Department"
                    Html.p [
                        prop.text @"The following SQUARE query will return the names of employees in the Toy Department."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "    "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "('Toy')\n"
                            property "NAME"
                            Html.text "   "
                            property "DEPT"
                        ]
                    ]
                    Html.p [
                        prop.text @"A mapping consists of a relation name (EMP), a domain name (DEPT), a range name (NAME), and an argument 
('TOY'). The value of the mapping is the set of values in the range column of the named relation whose 
associated values in the domain column match the argument. This mapping evaluates to a unary relation (in 
this case, a list of names.) Mapping emulates the way in which people use tables. In this example, to 
find the names of employees in the Toy Department, a person might look down the DEPT column of the EMP re- 
lation, finding 'TOY' entries and making a list of the corresponding NAME entries. "
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   NAME\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    DEPT = 'Toy'\n"
                        ]
                    ]
                ]
                React.fragment [
                    section 2 "Find the average salary of employees in the Shoe Department"
                    Html.p [
                        prop.text @"The following SQUARE query will return the average salary of employees in the Shoe Department."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "AVG ("
                            Html.text "   "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "('Shoe'))\n"
                            Html.text "     "
                            property "SAL"
                            Html.text "   "
                            property "DEPT"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   AVG(SAL)\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    DEPT = 'Shoe'\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"Mappings may be ""composed"" by applying one mapping to the result of another, as illustrated by Q3. "
                    ]
                ]
                
                React.fragment [
                    section 3 "Find those items sold by departments on the second floor"
                    Html.pre [
                        prop.children [
                            Html.text "    "
                            relationship "SALES"
                            Html.text "    "
                            Html.text "o"
                            Html.text "    "
                            relationship "LOC"
                            Html.text "     "
                            Html.text "('2'))\n"
                            property "ITEM"
                            Html.text "     "
                            property "DEPT"
                            Html.text " "
                            property "DEPT"
                            Html.text "   "
                            property "FLOOR"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   ITEM\n"
                            Html.text "FROM     SALES\n"
                            Html.text "WHERE    DEPT = (\n"
                            Html.text "                 SELECT  DEPT\n"
                            Html.text "                 FROM    LOC\n"
                            Html.text "                 WHERE   FLOOR = '2')\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"The floor '2' is first mapped to the departments located there, and then to the items which they sell. 
The range of the inner mapping must be compatible with the domain of the outer mapping, but they need 
not be identical, as illustrated by Q4."
                    ]
                ]

                React.fragment [
                    section 4 "Find the salary of Anderson's manager"
                    Html.pre [
                        prop.children [
                            Html.text "   "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "o"
                            Html.text "   "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "('ANDERSON'))\n"
                            property "SAL"
                            Html.text "   "
                            property "NAME"
                            Html.text " "
                            property "MGR"
                            Html.text "   "
                            property "NAME"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   SAL\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    NAME = (\n"
                            Html.text "                 SELECT  MGR\n"
                            Html.text "                 FROM    EMP\n"
                            Html.text "                 WHERE   NAME = 'ANDERSON')\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"Q3 is repeated in Section Ill in order to demonstrate the different perception of the query that is re- 
quired in order to answer the query in a predicate calculus-like language."
                    ]
                    Html.p [
                        prop.text @"The next important building block of relational expressions is called a free variable. A relational 
expression containing a free variable takes the following form: "
                    ]
                    Html.p [
                        prop.style [
                            style.fontStyle.italic
                        ]
                        prop.text @"free-variable-list : test"
                    ]
                    Html.p [
                        prop.text @"On the left side of the colon are listed the free variables to be used in the query and the relations to 
which they belong. Each free variable represents a row of a relation. Free variables may be given arbitrary 
names provided they do not conflict with the names of relations. On the right side of the colon 
is a logical test which may be true or false for each set of values of the free variables. The value of 
the expression is the set of free-variable values for which the test is true. A subscripted free variable 
represents a particular field-value from the row represented by the free variable. For example: "
                    ]
                ]

                React.fragment [
                    section 5 "Find the names of employees who make more than their managers"
                    Html.pre [
                        prop.children [
                            Html.text "x"
                            Html.text "    "
                            Html.text "ε "
                            relationship "EMP"
                            Html.text " : x"
                            Html.text "    "
                            Html.text ">"
                            Html.text "   "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "(x    ))\n"
                            Html.text " "
                            property "SAL"
                            Html.text "          "
                            property "NAME"
                            Html.text " "
                            property "MGR"
                            Html.text "   "
                            property "NAME"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   SAL\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    NAME > (\n"
                            Html.text "                 SELECT  MGR\n"
                            Html.text "                 FROM    EMP)\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"The following types of operators are permissible in tests: "
                    ]
                    Html.ul [
                        prop.children [
                            Html.li "Numeric comparisons: =, ≠, <, >, ≥, ≤"
                            Html.li "Set comparisons: =, ≠, ⊆, ⊂, ⊇, ⊃"
                            Html.li "Arithmetic operators: +, -, *, /"
                            Html.li "Set operators: ∪ ∩"
                            Html.li "Logical connectivity: ∧ ∨ -"
                            Html.li "Set intersection operator: ∩"
                            Html.li "parentheses for grouping: ( )"
                            Html.li "built-in functions: SUM, COUNT, AVG, MAX, MIN, etc. "
                        ]
                    ]
                    Html.p [
                        prop.text @"The following example constructs a binary relation: "
                    ]
                ]

                React.fragment [
                    section 6 "List the name and salary of all managers who manage more than ten employees"
                    Html.pre [
                        prop.children [
                            Html.text "x"
                            Html.text "          "
                            Html.text "ε "
                            relationship "EMP"
                            Html.text " : COUNT ("
                            Html.text "    "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "(x    )) > 10\n"
                            Html.text " "
                            property "NAME, SAL"
                            Html.text "                "
                            property "NAME"
                            Html.text "   "
                            property "MGR"
                            Html.text "   "
                            property "NAME"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   x.NAME, x.SAL\n"
                            Html.text "FROM     EMP x\n"
                            Html.text "WHERE    x.NAME = (\n"
                            Html.text "                 SELECT  MGR\n"
                            Html.text "                 FROM    EMP)\n"
                            Html.text "GROUP BY x.NAME, x.SAL\n"
                            Html.text "HAVING COUNT(*) > 10\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"The free variable is introduced into queries where it becomes necessary to correlate information per- 
taining to a specific row in a table with another row or set of rows from some table. Consequently, this 
variable is introduced only for queries that are more complex than simple selection. As can be seen in 
Section III, all queries regardless of complexity require free variables in predicate calculus based 
languages. "
                    ]
                    Html.p [
                        prop.text @"Another important concept is that of projection. If a relation-name appears subscripted by one or 
more column-names, it represents the set of unique tuples of values occurring in those columns of the 
relation. For example, 
`ITEM SUPPLY` is the set of all item-values in the SUPPLY relation. This feature is useful in constructing expressions like the following: "
                    ]
                ]

                React.fragment [
                    section 7 "Find those companies, each of which supplies every item"
                    Html.pre [
                        prop.children [
                            Html.text "x"
                            Html.text "          "
                            Html.text "ε "
                            relationship "SUPPLY"
                            Html.text " : "
                            Html.text "   "
                            relationship "SUPPLY"
                            Html.text "    "
                            Html.text "(x    )) =     "
                            relationship "SUPPLY"
                            Html.text "\n"
                            Html.text " "
                            property "COMP"
                            Html.text "                "
                            property "ITEM"
                            Html.text "      "
                            property "COMP"
                            Html.text "  "
                            property "NAME"
                            Html.text "     "
                            property "ITEM"
                        ]
                    ]
                    Html.p [
                        prop.text @"Cannot build equivalent in SQL. Feel free to contribute."
                    ]
                    Html.p [
                        prop.text @"We will now discuss some extensions to the concept of mapping. 
A mapping may specify more than one 
domain field in which case each domain field must be compatible with its reSpective argument. If an argument
is a set then the value of the domain field must match some element of the set. This facility is 
useful in dealing with n-ary associations. For example: "
                    ]
                ]

                React.fragment [
                    section 8 "Find the volume of guns sold by the Toy Department"
                    Html.pre [
                        prop.children [
                            Html.text "    "
                            relationship "SALES"
                            Html.text "          "
                            Html.text "('Toy', 'Gun')\n"
                            property "VOL"
                            Html.text "     "
                            property "DEPT, ITEM"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   VOL\n"
                            Html.text "FROM     SALES\n"
                            Html.text "WHERE    DEPT = 'TOY'\n"
                            Html.text " AND     ITEM = 'GUN'\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"Similarly, a mapping may specify more than one range field, in which case it returns tuples of values from 
the fields specified. "
                    ]
                    Html.p [
                        prop.text @"When one of the numeric comparison operators ≠, <, >, ≥, ≤, is used as a prefix to the argument of a 
mapping, the argument effectively becomes the set of all values which compare by the given operator with 
the given argument. This type of mapping often avoids the use of a free variable, as illustrated in Q9."
                    ]
                ]

                React.fragment [
                    section 9 "List the names and managers of employees in the Shoe Department with a salary greater than 10000"
                    Html.pre [
                        prop.children [
                            Html.text "         "
                            relationship "EMP"
                            Html.text "         "
                            Html.text "('SHOE', > 10000)\n"
                            property "NAME, MGR"
                            Html.text "   "
                            property "DEPT, SAL"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   NAME, MGR\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    DEPT = 'SHOE'\n"
                            Html.text " AND     SAL > 10000\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"The numeric comparison operators <, >, ≥, ≤, may also be extended so that a number may be compared to a set. 
This is done by placing the word SOME or ALL on the side(s) of the comparison operator which is a 
set. For example, `X > ALL Y` is true if the number X is greater than all elements of the set Y, and 
`Y ALL < SOME Z` is true if all elements of Y are less than some element of Z. This facility is useful in 
queries like the following: "
                    ]
                ]

                React.fragment [
                    section 10 "Find the names of those employees who make more than any employee in the Shoe Department"
                    Html.pre [
                        prop.children [
                            Html.text "x"
                            Html.text "    "
                            Html.text "ε "
                            relationship "EMP"
                            Html.text " : x"
                            Html.text "   "
                            Html.text "> ALL"
                            Html.text "   "
                            relationship "EMP"
                            Html.text "    "
                            Html.text "('SHOE')\n"
                            Html.text " "
                            property "NAME"
                            Html.text "         "
                            property "SAL"
                            Html.text "     "
                            property "SAL"
                            Html.text "   "
                            property "DEPT"
                        ]
                    ]
                    Html.p [
                        prop.text @"Equivalent in SQL."
                    ]
                    Html.pre [
                        prop.children [
                            Html.text "SELECT   NAME\n"
                            Html.text "FROM     EMP\n"
                            Html.text "WHERE    SAL >= (\n"
                            Html.text "                 SELECT  MAX(SAL)\n"
                            Html.text "                 FROM    EMP\n"
                            Html.text "                 WHERE   DEPT = 'SHOE')\n"
                        ]
                    ]
                    Html.p [
                        prop.text @"In understanding Ql0, it is important to remember that the free variable x represents a row of the EMP 
relation. If the test (which uses the SAL value of the row) is true, the NAME value of the row is 
returned. All rows of the relation are tested in this way, and duplicate values are eliminated from 
the returned set."
                    ]
                    Html.p [
                        prop.text @"It should be noted that the functions of ALL and SOME could be accomplished equally well by the built- 
in functions MAX and MIN. In fact, definitions of the modifiers ALL and SOME are given by the following 
table, which specifies how any modifier may be replaced by a built-in function."
                    ]
                ]

                Html.hr []
                Html.p [
                    Html.text "Written in Feliz and F#. Source code on "
                    Html.a [
                        prop.href "https://github.com/kant2002/square"
                        prop.text "GitHub"
                    ]
                ]
            ]
        ]

type Components =
    [<ReactComponent>]
    static member Square() =
        let state, dispatch = React.useElmish(AppLoader.init, AppLoader.update, [| |])
        AppLoader.render state dispatch
