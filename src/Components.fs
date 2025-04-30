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
                    Html.h2 [
                        prop.text @"Q1. Find the names of employees in the Toy Department." 
                    ]
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
                    Html.h2 [
                        prop.text @"Q2. Find the average salary of employees in the Shoe Department. " 
                    ]
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
                    Html.h3 [
                        prop.text @"Q3. Find those items sold by departments on the second floor." 
                    ]
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
                    Html.h3 [
                        prop.text @"Q4. Find the salary of Anderson's manager. " 
                    ]
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
                    Html.h3 [
                        prop.text @"Q5. Find the names of employees who make more than their managers. " 
                    ]
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
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
            ]
        ]

    [<ReactComponent>]
    static member Square() =
        let state, dispatch = React.useElmish(AppLoader.init, AppLoader.update, [| |])
        AppLoader.render state dispatch

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Components.Square()
                | [ "hello" ] -> Components.HelloWorld()
                | [ "counter" ] -> Components.Counter()
                | otherwise -> Html.h1 "Not found"
            ]
        ]