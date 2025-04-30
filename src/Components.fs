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
                Html.p [
                    prop.innerHtml @"Following properties coded: " 
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