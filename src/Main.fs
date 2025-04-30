module Main

open Feliz
open App
open Browser.Dom

let root = ReactDOM.createRoot(document.getElementById "square-app")
root.render(Components.Router())