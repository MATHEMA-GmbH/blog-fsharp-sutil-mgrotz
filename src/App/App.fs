module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Helpers

let idGenerator = makeIdGenerator ()
let ticksPerMinute = System.TimeSpan.TicksPerMinute


type Model = { Counter : int }


type Message =
    | NoOp

let init () : Model= ({ Counter = 0 })

let update (msg : Message) (model : Model) : Model =
    match msg with
    | NoOp -> model

let view() =
    let model, dispatch = () |> Store.makeElmishSimple init update ignore

    Html.div [
        disposeOnUnmount [ model ]

        Html.div [
            Html.text "It works."
        ]
    ]

view() |> Program.mountElement "sutil-app"
