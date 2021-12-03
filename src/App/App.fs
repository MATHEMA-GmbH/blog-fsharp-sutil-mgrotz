module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Helpers

let idGenerator = makeIdGenerator ()
let ticksPerMinute = System.TimeSpan.TicksPerMinute


type Model = { LastTimerValue : int  }
let getLastTimerValue m = m.LastTimerValue

type Message =
    | LastTimerValueChanged of int


let init () : Model= ({ LastTimerValue = 0 })

let update (msg : Message) (model : Model) : Model =
    match msg with
    | LastTimerValueChanged newValue -> { model with LastTimerValue = newValue }

let planEditView (model: IStore<Model>) dispatch = 
    Html.ul [
      Html.li [
          Html.label [Html.text "How many minutes should the next timer last?"]
          Html.input [
                          type' "number"
                          Attr.value (model |> Store.map getLastTimerValue |> Store.distinct, LastTimerValueChanged >> dispatch)
                          Attr.min 1
                          Attr.max 15
                          Attr.placeholder "How many minutes?"
                      ]
      ]
  ]    

let view() =
    // create the application with The Elm Architecture
    let model, dispatch = () |> Store.makeElmishSimple init update ignore

    Html.div [
        Html.div [
            planEditView model dispatch
        ]
    ]

view() |> Program.mountElement "sutil-app"
