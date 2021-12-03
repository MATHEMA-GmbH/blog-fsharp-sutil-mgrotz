module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Helpers

let idGenerator = makeIdGenerator ()

type Sound =
    | SingleShortBell
    | SingleLongBell
type TimerStep = {
    StepId : int
    Minutes : int
    Sound : Sound
}
let createStep minutes sound = 
    {StepId = idGenerator(); Minutes = minutes; Sound = sound;}

type Model = { 
    LastTimerValue : int  
    TimerStepsReversed : TimerStep list
    }
let getLastTimerValue m = m.LastTimerValue
let getTimerSteps m = m.TimerStepsReversed |> List.rev

type Message =
    | LastTimerValueChanged of int
    | AddSingleShortBell
    | AddSingleLongBell


let init () : Model= ({ 
    LastTimerValue = 1 
    TimerStepsReversed = []
    })

let update (msg : Message) (model : Model) : Model =
    match msg with
    | LastTimerValueChanged newValue -> { model with LastTimerValue = newValue }
    | AddSingleShortBell -> 
        let withNewStep = (createStep model.LastTimerValue SingleShortBell) :: model.TimerStepsReversed
        Browser.Dom.console.info(Fable.Core.JS.JSON.stringify withNewStep)

        { model with TimerStepsReversed = withNewStep }
    | AddSingleLongBell -> 
        let withNewStep = (createStep model.LastTimerValue SingleLongBell) :: model.TimerStepsReversed
        { model with TimerStepsReversed = withNewStep }

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

      Html.li [
        Html.button [
            type' "button"
            onClick (fun _ -> dispatch AddSingleShortBell) []
            Html.text "Add single short bell"
        ]

        Html.button [
            type' "button"
            onClick (fun _ -> dispatch AddSingleLongBell) []
            Html.text "Add single long bell"
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
