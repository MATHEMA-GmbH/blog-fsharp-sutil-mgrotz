module App

open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Helpers

let idGenerator = makeIdGenerator ()

type Sound =
    | SingleShortBell
    | SingleLongBell
let soundToString sound =
    match sound with
    | SingleShortBell -> "A single short bell chime"
    | SingleLongBell -> "A single long bell chime"
let soundToFile sound =
    match sound with
    | SingleShortBell -> "powerUp7.wav"
    | SingleLongBell -> "FileLoad.wav"

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
        { model with TimerStepsReversed = withNewStep }
    | AddSingleLongBell -> 
        let withNewStep = (createStep model.LastTimerValue SingleLongBell) :: model.TimerStepsReversed
        { model with TimerStepsReversed = withNewStep }

let meditationPlanView (model: IStore<Model>) dispatch =
    fragment 
        [
            Html.h2 "Meditation session plan"
            Bind.el (model |> Store.map getTimerSteps, fun steps ->
            if List.isEmpty steps then
                Html.h3 "No steps planned yet"
            else
            Html.ul 
                [
                Bind.each ((model |> Store.map getTimerSteps), (fun step ->
                    Html.li [
                        Html.span $"{soundToString step.Sound} after {step.Minutes} minute(s)"
                    ] )
                    )
                ]
            )
        ]

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
            meditationPlanView model dispatch
        ]
    ]

view() |> Program.mountElement "sutil-app"
