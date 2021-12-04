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

type Ticks = Ticks of int64
let formatElapsedTime (Ticks elapsedTime) : string =
    System.TimeSpan(elapsedTime).ToString()

type StepWithEndTime = {
    Step: TimerStep
    EndTime: Ticks
}

type PlayPlan = {
    CurrentStep : StepWithEndTime
    RemainingSteps : StepWithEndTime list
}

type PlayPlanCalculation = {
    PreviousStepEnd : Ticks
    CalculatedSteps : StepWithEndTime list
}

type Model = { 
    LastTimerValue : int  
    TimerStepsReversed : TimerStep list
    Running : bool
    StartTime : Ticks
    ElapsedTimeSinceStart : Ticks
    PlayPlan : PlayPlan option
    }
let getLastTimerValue m = m.LastTimerValue
let getTimerSteps m = m.TimerStepsReversed |> List.rev
let getRunning m = m.Running
let getElapsedTime m = m.ElapsedTimeSinceStart
let getPlayPlan m = m.PlayPlan

type Message =
    | LastTimerValueChanged of int
    | AddSingleShortBell
    | AddSingleLongBell
    | StartSession
    | TimerTick


let init () : Model * Cmd<Message> = ({ 
    LastTimerValue = 1 
    TimerStepsReversed = []
    StartTime = Ticks 0L
    ElapsedTimeSinceStart = Ticks 0L
    PlayPlan = None
    Running = false
    }, Cmd.none)

let calculateNextTimerStep (timerStep : TimerStep) (state : PlayPlanCalculation) : PlayPlanCalculation =
    // let minutesInTicks = (int64)timerStep.Minutes * System.TimeSpan.TicksPerMinute
    let minutesInTicks = (int64)timerStep.Minutes * System.TimeSpan.TicksPerMinute / 10L // for DEBUG

    // unwrap int64 value
    let (Ticks previousStepEnd) = state.PreviousStepEnd

    // calculate end and wrap again in Ticks type
    let stepEndsAt =  previousStepEnd + minutesInTicks |> Ticks

    // update the state we thread through each iteration
    let withEndTime = {Step = timerStep; EndTime =  stepEndsAt}
    { state with PreviousStepEnd = stepEndsAt; CalculatedSteps = withEndTime :: state.CalculatedSteps}

let update (getNow : unit -> Ticks) (msg : Message) (model : Model) : (Model * Cmd<Message>) =
    match msg with
    | LastTimerValueChanged newValue -> ({ model with LastTimerValue = newValue }, Cmd.none)
    | AddSingleShortBell -> 
        let withNewStep = (createStep model.LastTimerValue SingleShortBell) :: model.TimerStepsReversed
        ({ model with TimerStepsReversed = withNewStep }, Cmd.none)
    | AddSingleLongBell -> 
        let withNewStep = (createStep model.LastTimerValue SingleLongBell) :: model.TimerStepsReversed
        ({ model with TimerStepsReversed = withNewStep }, Cmd.none)
    | StartSession ->
        let now = getNow ()

        let calculatedSteps = 
                (List.foldBack  calculateNextTimerStep
                                model.TimerStepsReversed 
                                { PreviousStepEnd = now; CalculatedSteps = []})
                    .CalculatedSteps // only take final result
                    |> List.rev // put them in the right order   

        // build initial PlayPlan
        let playPlan = {
            PlayPlan.CurrentStep = calculatedSteps |> List.head
            RemainingSteps = calculatedSteps |> List.tail
        }

        // build Cmd to dispatch TimerTick message after approximately 1 second
        let timerCmd = Cmd.OfAsync.perform (fun _ -> Async.Sleep 1_000) () (fun _ -> TimerTick)
        ({model with Running = true; StartTime = now; PlayPlan = Some playPlan}, timerCmd)
    | TimerTick -> 
        (model, Cmd.none)

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

let startSessionButton (model: IStore<Model>) dispatch =
            Bind.el ((model |> Store.map (fun m -> (m.Running, List.isEmpty m.TimerStepsReversed)) |> Store.distinct), 
                    fun (running, noSteps) ->
                        if running || noSteps then
                            Html.text ""
                        else 
                            Html.button [
                                type' "button"
                                onClick (fun _ -> dispatch StartSession) []
                                Html.text "Start session"
                            ]
                )

let showElapsedTime (model: IStore<Model>) dispatch =
    Bind.el ((model |> Store.map getElapsedTime), fun elapsedTime ->
                Html.div (formatElapsedTime elapsedTime)
            )

let view() =
    // create the application with The Elm Architecture
    let model, dispatch = () |> Store.makeElmish init (update (fun () -> Ticks System.DateTime.UtcNow.Ticks)) ignore

    Html.div [
        Html.div [
            planEditView model dispatch
            meditationPlanView model dispatch
            startSessionButton model dispatch
            showElapsedTime model dispatch
        ]
    ]

view() |> Program.mountElement "sutil-app"
