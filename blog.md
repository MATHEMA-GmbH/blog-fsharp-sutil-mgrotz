# Outline
## What is F# Advent Calender and what is F#
## What are we building?
Grobe Beschreibung mit gif vom Ergebnis

## Writing JavaScript in F#
Was ist Fable
Was ist Svelte
Was ist Sutil, was nutzt Sutil

## Clone the project
npx degit davedawkins/sutil-template-elmish
dotnet tool restore && npm install
npm run start
-> http://localhost:8080

## The Elm Architecture

## What is the state -> Model

## What can happen -> Messages and update

## What should be shown -> views

## Increment : Input and listing the steps
- Input Bindings in Forms
- Event Handling
- Store
- Reactive bindings, Bind.el vs Bind.each

## Increment : Starting the timer, countdown

## Increment : Playing audio
- The audio element
- Bind.each with keyed collections to avoid restarting the sound every time
- Additional logic to allow multiple sounds to play at once, but not restarting all sounds on Model change due to reactivity

