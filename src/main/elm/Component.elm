module Component where
{-| Create standalone components.
-}

import Html exposing (Html)
import Task exposing (Task)
import Maybe
import Signal exposing (Address)

-- Note that the original StartApp uses `Task Never ()` in an attempt to force
-- callers to deal explicitly with error conditions. However, it is doubtful
-- whether that actually works, since the original examples do not actually
-- deal with error conditions -- and, do so silently. Also, it seems awkward
-- to force the error handling through the Task's success case -- we have
-- success and failure paths in Task for a reason, after all.

{-} A convenient alias for a task where the 'real' result and error have been
handled. The 'tuple 0' () is a convenient terminating type because the result
and error will often be dealt with using something like:

    actualTask
    `andThen` notify address HandleResult
    `onError` notify address HandleError

... which, given the return type of `notify`, will produce a Task () ()
-}
type alias HandledTask = Task () ()


{-| An `Output` is made up of a couple signals:

  * `html` &mdash; a signal of `Html` representing the current visual
    representation of your app. This should be fed into `main`.

  * `model` &mdash; a signal representing the current model. Generally you
    will not need this one, but it is there just in case. You will know if you
    need this.

  * `tasks` &mdash; a signal of tasks that need to get run. Your app is going
    to be producing tasks in response to all sorts of events, so this needs to
    be hooked up to a `port` to ensure they get run.
-}
type alias Output model =
    { html : Signal Html
    , model : Signal model
    , tasks : Signal HandledTask
    }


{-| Start an application. It requires a bit of wiring once you have created an
`Output`. It should pretty much always look like this:

    app =
        start { init = init, view = view, update = update, inputs = [] }

    main =
        app.html

    port tasks : Signal (Task () ())
    port tasks =
        app.tasks

So once we start the `Output` we feed the HTML into `main` and feed the resulting
tasks into a `port` that will run them all.
-}
start : Component model action -> Output model
start component =
    let
        -- messages : Signal.Mailbox (Maybe action)
        messages =
            Signal.mailbox Nothing

        -- address : Address action
        address =
            Signal.forwardTo messages.address Just

        -- update : Maybe action -> Update model -> Update model
        update (Just action) { model } =
            component.update address action model

        -- inputs : Signal (Maybe action)
        inputs =
            Signal.mergeMany (messages.signal :: List.map (Signal.map Just) component.inputs)

        -- modelAndTask : Signal (Update model)
        modelAndTask =
            Signal.foldp update (component.init address) inputs

        -- model : Signal model
        model =
            Signal.map .model modelAndTask

        -- task : Signal HandledTask
        task =
            Signal.map .task modelAndTask

    in
        { html = Signal.map (component.view address) model
        , model = model
        , tasks = task
        }


type alias Component model action =
  {
    init: Address action -> Update model,
    update: Address action -> action -> model -> Update model,
    view: Address action -> model -> Html,
    inputs: List (Signal action)
  }

type alias Update model =
  {
    model: model,
    task: HandledTask
  }

return : model -> Update model
return model =
  {
    model =
      model,
    task =
      Task.succeed ()
  }

returnAndRun: model -> HandledTask -> Update model
returnAndRun model task =
  {
    model =
      model,
    task =
      task
  }
