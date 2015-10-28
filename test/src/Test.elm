module Test where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import StartApp exposing (App)
import Component
import StructuralEditor.StringListEditor as StringListEditor

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Model =
  StringListEditor.Model

type alias Action =
  StringListEditor.Action

app : App Model
app =
  Component.run
    {
      init =
        StringListEditor.init actionMailbox.address url,
      update =
        StringListEditor.update actionMailbox.address,
      view address model =
        StringListEditor.view StringListEditor.lineSeparator address model |> handleAutofocus,
      inputs =
        inputs
    }

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox StringListEditor.None

url : String
url =
  "https://thsoft.firebaseio.com/DUX/test"

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]

handleAutofocus : Html -> Html
handleAutofocus html =
  Html.div
    []
    [
      Html.node "script" [] [Html.text script],
      html
    ]

script = """
var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    handleAutofocus(mutation.addedNodes);
  });
});
var target = document.querySelector('body > div');
var config = { childList: true, subtree: true };
observer.observe(target, config);

function handleAutofocus(nodeList) {
  for (var i = 0; i < nodeList.length; i++) {
    var node = nodeList[i];
    if (node instanceof Element && node.hasAttribute('data-autofocus')) {
      node.focus();
      break;
    } else {
      handleAutofocus(node.childNodes);
    }
  }
}
"""
