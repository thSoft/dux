module StructuralEditor.ValueEditorContexts where

import StructuralEditor.Codecs as Codecs
import StructuralEditor.StringConverters as StringConverters
import StructuralEditor.ValueEditor as ValueEditor

string : ValueEditor.Context String
string =
  {
    codec =
      Codecs.string,
    stringConverter =
      StringConverters.string
  }

int : ValueEditor.Context Int
int =
  {
    codec =
      Codecs.int,
    stringConverter =
      StringConverters.int
  }

float : ValueEditor.Context Float
float =
  {
    codec =
      Codecs.float,
    stringConverter =
      StringConverters.float
  }

bool : ValueEditor.Context Bool
bool =
  {
    codec =
      Codecs.bool,
    stringConverter =
      StringConverters.bool
  }
