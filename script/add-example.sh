#!/usr/bin/env bash
set -euo pipefail

import_code_block="import Examples.ExampleName as ExampleName"

example_code_block=$(cat <<-END
ExampleName.example
    |> Example.wrapMsg ExampleNameMsg
        (\msg ->
            case msg of
                ExampleNameMsg childMsg ->
                    Just childMsg

                _ ->
                    Nothing
        )
END
)

state_code_block="ExampleNameState ExampleName.State"
msg_code_block="ExampleNameMsg ExampleName.Msg"

imports="import Example exposing (Example)"

all=$(cat <<-ALL
all : List (Example State Msg)
all =
    [ ]
ALL
)

state=$(cat <<-STATE
type State
    =
STATE
)

msg=$(cat <<-MSG
type Msg
    =
MSG
)

cd component-catalog/src/Examples/
i=0
for example in *
do
  example_name=${example%".elm"}

  imports+="\n${import_code_block//ExampleName/"$example_name"}"

  subbed_example="${example_code_block//ExampleName/"$example_name"}"

  subbed_state=${state_code_block//ExampleName/"$example_name"}
  subbed_msg=${msg_code_block//ExampleName/"$example_name"}
  if [ $i -eq 0 ]
  then
    all="${all//\]/${subbed_example}}\n    ]"
    state+=" ${subbed_state}"
    msg+=" ${subbed_msg}"
  else
    all="${all//\]/    , ${subbed_example}}\n    ]"
    state+="\n    | ${subbed_state}"
    msg+="\n    | ${subbed_msg}"
  fi

  i=$((i+1))
done

cd ..
printf "module Examples exposing (Msg, State, all)\n\n" >| Examples.elm
printf "${imports}\n\n\n" >> Examples.elm
printf "${all}\n\n\n" >> Examples.elm
printf "${state}\n\n\n" >> Examples.elm
printf "${msg}\n\n\n" >> Examples.elm
