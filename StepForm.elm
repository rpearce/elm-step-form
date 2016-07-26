import Html exposing (Html, button, div, h1, h2, h3, input, li, text, textarea, ul)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- Model


type alias Model =
  { step : Int
  , name : String
  , age : Int
  , phone : String
  , address : String
  , postalCode : String
  , medicationInput : String
  , medications : List Medication
  }

type alias Medication =
  { name : String }

model : Model
model =
  { step = 1
  , name = ""
  , age = 0
  , phone = ""
  , address = ""
  , postalCode = ""
  , medicationInput = ""
  , medications = []
  }

newMedication : String -> Medication
newMedication name =
  { name = name }


-- UPDATE


type Msg
  = PrevStep
  | NextStep
  | Name String
  | Age String
  | Phone String
  | Address String
  | PostalCode String
  | UpdateMedicationField String
  | AddMedication

update : Msg -> Model -> Model
update msg model =
  case msg of
    PrevStep ->
      { model | step = model.step - 1 }

    NextStep ->
      { model | step = model.step + 1 }

    Name name ->
      { model | name = name }

    Age age ->
      { model | age = Result.withDefault 0 (String.toInt age) }

    Phone phone ->
      { model | phone = phone }

    Address address ->
      { model | address = address }

    PostalCode postalCode ->
      { model | postalCode = postalCode }

    UpdateMedicationField name ->
      { model | medicationInput = name }

    AddMedication ->
      { model
        | medications = model.medications ++ [newMedication model.medicationInput]
      }


-- VIEW


view : Model -> Html Msg
view model =
  div [ class "form" ]
    [ h1 [] [ text "Elm Step Form" ]
    , div [ classList [ ( "form__item", True ), ( "form__item--active", model.step == 1) ] ]
      [ h2 [] [ text "Step 1" ]
      , input [ type' "text", placeholder "Name", onInput Name ] []
      , input [ type' "number", placeholder "Age", onInput Age ] []
      , input [ type' "phone", placeholder "Phone", onInput Phone ] []
      ]
    , div
      [ classList [ ( "form__item", True ), ( "form__item--active", model.step == 2) ] ]
      [ h2 [] [ text "Step 2" ]
      , input [ type' "text", placeholder "Street Address", onInput Address ] []
      , input [ type' "text", placeholder "Postal Code", onInput PostalCode ] []
      ]
    , div
      [ classList [ ( "form__item", True ), ( "form__item--active", model.step == 3) ] ]
      [ h2 [] [ text "Step 3" ]
      , div []
        [ textarea [ placeholder "Describe your symptoms..." ] [] ]
      , div []
        [ h3 [] [ text "Current Medications" ]
        , input [ type' "text", placeholder "Enter Medication", onInput UpdateMedicationField ] []
        , button [ onClick AddMedication ] [ text "Add Medication" ]
        , ul [] <| List.map medicationItem (List.map .name model.medications)
        ]
      ]
    , stepButton PrevStep (model.step == 1) "Previous Step"
    , stepButton NextStep (model.step == 3) "Next Step"
    ]

stepButton : Msg -> Bool -> String -> Html Msg
stepButton msg isHidden buttonText =
  button
    [ classList [ ( "form__button", True ), ( "form__button--hidden", isHidden ) ]
    , onClick msg ] [ text buttonText ]

medicationItem : String -> Html Msg
medicationItem ( name ) =
  li [] [ text name ]
