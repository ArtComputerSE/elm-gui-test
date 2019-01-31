module Profile.Profile exposing (LocationLinks, Person, PersonMartialStatus(..), PersonSex(..), Profile, ProfileKind(..), SearchResult, SearchResult_embedded, SearchResult_linksSelf, decodeProfile, decodeSearchResult)

import Json.Decode as Decode exposing (map, null, nullable, oneOf)
import Json.Decode.Pipeline exposing (required)


type alias SearchResult =
    { embedded : SearchResult_embedded
    , links : LocationLinks
    }


type alias SearchResult_linksSelf =
    { href : String
    }


type alias LocationLinks =
    { self : SearchResult_linksSelf
    }


decodeSearchResult : Decode.Decoder SearchResult
decodeSearchResult =
    Decode.succeed SearchResult
        |> required "_embedded" decodeSearchResult_embedded
        |> required "_links" decodeLocationLinks


decodeSearchResult_linksSelf : Decode.Decoder SearchResult_linksSelf
decodeSearchResult_linksSelf =
    Decode.succeed SearchResult_linksSelf
        |> required "href" Decode.string


decodeLocationLinks : Decode.Decoder LocationLinks
decodeLocationLinks =
    Decode.succeed LocationLinks
        |> required "self" decodeSearchResult_linksSelf



-- Unique to Profile starts here. Above should be refactored to a utility.


type alias SearchResult_embedded =
    { profileList : List Profile
    }


type alias Profile =
    { id : String
    , userName : String
    , kind : ProfileKind
    , person1 : Person
    , person2 : Maybe Person
    , description : String
    }


type ProfileKind
    = SingleProfile
    | CoupleProfile


type alias Person =
    { id : String
    , sex : PersonSex
    , height : Maybe Int
    , weight : Maybe Int
    , martialStatus : Maybe PersonMartialStatus
    , description : String
    }


type PersonSex
    = Male
    | Female
    | Trans
    | Unknown


type PersonMartialStatus
    = Single
    | Married
    | Partner
    | Apart


decodeSearchResult_embedded : Decode.Decoder SearchResult_embedded
decodeSearchResult_embedded =
    Decode.succeed SearchResult_embedded
        |> required "profileList" (Decode.list decodeProfile)


decodeProfile : Decode.Decoder Profile
decodeProfile =
    Decode.succeed Profile
        |> required "id" Decode.string
        |> required "userName" Decode.string
        |> required "kind" decodeProfileKind
        |> required "person1" decodePerson
        |> required "person2" (nullable decodePerson)
        |> required "description" Decode.string


decodeProfileKind : Decode.Decoder ProfileKind
decodeProfileKind =
    Decode.map stringToProfileKind Decode.string


stringToProfileKind : String -> ProfileKind
stringToProfileKind string =
    if string == "SINGLE" then
        SingleProfile

    else
        CoupleProfile


decodePerson : Decode.Decoder Person
decodePerson =
    Decode.succeed Person
        |> required "id" Decode.string
        |> required "sex" decodePersonSex
        |> required "height" (nullable Decode.int)
        |> required "weight" (nullable Decode.int)
        |> required "martialStatus" (nullable decodeMartialStatus)
        |> required "description" Decode.string


nullable : Decode.Decoder a -> Decode.Decoder (Maybe a)
nullable decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


decodePersonSex : Decode.Decoder PersonSex
decodePersonSex =
    Decode.map stringToProfileSex Decode.string


stringToProfileSex : String -> PersonSex
stringToProfileSex string =
    if string == "FEMALE" then
        Female

    else if string == "MALE" then
        Male

    else if string == "TRANS" then
        Trans

    else
        Unknown


decodeMartialStatus : Decode.Decoder PersonMartialStatus
decodeMartialStatus =
    Decode.map stringToPersonMartialStatus Decode.string


stringToPersonMartialStatus : String -> PersonMartialStatus
stringToPersonMartialStatus str =
    if str == "MARRIED" then
        Married

    else if str == "APART" then
        Apart

    else if str == "PARTNER" then
        Partner

    else
        Single
