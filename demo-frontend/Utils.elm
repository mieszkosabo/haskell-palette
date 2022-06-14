module Utils exposing (..)

import Http
import Types exposing (..)

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message

-- drops elements from list until reaches the end or an element that doesn't satisfy
-- the condition function. Note: it removes that element as well 
-- (in contrary to Haskell base implementation)
dropWhile : (a -> Bool) -> List a -> List a
dropWhile fn l = 
    case l of
        [] -> []
        (c :: rest) -> if fn c then dropWhile fn rest else rest


-- removes "data:image/*;base64," prefix from the string leaving just the actual
-- base64 part
imageUrlToPureBase64 : ImageString -> ImageString
imageUrlToPureBase64 img = String.fromList (dropWhile ((/=) ',') (String.toList img))

const : a -> b -> a
const x _ = x

algorithmToString : Algorithm -> String
algorithmToString algorithm = 
    case algorithm of
        MedianCut -> "median_cut"
        Kmeans -> "k_means"
        KmeansPP -> "k_means_pp"