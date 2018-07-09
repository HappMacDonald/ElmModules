module FindRecord exposing (findRecord)

filterRecord : key -> ( record -> key ) -> record -> Maybe record -> Maybe record
filterRecord key keyGetter record intermediateResult =
  case intermediateResult of
    Nothing ->
      if keyGetter record == key then
        Just record
      else
        Nothing
        
    Just result ->
      Just result


findRecord : ( record -> key ) -> key -> (List record) -> Maybe record
findRecord keyGetter key grid =
    List.foldl (filterRecord key keyGetter) Nothing grid
