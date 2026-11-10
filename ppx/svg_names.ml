(* SVG attribute names with upper-case letters that the SVG specification
   writes in camelCase. Source: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute
   (SVG 2 and the SVG 1.1 names that browsers still accept). Every other
   multi-word SVG attribute is dashed, for example [clip-path]. *)
let camel_case_allow_list =
  [
    "allowReorder";
    "attributeName";
    "attributeType";
    "autoReverse";
    "baseFrequency";
    "baseProfile";
    "calcMode";
    "clipPathUnits";
    "contentScriptType";
    "contentStyleType";
    "diffuseConstant";
    "edgeMode";
    "externalResourcesRequired";
    "filterRes";
    "filterUnits";
    "glyphRef";
    "gradientTransform";
    "gradientUnits";
    "kernelMatrix";
    "kernelUnitLength";
    "keyPoints";
    "keySplines";
    "keyTimes";
    "lengthAdjust";
    "limitingConeAngle";
    "markerHeight";
    "markerUnits";
    "markerWidth";
    "maskContentUnits";
    "maskUnits";
    "numOctaves";
    "pathLength";
    "patternContentUnits";
    "patternTransform";
    "patternUnits";
    "pointsAtX";
    "pointsAtY";
    "pointsAtZ";
    "preserveAlpha";
    "preserveAspectRatio";
    "primitiveUnits";
    "refX";
    "refY";
    "repeatCount";
    "repeatDur";
    "requiredExtensions";
    "requiredFeatures";
    "specularConstant";
    "specularExponent";
    "spreadMethod";
    "startOffset";
    "stdDeviation";
    "stitchTiles";
    "surfaceScale";
    "systemLanguage";
    "tableValues";
    "targetX";
    "targetY";
    "textLength";
    "viewBox";
    "viewTarget";
    "xChannelSelector";
    "yChannelSelector";
    "zoomAndPan";
  ]

let is_upper character = character >= 'A' && character <= 'Z'

let dashed name =
  let buffer = Buffer.create (String.length name + 4) in
  String.iter
    (fun character ->
      if character = '_' then
        Buffer.add_char buffer '-'
      else if is_upper character then (
        Buffer.add_char buffer '-';
        Buffer.add_char buffer (Char.lowercase_ascii character)
      ) else
        Buffer.add_char buffer character
    )
    name;
  Buffer.contents buffer

let strip_prefix ~prefix name =
  let length = String.length prefix in
  if String.length name > length && String.starts_with ~prefix name then
    Some (String.sub name length (String.length name - length))
  else
    None

let printed_name label =
  let label =
    if String.ends_with ~suffix:"_" label then
      String.sub label 0 (String.length label - 1)
    else
      label
  in
  if List.mem label camel_case_allow_list then
    label
  else
    match label with
    | "xmlnsXlink" ->
        "xmlns:xlink"
    | _ -> (
        match strip_prefix ~prefix:"xlink" label with
        | Some rest ->
            "xlink:" ^ String.lowercase_ascii rest
        | None -> (
            match strip_prefix ~prefix:"xml" label with
            | Some rest when not (String.starts_with ~prefix:"ns" rest) ->
                "xml:" ^ String.lowercase_ascii rest
            | _ ->
                dashed label
          )
      )

let violation name =
  let has_colon = String.contains name ':' in
  let starts_with_namespace =
    (String.starts_with ~prefix:"xlink" name
    || String.starts_with ~prefix:"xml" name
    )
    && name <> "xmlns"
  in
  if starts_with_namespace && not has_colon then
    Some "starts with xlink or xml and has no colon"
  else if
    String.exists is_upper name && not (List.mem name camel_case_allow_list)
  then
    Some "has an upper-case letter and is not in the allow-list"
  else
    None
