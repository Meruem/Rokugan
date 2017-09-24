module JsonSerializers

open GameTypes

open System.IO
open System.Xml
open System.Text

open Newtonsoft.Json

let a = JsonConvert.SerializeObject (Province 3)
let b = JsonConvert.DeserializeObject<ZoneName> a 

/// Object to Json 
// let json<'t> (myObj:'t) =   
//     use ms = new MemoryStream() 
//     (DataContractJsonSerializer(typeof<'t>)).WriteObject(ms, myObj) 
//     Encoding.Default.GetString(ms.ToArray()) 


// /// Object from Json 
// let unjson<'t> (jsonString:string)  : 't =  
//     use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(jsonString)) 
//     let obj = (DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
//     obj :?> 't
    
// let zoneNameToJson (zone: ZoneName) = 
//     json {
//         let str = 
//             match zone with
//             | DynastyInProvinces x -> "DynastyInProvinces," + x.ToString()
//             | Province x -> "Province,"  + x.ToString()
//             | _ -> zone.ToString()  
//         do! Json.write "ZoneName" str }


// let zoneNameFromJson = json {
//     let! (zn:string) = Json.read "ZoneName"
//     return 
//         (if zn.StartsWith "DynastyInProvinces," then 
//             let x = zn.Split ',' 
//             let y = int (x.[1])
//             DynastyInProvinces y
//         else if zn.StartsWith "Province," then 
//             let x = zn.Split ',' 
//             let y = int (x.[1])
//             Province y
//         else fromString<ZoneName> zn) }

// let cardToJson (c:Card) = json {
//     do! Json.write "Id" (toString c.Id)
//     do! Json.write "Title" (toString c.Title)
//     do! Json.writeWith duToJson "Player" c.Owner
//     do! Json.write "Fate" c.Fate
//     //do! Json.writeWith (zoneNameToJson c.Zone) "Zone" c.Zone
//     //do! Json.write "States" (c.States |> Set.toArray)
//     }

// let gameStateToJson (gs:GameState) = json {
//     do! Json.write "TurnNumber" gs.TurnNumber
//     do! Json.writeWith duToJson "FirstPlayer" gs.FirstPlayer
//     do! Json.writeWith duToJson "ActivePlayer" gs.ActivePlayer
//     do! Json.writeWith duToJson "GamePhase" gs.GamePhase
//     // do! match gs.AttackState with
//     //     | None ->  Json.write "AttackState" "none" 
//     //     | Some st -> Json.write "AttackState" st) 
//     }        