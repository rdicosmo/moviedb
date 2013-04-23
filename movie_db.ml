(**************************************************************************)
(* Sample database of movies and theaters, for the 2013 PFAV project      *)
(*                                                                        *)
(*  Author(s):  Roberto Di Cosmo (2013 - )                                *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify          *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the LICENSE file for more information.                   *)
(**************************************************************************)


(** Sample database of theaters and movies showtimes for the project, dumped on April 23rd 2013 *)

(* Structure of the data *)

type theaterdb = entry list
and  entry = name * address * coords option * movietime list
and  coords = (string * float) list
and  movietime = title * props * showtimes
and  props = [`Genre of string | `Duration of string | `Public of string] list    (* assoc list of extra properties *)
and  showtimes = ( [`VOstFr | `VF] * showtime ) list
and  showtime = time list
and  name = string
and  address = string
and  title = string
and  time = string
;;

(** Some query combinators *)

(* Great circle distance calculations *)

let pi = 3.141592;;

let dist (lat1,long1) (lat2,long2) = 
  let lat1 = lat1 *. pi /. 180. in
  let lat2 = lat2 *. pi /. 180. in
  let long1 = long1 *. pi /. 180. in
  let long2 = long2 *. pi /. 180. in
  let r = 6371. in (* km *)
  acos (sin(lat1)*.sin(lat2)+.cos(lat1)*.cos(lat2)*.cos(long1-.long2)) *. r;;

(* Find theaters at a given distance (in meters) from a given point *)

let theaters_around lat long radius (db:theaterdb) =
  let inradius = function 
      (_,_,None,_) -> false 
    | (_,_,Some cl,_) -> 
	let d = dist (lat, long) (List.assoc "lat" cl, List.assoc "lng" cl)
	in truncate (d*.1000.) <= radius
  in List.filter inradius db
;;

(* Find theaters showing a given movie *)

let showing m (db: theaterdb) = 
  List.filter (fun (_,_,_,tl) -> List.mem m (List.map (fun (t,_,_) -> t) tl)) db;;

(* Example combination of queries : as radius is generally small, we
   start by filtering on it
 *)

let showing_around m lat long radius db =
  showing m (theaters_around lat long radius db);;

(* We can also use the same abbreviation as in Yojson.Basic.Util *)

let ( |> ) x f = f x;; 

let showing_around m lat long radius db =
 db 
  |> theaters_around lat long radius
  |> showing m;;


(** The database itself; notice that coordinates are still missing for many theaters *)

let moviedbcoords =
[("UGC Cin\195\169-Cit\195\169 les Halles", "7 place de la Rotonde, Paris",
  None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF,
      ["09:10"; "11:05"; "13:00"; "14:55"; "16:50"; "18:45"; "20:40";
       "22:35"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:25"; "11:40"; "13:50"; "16:00"; "18:10"; "20:20"; "22:30"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["09:15"; "11:50"; "14:25"; "17:00"; "19:35"; "22:10"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["09:20"; "11:50"; "14:35"; "17:10"; "19:45"; "22:15"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:30"; "11:40"; "14:00"; "16:10"; "18:15"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["09:20"; "11:55"; "14:30"; "17:05"; "19:40"; "22:15"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VOstFr, ["20:35"; "22:40"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr,
      ["09:20"; "11:50"; "14:00"; "16:10"; "18:15"; "20:25"; "22:35"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr,
      ["09:10"; "11:20"; "13:35"; "15:50"; "18:05"; "20:20"; "22:35"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:10"; "11:00"; "12:50"; "14:45"; "16:40"; "18:35"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr,
      ["09:10"; "11:20"; "13:30"; "15:45"; "18:00"; "20:15"; "22:30"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:15"; "11:25"; "13:35"; "15:45"; "18:00"; "20:10"; "22:20"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:20"; "11:30"; "13:45"; "15:55"; "18:10"; "20:20"; "22:30"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:20"; "11:40"; "13:50"; "16:05"; "18:15"; "20:25"; "22:30"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr,
      ["09:25"; "11:45"; "13:50"; "16:05"; "18:15"; "20:20"; "22:30"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr,
      ["09:05"; "11:00"; "12:55"; "14:50"; "16:45"; "18:40"; "20:35";
       "22:30"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VOstFr, ["20:25"; "22:35"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["09:30"; "13:45"; "16:30"; "19:15"; "22:00"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr,
      ["09:05"; "11:15"; "13:30"; "15:45"; "18:00"; "20:15"; "22:30"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:25"; "11:50"; "14:05"; "16:15"; "18:20"; "20:25"; "22:30"])]);
   ("Cloud Atlas",
    [`Duration "2h45"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["09:30"; "14:50"; "18:10"; "21:30"])])]);
 ("MK2 Beaubourg", "50 Rue Rambuteau, Paris",
  Some [("lat", 48.86156); ("lng", 2.352324)],
  [("Sugar Man",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["13:35"; "15:35"; "17:35"; "19:35"; "21:35"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["13:10"; "15:20"; "17:30"; "19:50"; "22:00"])]);
   ("Wadjda", [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:05"])]);
   ("Pieta", [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:15"; "17:25"; "21:45"])]);
   ("La Playa", [`Duration "1h30"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:40"; "15:45"; "17:50"; "20:05"; "22:10"])]);
   ("Le Repenti", [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:10"])]);
   ("Clip",
    [`Duration "1h42"; `Public "Interdit moins 16 ans"; `Genre "Drame"],
    [(`VOstFr, ["13:10"; "15:25"; "17:40"; "19:55"; "22:05"])]);
   ("Kinshasa Kids",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:15"; "15:30"; "19:45"])]);
   ("Les Voisins de Dieu",
    [`Duration "1h38"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:20"])]);
   ("Flammes",
    [`Duration "1h28"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:30"; "15:40"; "17:45"; "20:00"; "22:05"])]);
   ("Paroles de conflits",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["11:30"])]);
   ("Les Habitants", [`Duration "1h45"], [(`VOstFr, ["11:20"])])]);
 ("UGC Forum Orient Express",
  "Forum des Halles, niveau 4, rue l'Orient-Express, Paris", None,
  [("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["09:30"; "11:55"; "14:20"; "16:45"; "19:15"; "21:40"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:30"; "11:35"; "13:40"; "15:45"; "17:50"; "19:55"; "22:00"])]);
   ("Jack le chasseur de g\195\169ants",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["09:35"; "11:55"; "14:45"; "17:05"]);
     (`VOstFr, ["19:25"; "21:45"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["09:30"; "11:35"; "13:40"; "15:45"; "19:55"])]);
   ("Le Monde fantastique d'Oz",
    [`Duration "2h7"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:00"; "16:20"]); (`VOstFr, ["18:55"; "21:30"])]);
   ("Inch'Allah", [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:10"; "17:50"; "22:00"])]);
   ("Warm Bodies Renaissance",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "\195\137pouvante/Horreur"],
    [(`VOstFr,
      ["09:30"; "11:35"; "13:40"; "15:45"; "17:50"; "19:55"; "22:00"])]);
   ("Dead Man Down",
    [`Duration "1h57"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:35"; "11:55"; "14:25"; "16:50"; "19:15"; "21:40"])])]);
 ("Le nouveau Latina", "20 rue du Temple, Paris", None,
  [("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["14:00"; "18:00"; "22:00"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:45"])]);
   ("Blancanieves",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["12:00"])]);
   ("El Premio", [`Duration "1h38"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["12:00"])])]);
 ("Gaumont Parnasse", "74 Boulevard du Montparnasse, Paris",
  Some [("lat", 48.843067); ("lng", 2.324532)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:20"; "11:20"; "13:25"; "15:30"; "17:30"; "19:30"; "21:30"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:15"; "11:25"; "13:35"; "15:45"; "17:55"; "20:05"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:30"; "13:10"; "16:10"; "18:55"; "21:40"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["09:20"; "11:50"; "14:20"; "16:50"; "19:20"; "21:50"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["09:20"; "11:50"; "14:20"; "16:50"; "19:20"; "21:50"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:20"; "11:35"; "13:45"; "15:55"; "18:05"]);
     (`VOstFr, ["20:10"; "22:15"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:00"; "12:30"; "15:00"; "17:25"; "19:40"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:00"; "14:15"; "16:15"; "18:15"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["12:15"; "14:45"; "17:10"; "19:30"; "21:45"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["09:20"; "11:50"; "14:20"; "16:50"; "19:20"; "21:50"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:45"; "12:15"; "14:45"; "17:10"; "19:30"; "21:45"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["11:20"; "13:25"; "15:35"; "17:45"; "20:00"; "22:10"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["20:15"; "22:15"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:10"])])]);
 ("Rex", "1 bd Poissonni\195\168re, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:15"; "12:15"; "14:15"; "16:15"; "18:15"; "20:15"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:25"; "13:15"; "16:00"; "18:40"; "21:30"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:30"; "13:15"; "16:00"; "18:45"; "21:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:20"; "12:35"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:20"; "13:05"; "16:05"; "18:50"; "21:30"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:55"; "17:10"; "19:25"; "21:40"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:25"; "13:25"; "16:15"; "18:55"; "21:40"])])]);
 ("MK2 Hautefeuille", "7 Rue Hautefeuille, Paris",
  Some [("lat", 48.852142); ("lng", 2.342717)],
  [("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["13:50"; "15:55"; "20:05"; "22:10"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["21:30"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["12:55"; "15:15"; "17:35"; "19:55"; "22:10"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "16:20"; "18:55"; "22:05"])]);
   ("Les Lendemains",
    [`Duration "1h55"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["12:50"; "15:10"; "17:30"; "19:50"])])]);
 ("UGC Od\195\169on", "124 bd Saint-Germain, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:20"; "12:20"; "14:20"; "16:20"; "18:20"; "20:20"; "22:20"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:20"; "12:30"; "14:40"; "16:50"; "19:15"; "21:30"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:50"; "17:00"; "19:30"; "22:00"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:50"; "13:00"; "15:10"; "17:20"; "19:45"; "22:10"])])]);
 ("UGC Lyon Bastille", "12 Rue de Lyon, Paris",
  Some [("lat", 48.847444); ("lng", 2.371853)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["12:20"; "14:15"; "16:15"; "18:15"; "20:15"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:15"; "16:00"; "19:00"; "21:40"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:45"; "16:30"; "19:15"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:40"; "14:50"; "17:00"; "19:25"; "21:35"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:30"; "15:40"; "17:50"; "20:00"; "22:10"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:25"; "14:30"; "16:45"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["19:30"; "21:45"])])]);
 ("UGC Gobelins", "66 av. des Gobelins, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:15"; "12:15"; "14:15"; "16:15"; "18:15"; "20:15"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:25"; "15:35"; "17:45"; "19:55"; "22:05"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:40"; "13:40"; "16:20"; "19:00"; "21:45"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"; "13:15"; "15:25"; "17:35"; "19:45"; "21:55"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:00"; "12:00"; "14:00"; "16:00"; "18:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:20"; "12:40"; "15:00"; "17:20"; "19:40"; "22:00"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:20"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["22:10"])]);
   ("La Travers\195\169e",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["21:55"])]);
   ("Stars 80",
    [`Duration "1h40"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["19:35"])]);
   ("Associ\195\169s contre le crime",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Policier"],
    [(`VF, ["15:05"])]);
   ("Au galop",
    [`Duration "1h33"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:00"])])]);
 ("MK2 Od\195\169on", "113 Boulevard Saint-Germain, Paris",
  Some [("lat", 48.852416); ("lng", 2.338361)],
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:05"; "15:20"; "17:35"; "19:50"; "22:05"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:30"; "16:10"; "19:00"; "21:40"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:10"; "15:25"; "17:40"; "19:55"; "22:10"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:10"; "15:25"; "17:40"; "19:55"; "22:10"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:20"; "15:30"; "17:40"; "19:55"; "22:05"])])]);
 ("UGC Danton", "99 bd Saint-Germain, Paris", None,
  [("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["10:40"; "13:15"; "15:30"; "17:50"; "20:10"; "22:20"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:30"; "12:50"; "15:10"; "17:30"; "19:50"; "22:10"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:20"; "12:40"; "15:00"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:00"; "14:30"; "16:50"; "19:15"; "21:40"])])]);
 ("Gaumont Op\195\169ra c\195\180t\195\169 Capucines",
  "2 bd des Capucines, Paris", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:25"; "12:45"; "15:00"; "17:25"; "19:45"; "22:10"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:30"; "13:20"; "16:05"; "18:55"; "21:45"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:15"; "13:00"; "15:45"; "18:30"; "21:10"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:40"; "19:15"; "21:55"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:15"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:10"; "12:25"; "14:45"; "17:05"; "19:35"; "22:10"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["11:15"; "14:00"; "16:30"; "19:00"; "21:30"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:45"; "16:40"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:50"; "21:20"])])]);
 ("Gaumont Op\195\169ra c\195\180t\239\191\189 Premier",
  "32 rue Louis Legrand, Paris", None,
  [("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"; "15:10"; "19:55"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:50"; "17:30"; "22:10"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["10:20"; "13:00"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:15"; "12:40"; "15:00"; "17:20"; "19:45"; "22:10"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:10"; "12:45"; "15:15"; "17:30"; "19:50"; "22:10"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["10:20"; "12:30"; "15:00"; "17:15"; "19:25"; "21:55"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:40"; "13:30"; "15:45"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["18:00"; "20:10"; "22:20"])])]);
 ("Espace Saint-Michel", "7 place Saint-Michel, Paris", None,
  [("Pierre Rabhi au nom de la terre",
    [`Duration "1h38"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["13:00"; "16:55"; "20:25"; "22:10"])]);
   ("Slow Life",
    [`Duration "1h12"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["12:55"; "15:40"; "20:40"])]);
   ("Outreau, l'autre v\195\169rit\195\169",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["18:40"])]);
   ("Notre Monde",
    [`Duration "1h59"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:45"; "18:30"])]);
   ("Roman\195\168s",
    [`Duration "1h15"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:15"; "17:05"; "22:00"])])]);
 ("MK2 Biblioth\195\168que", "128-162 Avenue de France, Paris",
  Some [("lat", 48.832065); ("lng", 2.375768)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:50"; "15:50"; "17:50"; "19:50"; "21:50"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:45"; "13:00"; "15:10"; "17:20"; "19:55"; "22:00"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["11:00"; "13:45"; "16:45"; "19:30"; "22:00"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:50"; "14:25"; "17:00"; "19:40"; "22:05"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:45"; "14:00"; "16:00"]); (`VOstFr, ["18:00"; "20:00"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:50"; "14:25"; "17:00"; "19:40"; "22:05"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VOstFr, ["22:00"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["13:05"; "15:10"; "17:15"; "19:45"; "21:50"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"; "13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:45"; "14:00"; "16:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:45"; "13:00"; "15:15"; "17:35"; "19:55"; "22:10"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:55"; "14:20"; "16:50"; "19:30"; "22:00"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:50"; "13:05"; "15:20"; "17:35"; "19:50"; "22:05"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["10:50"; "22:05"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:50"; "12:55"; "15:00"; "19:45"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["11:00"; "13:00"; "15:00"; "17:00"; "20:00"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["18:00"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VOstFr, ["11:00"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["14:00"; "17:00"; "20:00"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"; "17:05"; "21:50"])]);
   ("Cloud Atlas",
    [`Duration "2h45"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["13:00"; "16:30"; "20:00"])])]);
 ("Sept Parnassiens", "98 bd du Montparnasse, Paris", None,
  [("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["11:00"; "13:50"; "15:50"; "17:50"; "19:50"; "21:50"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:05"; "13:30"; "15:45"; "18:05"; "20:30"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:20"; "13:35"; "16:10"; "18:30"; "20:45"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["11:30"; "13:45"; "15:45"; "17:45"; "19:45"; "21:45"])]);
   ("Wadjda", [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:15"; "14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("What Richard Did",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:35"; "15:35"; "17:35"; "19:35"; "21:35"])]);
   ("La Playa", [`Duration "1h30"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:30"; "13:55"; "15:50"; "17:55"; "19:55"; "21:55"])])]);
 ("Reflet M\195\169dicis", "3/7 rue Champollion, Paris", None,
  [("Inch'Allah", [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:35"])]);
   ("La T\195\170te la premi\195\168re",
    [`Duration "1h29"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:45"; "21:00"])]);
   ("Stories We Tell", [`Duration "1h48mn"], [(`VOstFr, ["11:30"])]);
   ("Casa Nostra",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["15:35"; "19:30"])]);
   ("Flammes",
    [`Duration "1h28"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["14:05"; "16:05"; "18:05"; "20:05"; "22:05"])]);
   ("Alps", [`Duration "1h33"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:40"])]);
   ("The Act of Killing - L'acte de tuer",
    [`Duration "1h55"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["14:00"; "16:20"; "18:40"])]);
   ("Los Salvajes",
    [`Duration "1h59"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:20"])]);
   ("Antiviral",
    [`Duration "1h44"; `Public "Interdit moins 12 ans"; `Genre "Fantastique"],
    [(`VOstFr, ["17:25"])])]);
 ("UGC Montparnasse", "83 Boulevard du Montparnasse, Paris",
  Some [("lat", 48.843476); ("lng", 2.325641)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:00"; "12:00"; "14:00"; "16:05"; "18:15"; "20:20"; "22:20"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:00"; "12:15"; "14:30"; "16:45"; "19:15"; "21:30"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:20"; "13:15"; "16:15"; "19:05"; "21:50"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:30"; "13:15"; "16:00"; "18:50"; "21:35"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:15"; "12:30"; "15:00"; "17:20"; "19:45"]);
     (`VOstFr, ["22:00"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"; "13:30"; "16:20"; "19:00"; "21:45"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:30"; "12:45"; "15:00"; "22:10"])])]);
 ("UGC Cin\195\169-Cit\195\169 Bercy", "2 Cour Saint-Emilion, Paris",
  Some [("lat", 48.832343); ("lng", 2.385539)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:45"; "11:45"; "13:45"; "15:45"; "17:45"; "19:50"; "22:00"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:00"; "13:00"; "15:20"; "17:50"; "20:10"; "22:25"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:15"; "13:40"; "16:30"; "19:20"; "22:00"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:10"; "13:45"; "16:30"; "19:30"; "22:10"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:15"; "12:40"; "14:50"; "17:05"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:15"; "14:00"; "16:45"; "19:30"; "22:10"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VOstFr, ["20:25"; "22:30"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["10:15"; "13:15"; "15:45"; "18:05"; "20:25"; "22:30"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:00"; "12:30"; "15:00"; "17:25"; "20:00"; "22:25"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:45"; "11:50"; "13:55"; "16:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:20"; "12:45"; "15:10"; "17:30"; "20:00"; "22:15"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:45"; "12:15"; "14:45"; "17:15"; "19:45"; "22:15"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:45"; "12:15"; "14:45"; "17:15"; "19:45"; "22:10"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:40"; "11:45"; "13:50"; "16:00"; "18:10"; "20:20"; "22:30"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["18:10"; "20:20"; "22:30"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VOstFr, ["10:05"; "12:45"; "15:15"; "17:40"; "20:05"; "22:25"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:00"; "12:55"; "16:00"; "18:55"; "21:55"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["09:50"; "12:15"; "14:50"; "17:25"; "19:50"; "22:15"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:00"; "12:50"; "15:10"])]);
   ("Cloud Atlas",
    [`Duration "2h45"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:00"; "13:30"; "17:00"; "20:30"])])]);
 ("MK2 Bastille", "4 Boulevard Beaumarchais, Paris",
  Some [("lat", 48.85461); ("lng", 2.368919)],
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:30"; "16:10"; "19:10"; "21:40"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:05"; "15:10"; "17:15"; "19:35"; "22:00"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["13:15"; "15:15"; "17:30"; "19:45"; "22:05"])]);
   ("Derri\195\168re la colline",
    [`Duration "1h34"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:00"; "15:40"; "18:00"; "22:15"])])]);
 ("Le Champo -Espace Jacques-Tati", "51 rue des Ecoles, Paris", None,
  [("Larmes de joie",
    [`Duration "1h46"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["11:45"; "13:45"; "15:50"; "17:55"; "20:00"; "22:05"])]);
   ("Chaplin, Keaton, \195\137taix et ses pairs",
    [`Duration "1h20"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:10"; "18:10"])]);
   ("Le Cri", [`Duration "1h56"], [(`VOstFr, ["12:00"; "19:40"])]);
   ("L'Innocent", [`Duration "2h5"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:50"])]);
   ("Tout sur ma m\195\168re",
    [`Duration "1h40"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:45"])])]);
 ("Saint-Andr\195\169-des-Arts I",
  "30 rue Saint-Andr\195\169-des-Arts, Paris", None,
  [("Sous le figuier",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "18:00"])]);
   ("What Richard Did",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:00"; "16:45"; "18:30"; "20:15"; "22:00"])]);
   ("El Premio", [`Duration "1h38"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:00"; "20:00"; "22:00"])])]);
 ("Les Trois Luxembourg", "67 rue Monsieur-le-Prince, Paris", None,
  [("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:30"; "19:00"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:20"; "15:30"; "17:40"; "19:50"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["17:00"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "18:30"])]);
   ("\195\128 la merveille",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:30"])]);
   ("Mystery", [`Duration "1h38"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["21:30"])]);
   ("Happiness Therapy",
    [`Duration "2h2"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["17:00"])])]);
 ("Le Desperado", "23 Rue des \195\137coles, Paris",
  Some [("lat", 48.848405); ("lng", 2.348907)],
  [("Le Mentor",
    [`Duration "1h23"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "20:00"; "22:00"])]);
   ("Correspondant 17",
    [`Duration "2h0"; `Public "Tous publics"; `Genre "Policier"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])])]);
 ("Gaumont Gobelins", "58 av. des Gobelins, Paris", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:10"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["12:40"; "16:15"; "19:00"; "21:45"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:30"; "16:30"; "19:10"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:15"; "17:25"; "19:35"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:45"; "21:45"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["13:15"; "16:00"; "18:50"; "21:20"])])]);
 ("Grand Action", "5 rue des Ecoles, Paris", None,
  [("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:00"; "18:00"; "20:00"; "21:55"])]);
   ("Spring Breakers",
    [`Duration "1h32"; `Public "Interdit moins 12 ans"; `Genre "Drame"],
    [(`VOstFr, ["18:00"])]);
   ("Passion", [`Duration "1h41"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["16:00"])]);
   ("Ken Park",
    [`Duration "1h35"; `Public "Interdit moins 16 ans"; `Genre "Drame"],
    [(`VOstFr, ["22:00"])]);
   ("Le Metteur en sc\195\168ne de mariages",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["14:00"])])]);
 ("UGC Op\195\169ra", "34 bd des Italiens, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:50"; "12:50"; "15:00"; "17:10"; "19:15"; "21:30"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:15"; "13:40"; "16:00"; "18:30"; "21:10"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:30"; "12:35"; "14:45"; "17:00"; "19:30"; "21:40"])]);
   ("Sugar Man",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["10:50"; "13:00"; "15:20"; "17:30"; "19:50"; "21:50"])])]);
 ("La Bastille", "5 Rue du Faubourg Saint-Antoine, Paris",
  Some [("lat", 48.853281); ("lng", 2.370536)],
  [("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["11:30"; "13:30"; "15:30"; "17:30"; "19:30"; "21:30"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:40"; "13:45"; "15:50"; "17:55"; "20:00"; "22:05"])]);
   ("Sugar Man",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["11:20"; "13:30"; "15:10"; "16:50"; "20:10"; "21:50"])]);
   ("Blanche Nuit",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["18:30"])])]);
 ("Path\195\169 Wepler", "140 bd de Clichy et 8, av de Clichy, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:25"; "13:30"; "15:40"; "17:50"; "20:00"; "22:05"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:25"; "13:15"; "15:30"; "17:50"; "20:05"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:20"; "13:25"; "16:10"]); (`VOstFr, ["19:05"; "21:45"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:30"; "13:50"; "16:35"; "19:15"; "21:50"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:20"; "13:25"; "16:20"; "19:05"; "21:50"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:15"; "13:20"; "15:40"; "17:55"; "20:10"; "22:20"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:20"; "15:35"; "17:55"]); (`VOstFr, ["20:10"; "22:20"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:35"; "13:00"; "15:20"; "17:40"; "19:55"; "22:15"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:10"; "15:30"; "17:35"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:35"; "13:05"; "15:20"; "17:40"; "20:00"; "22:15"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:50"; "15:30"; "22:15"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["10:40"; "13:20"; "15:30"; "17:50"; "20:05"; "22:20"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:15"; "17:45"; "20:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["19:40"; "22:05"])])]);
 ("Studio Galande", "42 Rue Galande, Paris",
  Some [("lat", 48.851608); ("lng", 2.347052)],
  [("Queen of Montreuil",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["12:30"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"])]);
   ("Stories We Tell",
    [`Duration "1h48"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["18:15"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["14:00"])]);
   ("Photo", [`Duration "1h16"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:50"])]);
   ("Alps", [`Duration "1h33"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["22:00"])]);
   ("Passion", [`Duration "1h41"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["20:10"])])]);
 ("Path\195\169 Quai d'Ivry",
  "5 Rue Fran\195\167ois Mitterrand, Ivry-sur-Seine",
  Some [("lat", 48.824268); ("lng", 2.387471)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:30"; "12:20"; "14:15"; "16:15"; "18:15"; "20:15"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:15"; "15:20"; "17:30"; "19:40"; "21:50"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF,
      ["10:45"; "11:00"; "13:30"; "14:15"; "16:00"; "17:00"; "19:15";
       "19:45"; "22:00"; "22:20"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:00"; "14:00"; "16:55"; "19:40"; "22:10"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:45"; "14:50"; "18:00"; "20:10"; "22:15"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:20"; "14:00"; "16:45"; "19:30"; "22:10"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:40"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:40"; "12:50"; "15:00"; "17:15"; "19:30"; "21:45"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:40"; "12:50"; "15:10"; "17:30"; "19:45"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:20"; "17:20"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:45"; "12:55"; "15:05"; "17:20"; "19:35"; "21:50"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:40"; "13:05"; "15:30"; "17:00"; "19:35"; "22:00"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:45"; "13:10"; "20:00"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["19:30"; "21:40"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["11:15"; "13:20"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["10:30"; "12:45"; "15:00"; "17:30"; "19:55"; "22:15"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["15:30"; "17:40"; "22:05"])])]);
 ("Gaumont Parnasse c\195\180t\195\169 Montparnos",
  "16-18 rue d'Odessa., Paris", None,
  [("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:15"; "11:20"; "13:30"; "15:40"; "17:50"; "20:00"; "22:10"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:25"; "11:30"; "13:40"; "15:50"; "18:00"; "20:10"; "22:15"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["09:50"; "12:45"; "15:40"; "18:35"; "21:30"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["09:30"; "11:55"; "14:20"; "16:45"; "19:15"; "21:45"])])]);
 ("La Filmoth\195\168que du quartier latin", "9 rue Champollion, Paris",
  None,
  [("Dr\195\180le de frimousse, version restaur\195\169e",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Musical"],
    [(`VOstFr, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("La Porte du paradis, version int\195\169grale et restaur\195\169e",
    [`Duration "3h36"; `Public "Tous publics"; `Genre "Western"],
    [(`VOstFr, ["20:10"])]);
   ("Huit et demi, version restaur\195\169e",
    [`Duration "2h18"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:40"])]);
   ("L'Esclave libre",
    [`Duration "2h5"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:00"])]);
   ("Ecrit sur du vent",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["16:10"])])]);
 ("Gaumont Op\195\169ra c\195\180t\195\169 Fran\195\167ais",
  "38 bd des Italiens, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:00"; "15:00"; "17:15"; "19:30"; "21:45"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["14:15"; "17:45"; "20:45"])])]);
 ("Cin\195\169ma le Brady", "39 bd de Strasbourg, Paris", None,
  [("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["20:00"])]);
   ("L'Artiste et son mod\195\168le",
    [`Duration "1h45"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:50"])]);
   ("Wadjda", [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:15"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:10"])]);
   ("Spring Breakers",
    [`Duration "1h32"; `Public "Interdit moins 12 ans"; `Genre "Drame"],
    [(`VOstFr, ["21:40"])]);
   ("Stories We Tell",
    [`Duration "1h48"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["11:35"])]);
   ("Guerri\195\168re",
    [`Duration "1h40"; `Public "Interdit moins 12 ans"; `Genre "Drame"],
    [(`VOstFr, ["16:20"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["13:30"])]);
   ("La Parade",
    [`Duration "1h55"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["11:35"])]);
   ("Zero Dark Thirty",
    [`Duration "2h29"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["21:30"])])]);
 ("Action Christine", "4 Rue Christine, Paris",
  Some [("lat", 48.854368); ("lng", 2.340359)],
  [("Le Charlatan",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("L'Homme des vall\195\169es perdues",
    [`Duration "1h58"; `Public "Tous publics"; `Genre "Western"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])])]);
 ("UGC Rotonde", "103 bd du Montparnasse, Paris", None,
  [("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:00"; "12:15"; "14:35"; "17:00"; "19:30"; "21:50"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:00"; "12:10"; "14:20"; "16:35"; "19:00"; "21:20"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\239\191\189die dramatique"],
    [(`VOstFr, ["10:20"; "12:30"; "15:00"; "17:30"; "19:50"; "22:00"])])]);
 ("L'Arlequin", "76 Rue de Rennes, Paris",
  Some [("lat", 48.851137); ("lng", 2.330523)],
  [("Derri\195\168re la colline",
    [`Duration "1h34"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:10"; "20:10"; "22:10"])]);
   ("Le Repenti", [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "15:35"; "17:25"; "19:15"])]);
   ("Blanche Nuit",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"])]);
   ("La for\195\170t de Jonathas",
    [`Duration "1h38"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:30"])]);
   ("Les Chansons d'amour de Rio",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"])]);
   ("Les yeux de Bacuri",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["19:00"])]);
   ("Tieta", [`Duration "2h20"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:10"])])]);
 ("Cin\195\169ma la Clef", "34 rue Daubenton, Paris", None,
  [("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:45"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["11:30"])]);
   ("La Playa", [`Duration "1h30"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "18:00"; "20:00"])]);
   ("Kinshasa Kids",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:30"])]);
   ("Guerri\195\168re",
    [`Duration "1h40"; `Public "Interdit moins 12 ans"; `Genre "Drame"],
    [(`VOstFr, ["16:00"])]);
   ("5 Cam\195\169ras Bris\195\169es",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["13:15"])]);
   ("Nostalgie de la lumi\195\168re",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["15:00"])])]);
 ("UGC George-V", "146 av. des Champs-Elys\195\169es, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:10"; "12:05"; "14:10"; "16:10"; "18:10"; "20:10"; "22:10"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["11:00"; "13:40"; "16:25"; "19:10"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:45"; "13:00"; "15:25"; "17:50"]);
     (`VOstFr, ["20:10"; "22:20"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["10:30"; "12:45"; "15:00"; "17:20"; "19:45"; "22:20"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:35"; "12:50"; "15:05"; "17:20"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:10"; "12:25"; "14:40"; "17:05"; "19:30"; "21:50"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:10"; "12:25"; "14:40"; "17:05"; "19:30"; "21:50"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["10:25"; "12:45"; "15:05"; "17:30"; "20:00"; "22:20"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:15"; "12:20"; "14:30"; "16:50"; "19:15"; "21:30"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:05"; "12:50"; "15:40"; "18:30"; "21:20"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["19:30"; "22:00"])])]);
 ("Gaumont Parnasse c\195\180t\195\169 Miramar",
  "3 rue du D\195\169part, Paris", None,
  [("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:25"; "11:30"; "13:35"; "15:45"; "17:55"; "20:05"; "22:15"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:15"; "13:15"; "16:15"; "19:00"; "21:40"])]);
   ("Cloud Atlas",
    [`Duration "2h45"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:00"; "13:50"; "17:20"; "20:50"])])]);
 ("Majestic Bastille", "4 Boulevard Richard-Lenoir, Paris",
  Some [("lat", 48.85392); ("lng", 2.369933)],
  [("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:50"; "15:55"; "18:00"; "20:05"; "22:10"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["20:45"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:15"])]);
   ("Le Repenti", [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:00"])])]);
 ("Saint-Andr\195\169-des-Arts II",
  "12 rue G\195\174t-le-C\197\147ur, Paris", None,
  [("La Travers\195\169e",
    [`Duration "55"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:00"; "15:30"; "19:00"; "20:30"; "22:00"])]);
   ("Dead Man Talking",
    [`Duration "1h41"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["17:00"])])]);
 ("MK2 Parnasse", "11 Rue Jules Chaplain, Paris",
  Some [("lat", 48.842868); ("lng", 2.330596)],
  [("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["13:40"; "15:50"; "18:00"; "20:10"; "22:15"])]);
   ("Kinshasa Kids",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:40"; "17:50"; "21:55"])]);
   ("Alceste \195\160 bicyclette",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:50"; "16:10"])]);
   ("Casa Nostra",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["15:40"; "19:50"])]);
   ("Lincoln", [`Duration "2h29"; `Public "Tous publics"; `Genre "Histoire"],
    [(`VOstFr, ["18:30"])]);
   ("Argo", [`Duration "1h59"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["21:40"])])]);
 ("Gaumont Al\195\169sia",
  "73 Avenue du G\195\169n\195\169ral Leclerc, Paris",
  Some [("lat", 48.8282); ("lng", 2.327371)],
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:35"; "15:40"; "17:45"; "20:10"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:50"; "13:10"; "15:25"; "17:40"; "20:00"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:40"; "13:25"; "16:10"; "19:00"; "21:40"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:45"; "17:15"; "19:40"; "21:50"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:45"; "16:25"; "19:10"; "21:55"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"; "15:00"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["11:00"; "13:55"; "16:30"; "19:35"; "22:00"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:25"; "12:40"; "15:00"; "17:20"; "19:45"; "22:10"])])]);
 ("Gaumont Aquaboulevard", "8 rue du Colonel-Pierre-Avia, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:15"; "12:15"; "14:30"; "16:30"; "18:30"; "20:30"; "22:30"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:30"; "13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF,
      ["10:30"; "11:15"; "13:30"; "14:30"; "16:30"; "17:30"; "19:20";
       "21:00"; "22:15"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:45"; "13:30"; "16:15"; "19:00"; "21:45"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:30"; "13:15"; "16:00"; "19:00"; "21:45"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:15"; "12:30"; "14:45"; "17:00"; "19:15"; "21:30"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:15"; "12:40"; "15:00"; "17:20"; "19:40"; "22:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:15"; "12:30"; "14:50"; "17:30"; "20:00"; "22:30"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["11:00"; "14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["10:30"; "12:45"; "15:00"; "17:15"; "19:30"; "21:45"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:30"; "13:00"; "15:20"; "17:45"; "20:10"; "22:30"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"; "12:45"; "15:00"; "17:15"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["10:15"; "12:35"; "15:00"; "17:30"; "20:00"; "22:30"])])]);
 ("Archipel-Paris Cin\195\169", "17 bd de Strasbourg, Paris", None,
  [("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:10"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"])]);
   ("Tabou", [`Duration "1h58"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:15"])]);
   ("La venta del para\195\173so",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["18:20"])])]);
 ("Escurial", "11 bd du Port-Royal, Paris", None,
  [("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:30"; "15:35"; "17:40"; "19:45"; "21:50"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["16:00"])]);
   ("La Religieuse",
    [`Duration "1h54"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:30"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:15"])])]);
 ("Lucernaire", "53 Rue Notre-Dame des Champs, Paris",
  Some [("lat", 48.844251); ("lng", 2.330286)],
  [("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:00"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:45"; "16:00"; "20:30"])]);
   ("Le Premier homme",
    [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:00"; "20:45"])]);
   ("Pieta", [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:00"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "18:15"; "20:30"])]);
   ("Blancanieves",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["18:30"])])]);
 ("Ep\195\169e de Bois", "100 rue Mouffetard, Paris", None,
  [("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["18:30"])]);
   ("Sous le figuier",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:30"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["20:45"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["17:30"; "20:45"])]);
   ("Lincoln", [`Duration "2h29"; `Public "Tous publics"; `Genre "Histoire"],
    [(`VOstFr, ["14:30"])])]);
 ("MK2 Quai-de-Loire", "7 quai de la Loire, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:00"; "15:10"; "17:20"; "19:30"; "21:45"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["12:45"; "15:00"; "17:15"; "19:40"; "22:05"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["13:20"; "16:00"; "19:10"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["12:50"; "15:00"; "17:05"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["13:10"; "15:30"; "17:40"; "20:00"; "22:10"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["12:50"; "15:15"; "17:30"; "22:10"])])]);
 ("MK2 Quai-de-Seine", "14 Quai de la Seine, Paris",
  Some [("lat", 48.885054); ("lng", 2.371419)],
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:10"; "16:15"; "19:00"; "21:45"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["12:50"; "15:10"; "17:30"; "19:50"; "22:10"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:15"; "15:15"; "17:15"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["12:45"; "15:05"; "17:25"; "19:45"; "22:05"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["13:20"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["12:40"; "15:00"; "17:20"; "19:40"; "22:00"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["19:30"; "22:00"])])]);
 ("Les Cinq Caumartin", "101 rue Saint-Lazare, Paris", None,
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:30"; "13:55"; "16:15"; "18:35"; "20:55"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:40"; "13:45"; "15:50"; "17:50"; "19:50"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["12:00"; "14:10"; "16:20"; "18:30"; "20:40"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:45"; "13:55"; "16:05"; "18:20"; "20:30"])])]);
 ("Gaumont Champs-Elys\195\169es (Ambassade)",
  "50 av. des Champs-Elys\195\169es, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:20"; "12:20"; "14:20"; "16:20"; "18:20"; "20:20"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:15"; "13:00"; "15:45"; "18:30"; "21:30"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:45"; "16:30"; "19:10"; "21:50"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"; "13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["10:15"; "12:30"; "14:45"; "17:00"; "19:30"; "22:00"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:15"; "12:50"; "15:30"; "18:30"; "21:15"])])]);
 ("Nouvel Od\195\169on (ex Racine Od\195\169on)",
  "6 rue de l'Ecole-de-M\195\169decine, Paris", None,
  [("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["13:30"; "15:30"; "17:30"; "19:30"; "21:30"])])]);
 ("Gaumont Al\195\169sia c\195\180t\195\169 Mistral",
  "70 av. du G\195\169n\195\169ral-Leclerc, Paris", None,
  [("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["11:00"; "14:00"; "16:20"; "19:10"; "21:30"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:55"; "13:45"; "16:00"; "19:20"; "21:35"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:25"; "12:40"; "14:55"; "17:10"; "19:25"; "21:40"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:35"; "12:45"; "15:05"; "17:20"; "19:35"; "21:45"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["10:25"; "12:35"; "14:45"; "17:00"; "19:15"; "21:30"])])]);
 ("Le Cin\195\169ma du Panth\195\169on", "13, rue Victor Cousin, Paris",
  Some [("lat", 48.847511); ("lng", 2.342257)],
  [("Berberian Sound Studio",
    [`Duration "1h32"; `Public "Tous publics";
     `Genre "\195\137pouvante/Horreur"],
    [(`VOstFr, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])])]);
 ("MK2 Gambetta", "6 rue Belgrand, Paris", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:20"; "13:30"; "15:40"; "17:50"; "20:00"; "22:10"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["13:25"; "16:05"; "19:00"; "21:40"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:15"; "16:00"]); (`VOstFr, ["19:05"; "21:45"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:20"; "15:30"; "17:35"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["19:50"; "21:55"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["10:45"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:30"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:25"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["10:40"])])]);
 ("Gaumont Champs-Elys\195\169es (Marignan)",
  "27-33 av des Champs-Elys\195\169es, Paris", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:30"; "13:30"; "16:15"; "19:00"; "21:45"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:30"; "12:40"; "14:50"; "17:15"]);
     (`VOstFr, ["19:30"; "21:40"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["10:40"; "12:55"; "15:10"; "17:25"; "19:45"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:15"; "12:15"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:15"; "12:30"; "14:40"; "17:00"; "19:20"; "21:45"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:10"; "16:10"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["18:30"; "21:30"])])]);
 ("Bretagne", "73 bd du Montparnasse, Paris", None,
  [("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:50"; "13:05"; "15:25"; "17:45"; "20:00"; "22:15"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["11:10"; "14:20"; "17:30"; "20:40"])])]);
 ("Gaumont Saint-Denis", "8 rue du Mondial-1998, Saint-Denis", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:45"; "15:45"; "17:45"; "20:15"; "22:20"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:40"; "15:40"; "17:45"; "20:15"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "16:45"; "19:20"; "22:05"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:10"; "17:00"; "19:45"; "22:10"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:40"; "17:45"; "22:00"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:45"; "20:05"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:45"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["14:15"; "17:00"; "19:45"; "22:10"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:45"; "17:45"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["14:10"; "16:30"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:45"; "15:55"; "18:05"; "20:15"; "22:20"])]);
   ("Iron Man",
    [`Duration "2h5"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["19:45"; "22:00"])]);
   ("Iron Man 2",
    [`Duration "1h57"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["19:45"; "22:00"])])]);
 ("Etoile Lilas", "place du Maquis du Vercors, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:10"; "15:20"; "17:30"; "19:40"; "21:40"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:25"; "15:35"; "17:45"; "19:55"; "21:55"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:50"; "16:30"; "19:05"; "21:45"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:10"; "19:50"; "21:55"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "16:25"; "19:05"; "21:40"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:20"; "17:40"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["13:30"; "15:30"; "17:35"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["19:45"; "21:50"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:20"; "15:30"; "17:45"; "19:50"; "21:50"])])]);
 ("Gaumont Convention", "29 Rue Alain Chartier, Paris",
  Some [("lat", 48.837697); ("lng", 2.296362)],
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:40"; "20:00"; "22:15"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:50"; "16:30"; "19:10"; "21:55"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:30"; "12:45"; "15:00"; "17:15"; "19:30"; "21:45"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:45"; "13:15"; "15:25"; "17:35"; "20:00"; "22:10"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:30"; "12:50"; "15:10"; "17:20"; "19:30"; "21:45"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["10:30"; "12:45"; "15:00"; "17:10"; "19:40"; "21:50"])])]);
 ("Le Rex", "passage de l'h\195\180tel, Le Palais", None,
  [("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["18:00"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["20:30"])])]);
 ("Max Linder Panorama", "24 boulevard Poissoni\195\168re, Paris",
  Some [("lat", 48.871338); ("lng", 2.344863)],
  [("Samsara",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["13:30"; "15:30"; "17:30"; "22:10"])])]);
 ("M\195\169garama", "8 avenue du Pr\195\169sident Allende, Arcueil", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "20:00"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["16:00"; "18:00"; "22:00"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["20:00"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "18:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["16:00"])])]);
 ("M\195\169garama",
  "44 avenue de la Longue Bertrane, Villeneuve-la-Garenne", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:15"; "22:30"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:15"; "22:30"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "16:45"; "18:00"; "19:30"; "21:00"; "22:15"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "15:00"; "16:00"; "18:00"; "21:00"; "22:30"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:00"; "20:15"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:15"; "22:30"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "18:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["14:00"; "19:30"; "22:15"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["14:00"; "18:00"])]);
   ("La Cage Dor\195\169e",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["20:15"; "22:30"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:45"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["16:00"; "20:15"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["16:45"; "22:15"])]);
   ("Jack le chasseur de g\195\169ants",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["14:00"; "16:45"; "19:30"; "22:15"])]);
   ("Le Monde fantastique d'Oz",
    [`Duration "2h7"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "19:30"])]);
   ("20 ans d'\195\169cart",
    [`Duration "1h32"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["14:00"; "18:00"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:15"; "22:30"])])]);
 ("Le Saint-Germain-des-Pr\195\169s, Salle G. de Beauregard",
  "22 rue Guillaume Apollinaire, Paris", None,
  [("Sugar Man",
    [`Duration "1h25"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["14:00"; "16:00"; "18:00"; "20:00"; "21:45"])])]);
 ("Saint-Lazare-Pasquier", "44 Rue Pasquier, Paris",
  Some [("lat", 48.874849); ("lng", 2.3239)],
  [("La Religieuse",
    [`Duration "1h54"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["12:00"; "14:10"; "16:20"; "18:30"; "20:40"])]);
   ("Sous le figuier",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:55"; "13:50"; "17:30"; "19:20"; "21:05"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["15:40"; "17:40"])]);
   ("Blanche Nuit",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:50"; "14:00"; "15:50"; "19:30"; "21:15"])])]);
 ("MK2 Nation", "133 bd Diderot, Paris", None,
  [("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["13:30"; "16:20"; "19:00"; "21:45"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:00"; "15:15"; "17:30"; "19:45"; "22:00"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["20:45"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:10"; "15:40"; "18:20"])])]);
 ("Path\195\169 Belle-Epine", "Centre commercial Belle-Epine, Thiais", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:50"; "11:50"; "13:50"; "15:50"; "17:50"; "19:50"; "21:50"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:50"; "12:00"; "14:10"; "15:50"; "18:00"; "20:10"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF,
      ["10:20"; "11:50"; "13:00"; "14:30"; "15:40"; "17:10"; "18:20";
       "19:50"; "21:00"; "22:30"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["09:40"; "12:10"; "14:40"; "16:00"; "17:30"; "20:00"; "22:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:00"; "12:30"; "14:40"; "16:50"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:00"; "12:30"; "15:00"; "17:30"; "20:00"; "22:30"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["19:50"; "22:00"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:00"; "13:30"; "15:45"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["09:40"; "12:00"; "14:20"; "16:40"; "17:50"; "20:10"; "22:30"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:50"; "13:50"; "15:50"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:50"; "12:10"; "14:30"; "16:50"; "19:50"; "22:10"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:40"; "12:10"; "14:40"; "17:10"; "19:50"; "22:20"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["10:00"; "12:10"; "19:30"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:40"; "11:40"; "13:45"; "15:50"; "18:00"; "20:20"; "22:30"])]);
   ("La Cage Dor\195\169e",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["20:00"; "22:00"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue, 3D",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["11:50"])]);
   ("Jack le chasseur de g\195\169ants",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["14:30"; "17:00"; "22:00"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["11:00"; "13:40"; "18:40"; "21:00"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:00"; "13:40"; "16:20"; "18:25"; "20:30"; "22:35"])])]);
 ("Le Louxor", "170 boulevard de Magenta, Paris", None,
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("Hannah Arendt",
    [`Duration "1h53"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:00"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "16:00"; "18:45"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["16:15"; "20:15"])]);
   ("Le Repenti", [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:15"; "18:15"])])]);
 ("La Pagode", "57 Bis Rue de Babylone, Paris",
  Some [("lat", 48.851704); ("lng", 2.316322)],
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:45"; "15:45"; "17:45"; "19:45"; "21:50"])])]);
 ("Le Cin\195\169ma des cin\195\169astes", "7 av. de Clichy, Paris", None,
  [("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:05"; "16:05"; "18:05"; "20:05"; "22:05"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:30"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["14:10"; "16:10"; "18:10"; "20:10"; "22:10"])])]);
 ("UGC Cin\195\169-Cit\195\169 La D\195\169fense",
  "Le D\195\180me - Centre commercial les 4 temps - La D\195\169fense 7, La D\195\169fense",
  None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:10"; "12:10"; "14:15"; "16:20"; "18:25"; "20:30"; "22:25"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:15"; "13:30"; "15:45"; "18:00"; "20:15"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:45"; "13:30"; "16:15"; "19:00"; "21:45"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:45"; "13:30"; "16:15"; "19:00"; "21:40"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:45"; "13:30"; "16:15"; "19:00"; "21:45"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:00"; "12:00"; "14:05"; "16:10"; "18:15"; "20:20"]);
     (`VOstFr, ["22:20"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr,
      ["09:35"; "11:35"; "13:40"; "15:50"; "18:00"; "20:10"; "22:20"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:10"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["11:00"; "13:00"; "15:00"; "17:00"; "19:00"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:15"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["09:35"; "12:00"; "14:30"; "17:00"; "19:30"; "22:00"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:10"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:00"; "13:15"; "15:30"; "17:45"; "20:00"; "22:10"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:50"; "11:50"; "13:55"; "16:00"; "18:10"; "20:15"; "22:20"])]);
   ("The Place Beyond the Pines",
    [`Duration "2h20"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["21:10"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VOstFr, ["10:30"; "12:50"; "15:10"; "22:10"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:35"; "11:40"; "13:50"; "16:00"; "18:10"; "20:15"; "22:20"])])]);
 ("UGC Normandie", "116 av. des Champs-Elys\195\169es, Paris", None,
  [("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["10:50"; "13:30"; "16:20"; "19:05"; "21:50"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:20"; "13:00"; "16:00"; "18:45"; "21:30"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["10:10"; "12:30"; "15:00"; "17:20"; "19:50"; "22:10"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr,
      ["10:15"; "12:15"; "14:15"; "16:15"; "18:15"; "20:15"; "22:15"])])]);
 ("UGC Cin\195\169 Cit\195\169 Rosny",
  "16 rue Konrad Adenauer, centre commercial Rosny 2, Rosny-sous-Bois", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:35"; "11:35"; "13:50"; "15:50"; "17:50"; "20:00"; "22:00"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:15"; "11:25"; "13:35"; "15:45"; "17:55"; "20:05"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF,
      ["09:25"; "10:30"; "12:00"; "13:30"; "14:35"; "16:15"; "17:15";
       "19:00"; "19:50"; "21:45"; "22:25"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["09:15"; "11:50"; "14:25"; "17:00"; "19:35"; "22:10"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["09:15"; "11:50"; "14:20"; "16:55"; "19:30"; "22:05"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:15"; "11:25"; "13:35"; "15:45"; "17:55"; "20:05"; "22:15"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:15"; "11:25"; "13:35"; "15:45"; "17:55"; "20:05"; "22:15"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:35"; "13:00"; "15:20"; "17:40"; "20:00"; "22:15"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:15"; "11:15"; "13:20"; "15:20"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:40"; "12:10"; "14:30"; "17:00"; "19:30"; "21:45"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["09:30"; "12:05"; "14:40"; "17:20"; "19:55"; "22:25"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["09:40"; "14:30"; "19:30"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["12:10"; "17:00"; "21:45"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["10:05"; "12:25"; "14:45"; "17:05"; "19:25"; "21:45"])]);
   ("Jack le chasseur de g\195\169ants",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["22:25"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:35"; "11:45"; "13:50"; "16:00"; "18:10"; "20:20"; "22:30"])])]);
 ("L'Entrep\195\180t", "7 Rue Francis de Pressens\195\169, Paris",
  Some [("lat", 48.833995); ("lng", 2.316572)],
  [("La Religieuse",
    [`Duration "1h54"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["19:30"])]);
   ("Queen of Montreuil",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["19:40"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["17:50"])]);
   ("Spring Breakers",
    [`Duration "1h32"; `Public "Interdit moins 12 ans"; `Genre "Drame"],
    [(`VOstFr, ["17:40"; "21:50"])]);
   ("Blancanieves",
    [`Duration "1h44"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["22:10"])]);
   ("Photo", [`Duration "1h16"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["21:40"])]);
   ("Dead Man Talking",
    [`Duration "1h41"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["17:40"])]);
   ("Slow Life",
    [`Duration "1h12"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["20:00"])]);
   ("Week-end Royal",
    [`Duration "1h35"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["15:40"])]);
   ("Les B\195\170tes du sud sauvage",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:40"])]);
   ("Miss Bala", [`Duration "1h53"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:40"])])]);
 ("Mega CGR", "5 av. Joffre, Epinay-sur-Seine", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:15"; "14:00"; "16:00"; "18:20"; "20:20"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:15"; "14:00"; "16:00"; "18:00"; "20:00"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:15"; "14:00"; "16:30"; "17:50"; "20:00"; "21:00"; "22:25"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:15"; "13:40"; "16:00"; "20:05"; "22:25"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:15"; "13:45"; "16:00"; "20:00"; "22:15"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF,
      ["11:15"; "14:00"; "15:30"; "16:00"; "17:20"; "18:00"; "20:00";
       "22:00"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:15"; "14:00"; "16:00"; "18:00"; "20:00"; "22:15"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["11:15"; "13:45"; "16:15"; "18:15"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["20:00"; "22:15"])]);
   ("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["11:15"; "13:45"; "15:50"; "18:05"; "20:15"; "22:30"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["11:15"; "14:00"; "16:00"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["18:00"; "22:15"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["11:15"; "13:45"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["20:00"; "22:30"])]);
   ("Boule & Bill",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["18:15"])])]);
 ("Cin\195\169ma Chaplin Saint-Lambert", "6 rue Peclet, Paris", None,
  [("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["19:30"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:00"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["17:20"])]);
   ("La Religieuse",
    [`Duration "1h54"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:45"])]);
   ("Sous le figuier",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["15:10"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:30"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:00"])]);
   ("Au bout du conte",
    [`Duration "1h52"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["11:15"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["11:45"])]);
   ("40 ans : mode d'emploi",
    [`Duration "2h14"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["20:45"])]);
   ("Week-end Royal",
    [`Duration "1h35"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["11:15"])]);
   ("M\195\182bius",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["21:20"])])]);
 ("le Lincoln", "14 rue Lincoln, Paris", None,
  [("What Richard Did",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "15:45"; "17:45"; "19:45"; "21:45"])]);
   ("La T\195\170te la premi\195\168re",
    [`Duration "1h29"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:55"; "15:55"; "17:55"; "19:55"; "21:40"])])]);
 ("UGC Cin\195\169-Cit\195\169 Cr\195\169teil",
  "Centre commercial Cr\195\169teil-Soleil, Cr\195\169teil", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:10"; "12:10"; "14:10"; "16:10"; "18:10"; "20:10"; "22:10"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["09:45"; "11:50"; "13:55"; "16:00"; "18:05"; "20:10"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:30"; "13:45"; "16:30"; "19:15"; "21:50"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["10:40"; "14:00"; "16:45"; "19:20"; "21:55"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:10"; "12:20"; "14:30"; "17:00"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["10:40"; "13:40"; "16:20"; "19:00"; "21:45"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:50"; "13:20"; "15:30"; "17:45"; "19:55"; "22:05"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:00"; "12:00"; "14:00"; "16:05"; "18:10"; "20:15"; "22:20"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["09:45"; "11:45"; "13:45"; "15:45"; "17:45"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:30"; "13:30"; "15:40"; "17:50"; "20:00"; "22:15"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:50"; "13:15"; "15:30"; "17:40"; "19:55"; "22:10"])]);
   ("Amour & Turbulences",
    [`Duration "1h36"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["20:00"; "22:10"])]);
   ("G.I. Joe : Conspiration, 3D",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["19:45"; "22:05"])]);
   ("La Cit\195\169 Rose",
    [`Duration "1h37"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["09:50"; "11:55"; "14:00"; "16:05"; "22:20"])])]);
 ("Le Studio des Ursulines", "10 rue des Ursulines, Paris", None,
  [("Tape",
    [`Duration "2h44"; `Public "Tous publics";
     `Genre "Film exp\195\169rimental"],
    [(`VOstFr, ["20:00"])])]);
 ("Le Vincennes", "30 av. de Paris, Vincennes", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:50"; "15:50"; "17:50"; "19:50"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:10"; "16:10"; "18:10"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:45"; "15:45"; "17:45"; "19:45"; "21:45"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:40"; "15:40"; "19:40"; "21:40"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["17:40"])]);
   ("Au nom du peuple italien",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["20:30"])])]);
 ("Le Balzac", "1 rue Balzac, Paris", None,
  [("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["13:45"; "15:50"; "17:55"; "20:00"; "22:05"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:50"; "16:10"; "20:45"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:30"])]);
   ("Free Angela",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["14:00"])]);
   ("Le Premier homme",
    [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["16:00"])])]);
 ("Majestic Passy", "18 rue de Passy, Paris", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:15"; "16:15"; "18:15"; "20:15"; "22:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["14:10"; "16:10"; "18:10"; "20:10"; "22:10"])])]);
 ("L'Alcazar", "1 rue de la Station, Asni\195\168res-sur-Seine", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:15"; "16:15"; "20:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:10"; "16:10"; "18:10"; "20:45"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:45"; "18:25"; "20:45"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:00"; "18:05"; "20:40"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["16:05"; "18:15"])]);
   ("Le Festin de Babette",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:55"])])]);
 ("Chaplin Denfert", "24 place Denfert-Rochereau, Paris", None,
  [("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["16:00"])]);
   ("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["17:50"])]);
   ("La Religieuse",
    [`Duration "1h54"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["11:30"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["20:35"])])]);
 ("La Pl\195\169iade", "12 av. Cousin-de-M\195\169ricourt, Cachan", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:20"; "16:10"; "18:00"; "20:20"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:10"; "16:20"; "18:20"; "20:40"])]);
   ("Mariage \195\160 l'anglaise",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["18:10"; "20:30"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:50"; "16:00"])])]);
 ("3 Cin\195\169s Robespierre", "19 av. Robespierre, Vitry-sur-Seine", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["16:00"; "18:00"; "20:30"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["16:00"]); (`VOstFr, ["20:30"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:50"; "16:00"; "20:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["18:15"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["18:10"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"])])]);
 ("Publicis Cin\195\169mas", "133 avenue des Champs-Elys\195\169es, Paris",
  None,
  [("Parker", [`Duration "1h58"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["14:00"; "16:30"; "19:00"; "21:30"])]);
   ("20 ans d'\195\169cart",
    [`Duration "1h32"; `Public "Tous publics";
     `Genre "Com\195\169die romantique"],
    [(`VF, ["19:00"])]);
   ("Sous le figuier",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"])]);
   ("Django Unchained",
    [`Duration "2h44"; `Public "Interdit moins 12 ans"; `Genre "Western"],
    [(`VOstFr, ["21:00"])]);
   ("The Act of Killing - L'acte de tuer",
    [`Duration "1h55"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["16:00"])])]);
 ("Path\195\169 Boulogne-Billancourt",
  "26 rue Le Corbusier, Boulogne-Billancourt", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:05"; "15:25"; "17:45"; "20:05"; "22:20"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VOstFr, ["13:35"; "16:20"; "19:10"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["22:15"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["13:30"; "16:10"; "19:00"; "21:55"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["15:20"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["12:55"; "15:10"; "17:30"; "19:50"; "22:10"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VOstFr, ["19:45"; "22:05"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["12:45"; "17:35"; "19:50"])])]);
 ("UGC V\195\169lizy",
  "Centre commercial V\195\169lizy II, V\195\169lizy-Villacoublay", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:00"; "12:00"; "14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["10:50"; "13:20"; "15:35"; "17:50"; "20:05"; "22:15"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:00"; "13:40"; "16:25"; "19:05"; "21:45"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["11:10"; "13:50"; "16:35"; "19:15"; "21:50"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["10:45"; "13:00"; "15:15"; "17:30"; "19:45"; "21:55"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["11:30"; "13:30"; "15:30"; "17:35"])]);
   ("Effets secondaires",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["10:05"; "12:25"; "14:45"; "17:05"; "19:20"; "21:40"])]);
   ("G.I. Joe : Conspiration",
    [`Duration "1h39"; `Public "Tous publics"; `Genre "Aventure"],
    [(`VF, ["19:40"; "22:05"])])]);
 ("Cin\195\169 104", "104 av. Jean-Lolive, Pantin", None,
  [("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["18:30"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["14:00"; "18:15"; "20:30"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:00"])]);
   ("Camille Claudel 1915",
    [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["14:15"; "20:15"])]);
   ("Queen of Montreuil",
    [`Duration "1h27"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:30"])]);
   ("Des abeilles et des hommes",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["20:45"])])]);
 ("Georges-M\195\169li\195\168s",
  "7 avenue de la R\195\169sistance (Centre commercial Croix de Chaveau), Montreuil",
  None,
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:00"; "20:30"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["18:30"; "20:45"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["20:15"])]);
   ("Des abeilles et des hommes",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["18:15"])])]);
 ("Luxy", "77 av. Georges-Gosnat, Ivry-sur-Seine", None,
  [("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["18:00"])]);
   ("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:30"])]);
   ("Inch'Allah", [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:00"])]);
   ("La Porte du paradis, version int\195\169grale et restaur\195\169e",
    [`Duration "3h36"; `Public "Tous publics"; `Genre "Western"],
    [(`VOstFr, ["20:00"])])]);
 ("Cin\195\169ma Jean-Vilar", "1 rue Paul-Signac, Arcueil", None,
  [("Jappeloup", [`Duration "2h10"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["12:00"])]);
   ("La Source des femmes",
    [`Duration "2h4"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["15:00"])])]);
 ("Espace 1789. Salles Messidor et Flor\195\169al",
  "2-4 rue Alexandre-Bachelet, Saint-Ouen", None,
  [("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:15"; "18:00"])]);
   ("La Belle Endormie",
    [`Duration "1h50"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:15"])]);
   ("40 ans : mode d'emploi",
    [`Duration "2h14"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["20:30"])]);
   ("C\195\180ng Binh, la longue nuit indochinoise",
    [`Duration "1h56"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VOstFr, ["20:15"])])]);
 ("Le Royal Palace", "165 Grande-Rue-Charles-de-Gaulle, Nogent-sur-Marne",
  None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["13:45"; "15:45"; "17:45"; "19:45"; "21:45"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:00"; "20:00"; "22:00"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["14:00"; "17:30"; "20:45"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["19:30"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["17:00"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:45"; "19:30"]); (`VOstFr, ["17:00"; "21:45"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["14:15"; "16:45"; "19:30"])]);
   ("11.6", [`Duration "1h42"; `Public "Tous publics"; `Genre "Thriller"],
    [(`VF, ["21:45"])]);
   ("Perfect Mothers",
    [`Duration "1h51"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["21:45"])]);
   ("Op\195\169ra : la Traviata",
    [`Duration "2h15"; `Public "Tous publics"; `Genre "Musical"],
    [(`VOstFr, ["14:00"])])]);
 ("Studio 28", "10 Rue Tholoz\195\169, Paris",
  Some [("lat", 48.886142); ("lng", 2.335422)],
  [("No", [`Duration "1h57"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:00"])])]);
 ("Le Village", "4 rue de Chezy, Neuilly-sur-Seine", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:15"; "16:15"; "18:15"; "20:15"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:00"; "18:30"; "20:30"])])]);
 ("Le Capitole", "3 rue Ledru-Rollin, Suresnes", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["16:30"; "18:45"; "20:30"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:15"; "16:15"; "18:30"; "20:45"])]);
   ("Les Croods, 3D",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["18:30"])]);
   ("Des gens qui s'embrassent",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["16:30"; "18:15"; "20:45"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:45"; "16:00"; "20:30"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:15"])])]);
 ("Les Lumi\195\168res", "49 rue Maurice-Thorez, Nanterre", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:15"; "18:15"; "21:00"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:15"; "20:45"])]);
   ("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["13:30"; "18:30"]); (`VOstFr, ["16:00"; "21:00"])]);
   ("Le Temps de l'aventure",
    [`Duration "1h45"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["16:15"; "18:30"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["20:45"])]);
   ("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["21:00"])])]);
 ("Cin\195\169mas du palais", "40 all\195\169e Parmentier, Cr\195\169teil",
  None,
  [("The Grandmaster",
    [`Duration "2h2"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:45"; "20:00"])]);
   ("Promised Land",
    [`Duration "1h46"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["16:00"; "18:00"; "20:00"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:45"; "18:00"])]);
   ("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["13:45"; "17:45"])]);
   ("Inch'Allah", [`Duration "1h41"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["15:45"; "19:45"])]);
   ("Travail au noir",
    [`Duration "1h40"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VOstFr, ["13:45"])])]);
 ("Studio 66", "66 rue Jean-Jaur\195\168s, Champigny-sur-Marne", None,
  [("Les Profs",
    [`Duration "1h28"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:15"; "18:15"; "20:30"])]);
   ("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:00"; "16:15"; "18:15"; "20:30"])]);
   ("Oblivion",
    [`Duration "2h6"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["20:30"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["13:45"; "16:00"; "18:15"; "20:30"])]);
   ("Les Croods",
    [`Duration "1h32"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "16:15"; "18:15"; "20:30"])]);
   ("Tad l'explorateur : A la recherche de la Cit\195\169 Perdue",
    [`Duration "1h31"; `Public "Tous publics"; `Genre "Film d'animation"],
    [(`VF, ["14:00"; "16:15"; "18:15"])])]);
 ("Mac-Mahon", "5 av. Mac-Mahon, Paris", None,
  [("Dr\195\180le de frimousse, version restaur\195\169e",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Musical"],
    [(`VOstFr, ["14:00"; "16:15"; "18:30"; "20:30"])])]);
 ("L'Ecran - salle Armand Bad\195\169yan",
  "14 passage de l'Aqueduc, Saint-Denis", None,
  [("La Maison de la radio",
    [`Duration "1h43"; `Public "Tous publics"; `Genre "Documentaire"],
    [(`VF, ["14:15"; "16:30"; "18:45"; "20:45"])]);
   ("Wadjda", [`Duration "1h37"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["14:00"; "16:45"; "20:30"])]);
   ("Syngu\195\169 Sabour - Pierre de patience",
    [`Duration "1h42"; `Public "Tous publics"; `Genre "Drame"],
    [(`VOstFr, ["18:30"])])]);
 ("Abel Gance", "184 bd. Saint-Denis, Courbevoie", None,
  [("Les Amants passagers",
    [`Duration "1h30"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VOstFr, ["16:15"])]);
   ("L'Artiste et son mod\195\168le",
    [`Duration "1h45"; `Public "Tous publics"; `Genre "Drame"],
    [(`VF, ["12:00"])])]);
 ("Ariel - Centre-ville", "97-99 av. Paul-Doumer, Rueil-Malmaison", None,
  [("Les Gamins",
    [`Duration "1h35"; `Public "Tous publics"; `Genre "Com\195\169die"],
    [(`VF, ["14:15"; "16:15"; "18:15"; "21:00"])]);
   ("Les \195\130mes vagabondes",
    [`Duration "2h4"; `Public "Tous publics"; `Genre "Fantastique"],
    [(`VF, ["18:15"; "20:45"])]);
   ("Quartet",
    [`Duration "1h38"; `Public "Tous publics";
     `Genre "Com\195\169die dramatique"],
    [(`VF, ["13:50"; "18:20"]); (`VOstFr, ["16:00"; "20:30"])])])]
;;
