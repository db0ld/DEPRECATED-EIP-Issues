(* ************************************************************************** *)
(* Project: La Vie Est Un Jeu - Public API, fullfill db with Caml through API *)
(* Description: fullfill for tests                                            *)
(* Author: JW                                                                 *)
(* Latest Version on GitHub: https://github.com/LaVieEstUnJeu/Internal-tools  *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* ocamlc api.cma fullfill.ml -g -o test                                      *)
(* OCAMLRUNPARAM=b ./test                                                     *)
(*                                                                            *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* Library configuration from example.ml                                      *)
(* ************************************************************************** *)

let _ =
  let open ApiConf in
      verbose := true;
      base_url := "life.paysdu42.fr:2048/api/v1";
      set_all_output "output.txt"

(* ************************************************************************** *)
(* Tests configuration from example.ml                                        *)
(* ************************************************************************** *)

let print_success = true
let stop_on_error = false

(* ************************************************************************** *)
(* Tools from example.ml                                                      *)
(* ************************************************************************** *)

(* Generate a random string with the given lenght                             *)
let random_string (length : int) : string =
  let _ = Random.self_init () in
  let gen () =
    match Random.int (26 + 26 + 10) with
      | n when n < 26      -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n                  -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
    String.concat "" (Array.to_list (Array.init length gen))

(* Display the message that explains the test                                 *)
let print_title str =
  ApiDump.lprint_endline ("\n\n\n## " ^ str)

(* Function to display a list (give it to the test function)                  *)
let listprint res = ApiDump.list res ApiDump.print

(* Calculate the number of success/failure to display the result at the end   *)
type result = { mutable success: int; mutable failure: int; }
let total = { success = 0; failure = 0; }

(* Print the total at the end                                                 *)
let print_total () =
  let t = string_of_int (total.success + total.failure) in
  ApiDump.lprint_endline ("
####################################################

                      T O T A L

  Success    : " ^ (string_of_int total.success) ^ " / " ^ t ^ "
  Failure    : " ^ (string_of_int total.failure) ^ " / " ^ t ^ "

####################################################

")

(* ************************************************************************** *)
(* Various data used by the program                                           *)
(* ************************************************************************** *)

let auth = ref (ApiTypes.Error ApiError.generic)

(* ************************************************************************** *)
(* Data used by the program to add users                                      *)
(* ************************************************************************** *)

and logins = "XJW"::"db0"::"rFlex"::"Nox"::"Tuxkowo"::"NicoLOL"::"Natas"::"Ironzorg"::"Rexou"::"Eiael"::[]
and lang = ApiTypes.Lang.default
and firstnames = (Some "David")::(Some "Barbara")::(Some "Simon")::(Some "Francois")::(Some "Guillaume")::(Some "Nicolas")::(Some "
GuillaumeX")::(Some "Frank")::(Some "Wilfried")::(Some "Youssef")::[]
and lastnames = (Some "Lassagne")::(Some "Lepage")::(Some "Corsin")::(Some "Glorieux")::(Some "Louvigny")::(Some "Klarman")::(Some "Caradec")::(Some "Lenormand")::(Some "Le-cormachin")::(Some "El-outmani")::[]
and genders = (Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Female)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Female)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Male)::(Some ApiTypes.Gender.Female)::[]
and birthday = ApiTypes.Date.of_string "1990-02-29"
and password = "HelloGlife"
and someone_else = "E.T."

(* ************************************************************************** *)
(* Data used by the program to add achievements                               *)
(* ************************************************************************** *)

and names = "20000 lieues sous les mers"::"Le riz yabon"::"I believe I can fly"::"I am your father !"::"All your base are belong to us"::"Welcome to life, glife !"::"Pas de bras, pas de chocolat"::"Fuuuuuuuuuuuudge !"::"Sous les sunlight des tropiques"::"HeadShot!"::[]
and descriptions = (Some "Avoir fait l'amour avec un cetace")::(Some "Avoir visite la Chine")::(Some "Avoir saute en parachute")::(Some "Avoir vu 14 fois la saga Star Wars")::(Some "Avoir termine Zero Wing")::(Some "Avoir cree son compte glife")::(Some "Avoir mange son poid en chocolat")::(Some "Avoir mange assez de fudge pour en vomir")::(Some "Avoir chante du Gilbert Montagne en fermant les yeux, avec des lunettes noires et en dodelinant de la tete")::(Some "Avoir tue une personne d'une balle dans la tete")::[]

(* ************************************************************************** *)
(* Test generic function from example.ml                                      *)
(* ************************************************************************** *)

let test
    ?(f = ApiDump.print) (* function to display the result of the test        *)
    ?(t = false)         (* true if the test should fail (so it's a success)  *)
    (result : 'a Api.t) : unit =
  let _failure () = total.failure <- total.failure + 1
  and _success () = total.success <- total.success + 1 in
  let failure () = if t then _success () else _failure ()
  and success () = if t then _failure () else _success () in
  let on_error e =
    begin
      failure ();
      ApiDump.error e;
      ApiDump.lprint_endline "\n  ----> FAILURE\n";
      if stop_on_error then exit 1
    end
  and on_result r =
    begin
      success ();
      if print_success
      then ApiDump.lprint_endline "\n  ## OCaml object generated:\n";
      f r;
      ApiDump.lprint_endline "\n  ----> SUCCESS\n"
    end in
  match result with
    | ApiTypes.Error  e -> on_error e
    | ApiTypes.Result r -> on_result r

(* ************************************************************************** *)
(* Inputs                                                                     *)
(* ************************************************************************** *)

let rec users_input lgs fstn lstn gdrs =
  match lgs with
    | head :: tail ->
      test (ApiUser.create
	      ~login:(List.hd lgs)
	      ~email:(random_string 5 ^ "." ^ random_string 8 ^ "@gmail.com")
	      ~password:password
	      ~lang:lang
	      ~firstname:(List.hd fstn)
	      ~lastname:(List.hd lstn)
	      ~gender:(List.hd gdrs)
	      ~birthday:(Some birthday)
	      ());

      users_input (List.tl lgs) (List.tl fstn) (List.tl lstn) (List.tl gdrs)
    | [] -> print_title "All users created !"


let rec achievements_input_asJW auth anames adescs =
  let main_auth = match auth with
    | ApiTypes.Error e -> ApiTypes.Curl ((List.hd logins), password)
    | ApiTypes.Result main_auth -> ApiTypes.Token main_auth.ApiAuth.token in
  begin
    match anames with
      | head :: tail ->
	test (ApiAchievement.post
		~auth:main_auth
		~name:(List.hd anames)
		~description:(List.hd adescs)
		());
	
	achievements_input_asJW auth (List.tl anames) (List.tl adescs)
      | [] -> print_title "All achievements created !"
  end

let rec achievements_status_input auth anames usrlog =
  match anames with
    | head :: tail ->
      test (ApiAchievementStatus.add
	      ~auth:auth
	      ~achievement:(List.hd anames)
	      ~state_code:(Random.int 2)
	      ~message:"Ananas"
	      usrlog);
      achievements_status_input auth (List.tl anames) usrlog
    | [] -> print_title ("Achievement status added for " ^ usrlog)
      

let rec achievements_foreach_user auth usrlogins =
  let main_auth = match auth with
    | ApiTypes.Error e -> ApiTypes.Curl ((List.hd usrlogins), password)
    | ApiTypes.Result main_auth -> ApiTypes.Token main_auth.ApiAuth.token in
  begin
    match usrlogins with
      | head :: tail ->
	achievements_status_input main_auth names (List.hd usrlogins);
	achievements_foreach_user auth (List.tl usrlogins)
      | [] -> print_title "All users have new achievement status !"
  end
let _ =

  print_title "Creating new users";
  users_input logins firstnames lastnames genders;

  print_title "Get authentication tokens using JW user";
  test (let _auth = ApiAuth.login (List.hd logins) password in
	auth := _auth; _auth);

  print_title "Creating new achievements";
  achievements_input_asJW !auth names descriptions;

  print_title "Generating achievement status for each user";
  achievements_foreach_user !auth logins;

  print_total ()
