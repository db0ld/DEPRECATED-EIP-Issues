(* ************************************************************************** *)
(* Project: La Vie Est Un Jeu - Internal Portal                               *)
(* Description: Simple portal to have all our organization tools together     *)
(* Author: db0 (db0company@gmail.com, http://db0.fr/)                         *)
(* Latest Version is on GitHub: http://goo.gl/nq7mj                           *)
(* ************************************************************************** *)

{shared{
open Eliom_content
open Html5
open D
}}

open Eliom_parameter

(* ************************************************************************** *)
(* Application                                                                *)
(* ************************************************************************** *)

module Example =
  Eliom_registration.App
    (struct
      let application_name = "portal"
     end)

(* ************************************************************************** *)
(* Services declaration                                                       *)
(* ************************************************************************** *)

let main =
  Eliom_service.service
    ~path:[]
    ~get_params:unit
    ()

let agenda =
  Eliom_service.service
    ~path:["agenda"; ""]
    ~get_params:unit
    ()

let global_agenda =
  Eliom_service.service
    ~path:["agenda"; "global"]
    ~get_params:unit
    ()

let issues =
  Eliom_service.service
    ~path:["issues"]
    ~get_params:unit
    ()

let github =
  Eliom_service.service
    ~path:["github"]
    ~get_params:unit
    ()

let drive =
  Eliom_service.service
    ~path:["drive"]
    ~get_params:unit
    ()

let emails =
  Eliom_service.service
    ~path:["emails"]
    ~get_params:unit
    ()

let server =
  Eliom_service.service
    ~path:["server"]
    ~get_params:unit
    ()

let irc =
  Eliom_service.service
    ~path:["irc"]
    ~get_params:unit
    ()

let group =
  Eliom_service.service
    ~path:["group"]
    ~get_params:unit
    ()

let vitrine =
  Eliom_service.service
    ~path:["vitrine"]
    ~get_params:unit
    ()

let nolink = Tools.no_link ()

let pages =
  [(main,          ("Accueil", ""));
   (nolink,        ("Organisation", ""));
   (agenda,        ("Agenda", "Rendez-vous, événements, rendus"));
   (global_agenda, ("Agenda Global", "Répartition du travail sur l'année"));
   (nolink,        ("Code", ""));
   (issues,        ("Tâches en cours", "Issues Github ouvertes"));
   (github,        ("Dépôts GitHub", ""));
   (server,        ("Le serveur", "Documentation et trésorerie"));
   (nolink,        ("Communication", ""));
   (emails,        ("E-mails", "Le Google Groups"));
   (irc,           ("Channel de discussion IRC", ""));
   (nolink,        ("Divers", ""));
   (vitrine,       ("Site Vitrine", ""));
   (drive,         ("Google Documents", ""));
   (group,         ("Informations sur le groupe", ""));
  ]

let get_page_infos srv =
  try (Some (List.assq srv pages))
  with _ -> None

let get_page_title srv =
  match get_page_infos srv with
    | Some (name, _) -> Some name
    | None           -> None

let get_page_descr srv =
  match get_page_infos srv with
    | Some (_, descr) -> Some descr
    | None            -> None

(* ************************************************************************** *)
(* Calendar Extra functions                                                   *)
(* ************************************************************************** *)

let calendar_id =
  ("o4tijjunbuucrf9d3p8ijk8s54@group.calendar.google.com", GCal.LightGreen)

let global_calendar_id =
  ("u5u80vniilf4k480mqgiq5tl44@group.calendar.google.com", GCal.Green)

let available_timezones =
  ["Europe/Paris";
   "Asia/Shanghai";
   "Europe/Helsinki";
   "Europe/Stockholm";
   "America/Los_Angeles";
  ]

let default_timezone =
  List.hd available_timezones

(* iframe list -> div -> div                                                  *)
(* Take in parameter the list of available calendars and return smart tabs    *)
let timezone_tabs calendars div_iframe =
  let tmz_btn (tmz_str, cal) =
    let elt =
      let classs = if tmz_str = default_timezone then ["active"] else [] in
      li ~a:[a_id tmz_str; a_class classs]
	[a ~service:nolink [pcdata tmz_str] ()] in
    let _ = Eliom_service.onload
      {{
	let replace_iframe _ =
	  let container = To_dom.of_div %div_iframe
	  and to_replace = Tools.get_element_by_id "calendar"
	  and new_div = To_dom.of_iframe %cal
	  and tab = Tools.get_element_by_id %tmz_str in
	  (Dom.replaceChild container new_div to_replace);
	  List.iter (fun tmz ->
	    let tab = Tools.get_element_by_id tmz in
	    tab##className <- Js.string "") %available_timezones;
	  tab##className <- Js.string "active"; () in
	let open Event_arrows in
	    ignore (run (clicks (To_dom.of_li %elt)
			   (arr replace_iframe)) ()); ()
      }} in
    elt in
  (div
     [ul ~a:[a_class ["nav"; "nav-tabs"]]
	 (List.map tmz_btn calendars)])

let my_kind_of_calendar calendar_id style tmz =
  GCal.calendar
    ~show_title:false
    ~show_print:false
    ~show_calendars:false
    ~show_timezone:false
    ~display_mode:style
    ~timezone:(Some tmz)
    ~week_start_on:GCal.Monday
    ~height:800
    ~width:(GCal.Percent 100)
    [calendar_id]

(* ************************************************************************** *)
(* Tools                                                                      *)
(* ************************************************************************** *)

let great_iframe url =
  iframe ~a:[a_src (Xml.uri_of_string url); a_class ["bigiframe"]] []

(* ************************************************************************** *)
(* Page Skeletton                                                             *)
(* ************************************************************************** *)

let skeletton
    ?page_title:(page_title=None)
    ?curr_service:(curr_service=main)
    body_content =
  let menu =
    let menu_li_elt (serv, (name, dsc)) =
      let txt = [pcdata name; small [pcdata dsc]] in
      let link = a ~service:(serv) txt () in
      if serv == nolink
      then li ~a:[a_class ["nav-header"]] txt
      else if serv == curr_service
      then li ~a:[a_class ["active"]] [link]
      else li [link] in
    ul ~a:[a_class ["nav"; "nav-tabs"; "nav-pills"; "nav-stacked"]]
      (List.map menu_li_elt pages)
  and page_title =
    let base_title = "La Vie Est Un Jeu :: Internal Portal" in
    match page_title with
      | Some str -> base_title ^ " :: " ^ str
      | None    -> base_title
  and css_list =
    let css_files = [["bootstrap.min.css"];
		     ["style.css"];
		    ] in
    List.map Tools.css css_files in
  Lwt.return
    (html
       (head (title (pcdata page_title)) css_list)
       (body
	  [div ~a:[a_class ["main"; "row-fluid"]]
	      [div ~a:[a_class ["menu"; "span2"]]
		  [menu];
	       div ~a:[a_class ["page"; "span10"]]
		 body_content];
	   Tools.script_url ["bootstrap.js"]
	  ]))

(* ************************************************************************** *)
(* Service definition                                                         *)
(* ************************************************************************** *)

let _ =

  Example.register ~service:main
    (fun () () ->
      let right_icon url =
	img ~alt:("icon") ~a:[a_class ["right_icon"]]
	  ~src:(Tools.sturi ["img"; url]) () in
      skeletton
	[
	  img ~alt:("La Vie Est Un Jeu") ~a:[a_class ["cute_logo"]]
	    ~src:(Tools.sturi ["img"; "cuteface_small.png"]) ();
	  h2 [pcdata "Bonjour le groupe d'EIP le plus awesome de l'univers !"];
	  h4 ~a:[a_class ["subtitle"]]
	    [pcdata "Bienvenue sur notre portail."];
	  p [pcdata ("Ce portail a pour but de regrouper tous nos outils afin" ^
			" de vous aider au quotidien. Il est donc fortement" ^
			" conseillé de s'y rendre tous les jours.")];
	  div ~a:[a_class ["row-fluid"; "paraph"]]
	    [div ~a:[a_class ["span3"; "well"; "well-small"]]
		[right_icon "calendar.png";
		  h4 [pcdata "1ère étape : ";
		     a ~service:agenda [pcdata "L'agenda"] ()];
		 p [pcdata ("Vérifiez les rendez-vous EIP du jour et les" ^
			       " prochaines dead-line.")]];
	     div ~a:[a_class ["span3"; "well"; "well-small"]]
	       [right_icon "gmail.png";
		h4 [pcdata "2ème étape : ";
	     a ~service:emails [pcdata "Les e-mails"] ()];
	  p [pcdata ("Lisez vos e-mails afin de vous tenir au courant de" ^
			" toutes les discussions en cours et de répondre aux" ^
			" éventuelles questions.")]];
	     div ~a:[a_class ["span3"; "well"; "well-small"]]
	       [right_icon "tasks.png";
		h4 [pcdata "3ème étape : ";
		    a ~service:issues [pcdata "Les tâches en cours"] ()];
		p [pcdata ("Allez voir vos tâches en cours dans la liste des" ^
			      " issues GitHub. Si vous n'en avez pas en" ^
			      " cours, assignez-vous aux tâches qui n'ont" ^
			      " pas encore d'assigné.")]];
	     div ~a:[a_class ["span3"; "well"; "well-small"]]
	       [right_icon "code.png";
		h4 [pcdata "4ème étape : Codez !"];
		p [pcdata ("Vous êtes prêt à coder pour l'EIP ! Si vous" ^
			      " avez la moindre question, utilisez l'issue" ^
			      " sur laquelle vous êtes, la mailing-list ou" ^
			      " le channel irc.")]]
	    ]
	])

let _ =
  Example.register ~service:agenda
    (fun () () ->
      let calendars =
	let make_calendar tmz =
	  (tmz, my_kind_of_calendar calendar_id GCal.Week tmz) in
	List.map make_calendar available_timezones in
      let div_iframe = div [List.assoc default_timezone calendars] in
      skeletton ~page_title:(get_page_title agenda) ~curr_service:agenda
	[timezone_tabs calendars div_iframe; div_iframe])

let _ =
  Example.register ~service:global_agenda
    (fun () () ->
      let calendars =
	let make_calendar tmz =
	  (tmz, my_kind_of_calendar global_calendar_id GCal.Month tmz) in
	List.map make_calendar available_timezones in
      let div_iframe = div [List.assoc default_timezone calendars] in
      skeletton
	~page_title:(get_page_title global_agenda)
	~curr_service:global_agenda
	[timezone_tabs calendars div_iframe; div_iframe])

let _ =
  let url = "https://github.com/organizations/LaVieEstUnJeu/dashboard/" ^
    "issues/repos?direction=asc&page=1&sort=created&state=open" in
  Example.register ~service:issues
    (fun () () ->
      skeletton ~page_title:(get_page_title issues) ~curr_service:issues
	[great_iframe url])

let _ =
  Example.register ~service:github
    (fun () () ->
      skeletton ~page_title:(get_page_title github) ~curr_service:github
	[])

let _ =
  let url = "https://drive.google.com/#folders/" ^
    "0Bw8n0yHMUHF-NlRFUk15N2hTUC1lRkVrakhzYWdIdw&embedded=true" in
  Example.register ~service:drive
    (fun () () ->
      skeletton ~page_title:(get_page_title drive) ~curr_service:drive
	[great_iframe url])

let _ =
  let url = "https://groups.google.com/forum/embed/?place=forum/lavieestunjeu" ^
    "&showsearch=true&showpopout=true" ^
    "&parenturl=http://life.paysdu42.fr/emails" in
  Example.register ~service:emails
    (fun () () ->
      skeletton ~page_title:(get_page_title emails) ~curr_service:emails
	[great_iframe url])

let _ =
  Example.register ~service:server
    (fun () () ->
      let url = "https://docs.google.com/spreadsheet/pub" ^
	"?key=0Ag8n0yHMUHF-dGRTUXk4bVl3TFVxTXMzdElIWDJxa2c" ^
	"&output=html&widget=true"
      and url_server_thread ="https://groups.google.com/forum/" ^
	"#!topic/lavieestunjeu/2V8tSOiIETE/discussion" in
      let infos =
	[("Distribution", [pcdata "Ubuntu Server 12.04 LTS en 64 bits"]);
	 ("Hostname", [pcdata "gangbang"]);
	 ("I.P.", [pcdata "88.191.147.207"]);
	 ("Reverse", [pcdata "life.paysdu42.fr"]);
	 ("Prix", [pcdata ("2€ par personne par mois, 1 mois gratuit tous" ^
			      "les 10 mois")]);
	 ("Se loguer", [pre [pcdata "$> ssh login@life.paysdu42.fr"];
			small [pcdata "\"login\" est votre pseudo"]]);
	 ("Changer de pass", [pre [pcdata "$> passwd"]]);
	 ("Toutes les news à propos du serveur",
	  [Tools.external_link url_server_thread
	      [pcdata "Fil de discussion sur la mailing-list"]]);
	 ("IRC", [pcdata "Si vous voulez être sur le chan irc tout le temps :";
		  pre [pcdata "$> screen irssi 
/server irc.rezosup.org 
/join #life-eip "];
		  pcdata "Pour sortir :";
		  pre [pcdata "Ctrl + A + D"];
		  pcdata "Pour revenir plus tard :";
		  pre [pcdata "screen -rd"]]);
	] in
      skeletton ~page_title:(get_page_title server) ~curr_service:server
	[div ~a:[a_class ["row-luid"]]
	    [div ~a:[a_class ["span5"]]
		[h3 [pcdata "Informations sur le serveur"];
		 dl ~a:[a_class ["dl-horizontal"]]
		   (List.map
		      (fun (key, value) ->
			((dt [pcdata key], []),
			 (dd value, [])))
		      infos)
		   ];
	     div ~a:[a_class ["span7"]]
	       [h3 [pcdata "Trésorerie"];
		great_iframe url]]])

let _ =
  Example.register ~service:irc
    (fun () () ->
      let center_icon url =
	img ~alt:("icon") ~a:[a_class ["center_icon"]]
	  ~src:(Tools.sturi ["img"; url]) () in
      let irc_serv = "irc.rezosup.org"
      and irc_chan = "life-eip" in
      let url = "http://widget.mibbit.com/" ^
	"?server=" ^ irc_serv ^
	"&channel=%23" ^ irc_chan in
      skeletton ~page_title:(get_page_title irc) ~curr_service:irc
	[h3 [pcdata "Notre channel irc "; code [pcdata ("#" ^ irc_chan)];
	     pcdata " sur "; code [pcdata irc_serv]];
	 p [pcdata "Pour le rejoindre, 3 solutions :"];
	 div ~a:[a_class ["row-fluid"]]
	   [div ~a:[a_class ["span3"]]
	       [div ~a:[a_class ["well"; "irc"]]
		   [Tools.external_link
		       ("irc://" ^ irc_serv ^ "/" ^ irc_chan)
		       [center_icon "mirc.png"; br (); br ();
			 pcdata "Utiliser un client lourd"]];
		div ~a:[a_class ["well"; "irc"]]
		  [a ~service:server
		      [center_icon "irssi.png"; br (); br ();
		       pcdata "Utiliser irssi sur le serveur"] ()];
		div ~a:[a_class ["well"; "irc"]]
		  [center_icon "mibbit.png"; br (); br ();
		   pcdata "Utiliser le widget ci-contre"]];
	    div ~a:[a_class ["span9"]]
	      [great_iframe url]]])

let _ =
  Example.register ~service:group
    (fun () () ->
      let url_infos = "https://docs.google.com/spreadsheet/pub" ^
	"?key=0Ag8n0yHMUHF-dDJPS1RuLUdYUlc1WFYwMUlRaGJ0X0E" ^
	"&single=true&gid=0&output=html&widget=true"
      and url_teams = "https://docs.google.com/document/pub" ^
	"?id=14ShvKV0krhqvmSaXshM6TBPYk8MOxPLEMHXp0woqGr4" ^
	"&embedded=true" in
      skeletton ~page_title:(get_page_title group) ~curr_service:group
	[iframe ~a:[a_src (Xml.uri_of_string url_infos);
		    a_style "height: 300px;"] [];
	 iframe ~a:[a_src (Xml.uri_of_string url_teams);
		    a_style "height: 2500px;"] []])

let _ =
  Example.register ~service:vitrine
    (fun () () ->
      skeletton ~page_title:(get_page_title vitrine) ~curr_service:vitrine
	[great_iframe "http://eip.epitech.eu/2014/lavieestunjeu/"])
