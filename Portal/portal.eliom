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

let new_service path = Eliom_service.service ~path:path ~get_params:unit ()

let main          = new_service []
let agenda        = new_service ["agenda"; ""]
let issues        = new_service ["issues"]
let github        = new_service ["github"]
let documents     = new_service ["documents"]
let emails        = new_service ["emails"]
let server        = new_service ["server"]
let group         = new_service ["group"]
let epitech       = new_service ["epitech"]
let faq           = new_service ["F.A.Q."]

let nolink = Tools.no_link ()

let pages =
  [(main,          ("Home", "", "home"));

   (nolink,        ("Get things done", "", "stats"));
   (agenda,        ("Calendar", "Weekly meetings, special events", "calendar"));
   (issues,        ("Tasks", "What are the very next actions?", "fire"));

   (nolink,        ("Communicate", "", "conversation"));
   (emails,        ("E-mails", "Let's talk together on our Google Groups",
		    "envelope"));
   (group,         ("Team", "All the team members and their info", "group"));

   (nolink,        ("Code", "", "embed_close"));
   (github,        ("GitHub", "Our repositories", "github"));
   (server,        ("The Server", "We use it to test and code", "cloud"));

   (nolink,        ("Wiki", "", "circle_info"));
   (documents,     ("Documents", "All the documentation in one place",
		    "folder_open"));
   (faq,           ("F.A.Q.", "All you need to know when you enter the group",
		    "life_preserver"));

   (nolink,        ("Epitech", "", "table"));
   (epitech,       ("LabEIP", "Tickets, Grades, Assessment",
		    "show_big_thumbnails"));
  ]

let get_page_infos srv =
  try (Some (List.assq srv pages))
  with _ -> None

let get_page_title srv =
  match get_page_infos srv with
    | Some (name, _, _) -> Some name
    | None              -> None

let get_page_title_anyway srv =
  match get_page_title srv with
    | Some name -> name
    | None      -> ""

let get_page_descr srv =
  match get_page_infos srv with
    | Some (_, descr, _) -> Some descr
    | None               -> None

let get_page_descr_anyway srv =
  match get_page_descr srv with
    | Some descr -> descr
    | None       -> ""

let get_page_icon srv =
  match get_page_infos srv with
    | Some (_, _, icon) -> Some icon
    | None              -> None

let get_page_icon_anyway srv =
  match get_page_icon srv with
    | Some icon -> icon
    | None      -> ""

(* ************************************************************************** *)
(* Tools                                                                      *)
(* ************************************************************************** *)

(* string -> string -> int -> int                                             *)
(* Compare the n first characters of two string, can raise an exception       *)
let ncompare s1 s2 n =
  if String.length s1 < n || String.length s2 < n
  then String.compare s1 s2
  else
    try String.compare (String.sub s1 0 n) (String.sub s2 0 n)
    with _ -> raise (Invalid_argument "ncompare")

(* mapi : (int -> 'a -> 'b) -> 'a list -> 'b list                             *)
(* Same as List.map, but the function is applied to the index of the element  *)
(* as first argument (counting from 0), and the element itself as second      *)
(* argument. Not tail-recursive.                                              *)
let mapi f l =
  let rec aux i f = function
    | [] -> []
    | a::l -> let r = f i a in r :: aux (i + 1) f l in
  aux 0 f l

let great_iframe url =
  iframe ~a:[a_src (Xml.uri_of_string url); a_class ["bigiframe"]] []

let display_error e =
  div ~a:[a_class ["alert"; "alert-error"]]
    [pcdata e]

let icon icon =
  img ~alt:icon ~a:[a_style "padding: 5px;"]
    ~src:(Xml.uri_of_string ("http://icons.db0.fr/g/green/" ^ icon ^ ".png"))
    ()

(* ************************************************************************** *)
(* Set GitHub Login/Password                                                  *)
(* ************************************************************************** *)

exception Invalid_github_config

(* /!\ This value can raise an exception that prevents the website to start   *)
let _ =
  let fail () = raise Invalid_github_config in
  try match List.hd (Eliom_config.get_config ()) with
    | Simplexmlparser.Element (name, list_attrib, content_list) ->
      if name = "github"
      then
        Github.connect
	  ~agent:"LaVieEstUnJeu-Portal"
          (List.assoc "login" list_attrib)
          (List.assoc "password" list_attrib)
      else fail ()
    | _ -> fail ()
  with _ -> fail ()

(* ************************************************************************** *)
(* Calendar Extra functions                                                   *)
(* ************************************************************************** *)

let calendar_id =
  ("o4tijjunbuucrf9d3p8ijk8s54@group.calendar.google.com", GCal.LightGreen)

let available_timezones =
  ["Europe/Paris";
   "Asia/Shanghai";
   "Europe/Helsinki";
   "Europe/Stockholm";
   "America/Los_Angeles";
   "America/New_York";
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
        [Html5.F.Raw.a ~a:[a_href (Xml.uri_of_string "#")] [pcdata tmz_str]] in
    let _ = Eliom_service.onload
      {{
        let replace_iframe _ =
          let container = To_dom.of_div %div_iframe
	  and _ = Firebug.console##log (Js.string "test")
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
(* Page Skeletton                                                             *)
(* ************************************************************************** *)

let skeletton
    ?page_title:(page_title=None)
    ?curr_service:(curr_service=main)
    body_content =
  let menu =
    let menu_li_elt (serv, (name, dsc, licon)) =
      let txt = [icon licon; pcdata " "; pcdata name; small [pcdata dsc]] in
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
          ([div ~a:[a_class ["main"; "row-fluid"]]
               [div ~a:[a_class ["menu"; "span2"]]
                   [menu];
		div ~a:[a_class ["page"; "span10"]]
                  body_content];
            Tools.script_url ["jquery.min.js"];
            Tools.script_url ["bootstrap.js"];
           ] @ (if curr_service == issues
	     then [Tools.script_url ["tasks.js"]] else []))))

(* ************************************************************************** *)
(* Service definition                                                         *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* Main                                                                       *)
(* ************************************************************************** *)

let _ =

  Example.register ~service:main
    (fun () () ->
      let right_icon url =
        img ~alt:("icon") ~a:[a_class ["right_icon"]]
          ~src:(Tools.sturi ["img"; url]) () in
      skeletton
        [
          img ~alt:("Life") ~a:[a_class ["cute_logo"]]
            ~src:(Tools.sturi ["img"; "Life_green.png"]) ();
          h2 [pcdata "Let's make everybody's lives a game!"];
          h4 ~a:[a_class ["subtitle"]]
            [pcdata "Welcome! This is Life Internal Portal."];
          small [pcdata ("It's centralizing all the tools the team "
			 ^ "is using so you don't get lost.")];
          div ~a:[a_class ["row-fluid"; "paraph"]]
            [div ~a:[a_class ["span3"; "well"; "well-small"]]
                [right_icon "calendar.png";
                 h4 [pcdata "1st step: ";
                     a ~service:agenda [pcdata "Calendar"] ()];
                 p [pcdata ("Check the upcoming meetings," ^
                               " deadlines and events.")]];
             div ~a:[a_class ["span3"; "well"; "well-small"]]
               [right_icon "gmail.png";
                h4 [pcdata "2nd step: ";
                    a ~service:emails [pcdata "E-mails"] ()];
                p [pcdata ("Read the latest e-mails to keep yourself up-to-date"
			   ^ " on the latest group's discussions and answer"
			   ^ " the questions asked to the team.")]];
             div ~a:[a_class ["span3"; "well"; "well-small"]]
               [right_icon "tasks.png";
                h4 [pcdata "3rd step: ";
                    a ~service:issues [pcdata "Tasks"] ()];
                p [pcdata ("Go check out the list of open tasks and pick yours." ^
                              " If you're not assigned to a task yet,"
			   ^ " go ahead and assign yourself to an existing one or"
			   ^ " feel free to create a new one.")]];
             div ~a:[a_class ["span3"; "well"; "well-small"]]
               [right_icon "productivity.png";
                h4 [pcdata "Last step: Get things done!"];  
		p [pcdata ("You're ready to work on the project!" ^
                              " If you have any question about a task," ^
                              " ask it on the task thread. You may also use" ^
			      " the mailing list.")]]
            ];
	  hr ();
	  h2 [pcdata "External links"];
	  div ~a:[a_class ["row-fluid"]]
	    [
	      div ~a:[a_class ["span4"; "well"]]
		[h3 [pcdata "Showcase website"];
		 p [pcdata ("Public portal for our community of beta-testers"
			    ^ " and potential investors.")];
		 Tools.external_link "http://life.db0.fr/"
		   [div ~a:[a_class ["btn"; "btn-success"]]
		       [pcdata "» Visit the website"]]];
	      div ~a:[a_class ["span4"; "well"]]
		[h3 [pcdata "1st version"];
		 p [pcdata ("The demo of the first version of the website."
			    ^ " Might not be running.")];
		 Tools.external_link "http://life.paysdu42.fr:2010"
		   [div ~a:[a_class ["btn"; "btn-success"]]
		       [pcdata "» Visit the website"]]];
	      div ~a:[a_class ["span4"; "well"]]
		[h3 [pcdata "API Web service"];
		 p [pcdata ("Demo of the web service."
			    ^ " Might not be running.")];
		 Tools.external_link "http://life.paysdu42.fr:2048"
		   [div ~a:[a_class ["btn"; "btn-success"]]
		       [pcdata "» Visit the website"]]];
	    ];
	  hr ();
	  h2 [pcdata "First time here?"];
	  p [pcdata "You might want to take the time to read a few ";
	     a ~service:documents [pcdata "documents"] ();
	     pcdata " before starting, read the ";
	     a ~service:faq [pcdata "F.A.Q"] ();
	     pcdata " and meet ";
	     a ~service:group [pcdata "the team"] ();
	     pcdata ".";
	    ];
	  p [pcdata "If you are a developer, you should also browse ";
	     a ~service:github [pcdata "our repositories"] ();
	     pcdata " and use ";
	     a ~service:server [pcdata "our development server"] ();
	     pcdata ".";
	  ];
        ])

(* ************************************************************************** *)
(* Calendar                                                                     *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:agenda
    (fun () () ->
      let calendars =
        let make_calendar tmz =
          (tmz, my_kind_of_calendar calendar_id GCal.Week tmz) in
        List.map make_calendar available_timezones in
      let div_iframe = div [List.assoc default_timezone calendars] in
      skeletton ~page_title:(get_page_title agenda) ~curr_service:agenda
        [h1 [pcdata "Calendar"];
	 h4 [pcdata "Add it in your own calendar"];
	 p [pcdata ("It is highly recommended, for your own organization and the"
		    ^ " cohesion of the team, to use a calendar and to " ^ 
		      " synchronize it with this one. You may also install" ^
		       " an app on your smartphone to get notifications.")];
	 p [pcdata "Use this ICal URL to add it on your calendar:"];
	 pre [pcdata (fst calendar_id)];
	 timezone_tabs calendars div_iframe; div_iframe])

(* ************************************************************************** *)
(* Issues                                                                     *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:issues
    (fun () () ->
      let display_summary o_issues =
	let display_line (repo, issues) =
	  let serv = Eliom_service.external_service
	    ~prefix:("#" ^ repo.Github.name) ~path:[] ~get_params:unit () in
	  li ~a:[a_style (match issues.Github.issues with
	    | [] -> "display: none" | _ -> "")]
	    [a serv [pcdata repo.Github.name] ()] in
	ul ~a:[a_class ["nav"; "nav-pills"]]
	  (List.map display_line o_issues) in
      let display_o_issues o_issues =
	let display_repo (repo, issues) =
	  let display_issue i issue =
	    let display_assignee = function
              | Some a ->
		[img ~alt:(a.Github.a_name) ~a:[a_class ["avatar"]]
		    ~src:(Xml.uri_of_string a.Github.a_avatar) ();
		 b [pcdata a.Github.a_name]]
              | None   ->
		[h3 ~a:[a_class ["badge"; "badge-important"]]
		    [pcdata "Nobody"]]
	    and display_labels =
	      let display_label label =
		span ~a:[a_class ["badge"];
			 a_style ("background-color: #"
				  ^ label.Github.label_color)]
		  [pcdata label.Github.label_name] in
	      List.map display_label in
            tr ~a:[a_style
                      (if (i mod 2) = 0
                       then "background-color: #F9F9F9;" else "")]
              [td [Tools.external_link repo.Github.url
                      [pcdata (repo.Github.name)]];
	       td (display_labels issue.Github.labels);
               td [pcdata (issue.Github.title)];
               td (display_assignee issue.Github.assignee);
               td [Tools.external_link issue.Github.issue_url
                      [div ~a:[a_class ["btn"; "btn-info"; "btn-large"]]
			  [pcdata "» Details of the task"]];
		  ];
              ] in
	  match issues.Github.issues with
	    | [] -> div []
	    | issues ->
	      let is_label label_name label =
		label.Github.label_name = label_name in
	      let is_bonus issue = List.exists (is_label "bonus") issue.Github.labels in
	      let (bonus, regular) = List.partition is_bonus issues
	      and table_header () =
		(tr [th [pcdata "Repository"];
		     th [pcdata "Labels"];
		     th [pcdata "Task name (issue)"];
		     th [pcdata "Assigned"];
		     th [pcdata "Link"]
		    ]) in
	      let serv = Eliom_service.external_service
		~prefix:("#") ~path:[] ~get_params:unit () in
	      div ~a:[a_id (repo.Github.name ^ "/")]
		[h2 [pcdata repo.Github.name];
		 table ~a:[a_class ["table"; "table-bordered"]]
		   (table_header ()) (mapi display_issue regular);
		 p [a ~a:[a_id repo.Github.name; a_class ["bonus_tasks"]]
		       ~service:serv [pcdata ("+ " ^ (string_of_int (List.length bonus))
					      ^ " bonus tasks")] ()];
		 table ~a:[a_id (repo.Github.name ^ "_bonus");
			   a_class ["table"; "table-bordered"];
			   a_style "display: none"]
		   (table_header ()) (mapi display_issue bonus);
		] in
        div [h1 [pcdata (get_page_title_anyway issues)];
	     display_summary o_issues.Github.o_issues;
	     p [pcdata ("All the tasks in the Internal_tools repository are not"
			^ " code-related. If you are not a developer, go check"
			^ " them out!")];
	     div (List.map display_repo o_issues.Github.o_issues)] in
      skeletton ~page_title:(get_page_title issues) ~curr_service:issues
	[match Github.get_issues_from_organization "LaVieEstUnJeu" with
	  | Github.Success o_issues -> display_o_issues o_issues
	  | Github.Error e -> display_error e])

(* ************************************************************************** *)
(* GitHub repositories                                                        *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:github
    (fun () () ->
      let display_repos repos =
	let display_repo repo =
          let horizontal_element (name, content) =
            div ~a:[a_class ["row-fluid"]]
              [div ~a:[a_class ["span6"]] [b [pcdata name]];
               div ~a:[a_class ["span6"]] [content]]
          and infos =
            [("Open tasks", 
              cdata (string_of_int repo.Github.nb_issues));
             ("Description", pcdata repo.Github.description);
             ("Last push", pcdata repo.Github.pushed_at);
             ("git clone", pre [pcdata repo.Github.git_url]);
            ] in
          div ~a:[a_class ["well"]]
            [h1 [pcdata (repo.Github.name)];
             div (List.map horizontal_element infos);
             pcdata " ";
             Tools.external_link repo.Github.url
               [div ~a:[a_class ["btn"; "btn-success"; "btn-large"]]
                   [pcdata "» Open repository"]];
            ] in
	div [h1 [pcdata (get_page_title_anyway github)];
             div (List.map display_repo (repos.Github.repos))] in
      skeletton ~page_title:(get_page_title github) ~curr_service:github
	[match Github.get_repos ~usertype:Github.Organization "LaVieEstUnJeu" with
	  | Github.Success repos -> display_repos repos
	  | Github.Error e -> display_error e])

(* ************************************************************************** *)
(* Documents                                                                  *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:documents
    (fun () () ->
      skeletton ~page_title:(get_page_title documents) ~curr_service:documents
	[match Github.get_readme "LaVieEstUnJeu" "Doc" with
	   | Github.Success readme ->
	     (Html5.F.unsafe_data readme : [> Html5_types.b ] Html5.F.elt)
	   | Github.Error e -> display_error e;
	]
    )

(* ************************************************************************** *)
(* E-mails mailing list Google Groups                                         *)
(* ************************************************************************** *)

let _ =
  let url = "https://groups.google.com/forum/embed/?place=forum/lavieestunjeu" ^
    "&showsearch=true&showpopout=true" ^
    "&parenturl=http://life.paysdu42.fr/emails" in
  Example.register ~service:emails
    (fun () () ->
      skeletton ~page_title:(get_page_title emails) ~curr_service:emails
        [great_iframe url])

(* ************************************************************************** *)
(* Server                                                                     *)
(* ************************************************************************** *)

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
         ("Price", [pcdata ("2€ per developer per month, " ^
                              "1 month free every 10 months")]);
         ("Login", [pre [pcdata "$> ssh login@life.paysdu42.fr"];
                        small [pcdata "\"login\" is your nickname"]]);
         ("Change your password", [pre [pcdata "$> passwd"]]);
         ("News about the server",
          [Tools.external_link url_server_thread
              [pcdata "Thread on the mailing-list"]]);
        ] in
      skeletton ~page_title:(get_page_title server) ~curr_service:server
        [div ~a:[a_class ["row-luid"]]
            [div ~a:[a_class ["span5"]]
                [h3 [pcdata "Informations about the server"];
                 dl ~a:[a_class ["dl-horizontal"]]
                   (List.map
                      (fun (key, value) ->
                        ((dt [pcdata key], []),
                         (dd value, [])))
                      infos)
                ];
             div ~a:[a_class ["span7"]]
               [h3 [pcdata "Financing the server"];
                great_iframe url]]])

(* ************************************************************************** *)
(* Group information                                                          *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:group
    (fun () () ->
      let url = "http://life.db0.fr/page/team.php?more" in
      skeletton ~page_title:(get_page_title group) ~curr_service:group
	[div ~a:[a_class ["alert"; "alert-info"]]
	    [pcdata "You can edit your information ";
	     Tools.external_link "http://goo.gl/WoCXL" [pcdata "on Google Drive"];
	     pcdata "."];
	 great_iframe url])

(* ************************************************************************** *)
(* F.A.Q.                                                                     *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:faq
    (fun () () ->
      let url = "https://docs.google.com/document/d/1q5Ou3VAwzpNi6IhZrHSvClYkSgCNgWv6WQhmY7SIjtQ/pub" in
      skeletton ~page_title:(get_page_title faq) ~curr_service:faq
        [great_iframe url])

(* ************************************************************************** *)
(* Epitech                                                                    *)
(* ************************************************************************** *)

let _ =
  Example.register ~service:epitech
    (fun () () ->
      let liurl txt url =
	li [Tools.external_link url [pcdata txt]] in
      skeletton ~page_title:(get_page_title epitech) ~curr_service:epitech
        [h3 [pcdata "Epitech & LabEIP"];
	 ul
	   [liurl "Grades, marks, follow-up"
	       "https://eip.epitech.eu/projects/view/464#wall";
	    liurl "Live Meeting"
	      "https://www.livemeeting.com/cc/ionis/join?id=MFWG42&role=present&pw=Aepiecae";
	    liurl "SVN Rendu Repository"
	      "https://labeip.epitech.eu/svn/2014/lavieestunjeu";
	    liurl "Tickets with the LabEIP"
	       "https://eip.epitech.eu/projects/view/464#discussions";
	   ]])

