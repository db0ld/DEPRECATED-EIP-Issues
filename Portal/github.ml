(* ************************************************************************** *)
(* Project: La Vie Est Un Jeu - Internal Portal                               *)
(* Description: GitHub API Module                                             *)
(* Author: db0 (db0company@gmail.com, http://db0.fr/)                         *)
(* Latest Version is on GitHub: http://goo.gl/nq7mj                           *)
(* ************************************************************************** *)

open Yojson.Basic.Util

(* ************************************************************************** *)
(* Requirements                                                               *)
(* ************************************************************************** *)

let userpwd = ref ""

(* string -> string -> unit                                                   *)
(* Set the user and the password on GitHub.                                   *)
let account username password =
  userpwd := username ^ ":" ^ password

(* ************************************************************************** *)
(* Curl Get Page                                                              *)
(* ************************************************************************** *)

(* string -> string                                                           *)
(* Return a text from a url using Curl and HTTP Auth                          *)
(* You must call "account" before calling this function                       *)
(* This function can raise an exception on failure                            *)
let get_text_form_url url =
  let writer accum data =
    Buffer.add_string accum data;
    String.length data in
  let result = Buffer.create 4096
  and errorBuffer = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let text =
    try
      (let connection = Curl.init () in
       Curl.set_errorbuffer connection errorBuffer;
       Curl.set_writefunction connection (writer result);
       Curl.set_followlocation connection true;
       Curl.set_url connection url;
       
       Curl.set_httpauth connection [Curl.CURLAUTH_BASIC];
       Curl.set_userpwd connection (!userpwd);
       
       Curl.perform connection;
       Curl.cleanup connection;
       Buffer.contents result)
    with
      | Curl.CurlException (_, _, _) ->
	raise (Failure ("Error: " ^ !errorBuffer))
      | Failure s -> raise (Failure s) in
  let _ = Curl.global_cleanup () in
  text

(* string -> json                                                             *)
(* Take a url, get the page and return a json tree                            *)
let curljson url =
  let result = get_text_form_url url in
  Yojson.Basic.from_string result

(* ************************************************************************** *)
(* Get repositories                                                           *)
(* ************************************************************************** *)

type usertype = User | Organization

type repository =
    {
      owner       : string;
      name        : string;
      description : string;
      pushed_at   : string;
      git_url     : string;
      nb_issues   : int;
      url         : string;
      issues_url  : string;
    }

type repositories =
    {
      user_type : usertype;
      user_name : string;
      repos     : repository list;
    }

let usertype_tostring = function
  | User         -> "users"
  | Organization -> "orgs"

(* ?usertype -> string -> repositories                                        *)
let get_repos ?(usertype = User) user =
  let url =
    "https://api.github.com/" ^ (usertype_tostring usertype) ^ "/" ^
      user ^ "/repos?sort=updated&" in
  let tree = curljson url in
  let repo tree =
    let name = tree |> member "name" |> to_string in
    {
      owner       = user;
      name        = name;
      description = tree |> member "description" |> to_string;
      pushed_at   = tree |> member "pushed_at"   |> to_string;
      git_url     = tree |> member "git_url"     |> to_string;
      nb_issues   = tree |> member "open_issues" |> to_int;
      url         = tree |> member "html_url"    |> to_string;
      issues_url  =
	"https://github.com/" ^ user ^ "/" ^ name ^
	  "/issues?sort=created&state=open";

    } in
  let repos = List.map repo (tree |> to_list) in
  {
    user_type = usertype;
    user_name = user;
    repos     = repos;
  }

(* ************************************************************************** *)
(* Get Issues                                                                 *)
(* ************************************************************************** *)

type assignee =
    {
      a_name   : string;
      a_avatar : string;
    }

type label =
    {
      label_url   : string;
      label_name  : string;
      label_color : string;
    }

type issue =
    {
      title     : string;
      issue_url : string;
      assignee  : assignee option;
      labels    : label list;
    }

type issues =
    {
      user      : string;
      repo_name : string;
      html_url  : string;
      issues    : issue list;
    }

(* string -> string -> issues                                                 *)
let get_issues user repo_name =
  let url =
    "https://api.github.com/repos/" ^ user ^ "/" ^
      repo_name ^ "/issues?state=open&sort=created&direction=asc" in
  let tree = curljson url in
  let issue tree =
    let get_assignee tree =
      match tree |> member "assignee" with
	| `Null  -> None
	| _ as a -> Some
	  {
	    a_name   = a |> member "login"      |> to_string;
	    a_avatar = a |> member "avatar_url" |> to_string;
	  }
    and get_labels =
      let to_label label =
	{
	  label_url   = label |> member "url"   |> to_string;
	  label_name  = label |> member "name"  |> to_string;
	  label_color = label |> member "color" |> to_string;
	} in
      List.map to_label in
    {
      title           = tree |> member "title"    |> to_string;
      issue_url       = tree |> member "html_url" |> to_string;
      assignee        = get_assignee tree;
      labels          = get_labels (tree |> member "labels" |> to_list);
    } in
  let issues = List.map issue (tree |> to_list) in
  {
    user      = user;
    repo_name = repo_name;
    html_url  =
      "https://github.com/" ^ user ^ "/" ^ repo_name ^
	"/issues?sort=created&state=open";
    issues    = issues;
  }

(* repository -> issues                                                       *)
let get_issues_from_repository repo =
  get_issues repo.owner repo.name

type organization_issues =
    {
      o_name       : string;
      o_issues_url : string;
      o_issues     : (repository * issue) list;
    }

(* string -> organization_issues                                              *)
let get_issues_from_organization org =
  let repos = get_repos ~usertype:Organization org in
  let aux_ifr repo = (get_issues_from_repository repo).issues in
  let aux_pair repo issue = (repo, issue) in
  let aux_issue_repos repo = List.map (aux_pair repo) (aux_ifr repo) in
  {
    o_name       = org;
    o_issues_url =
      "https://github.com/organizations/" ^ org ^
	"/dashboard/issues/repos?direction=asc&sort=created&state=open";
    o_issues     = List.flatten (List.map aux_issue_repos repos.repos)
  }
