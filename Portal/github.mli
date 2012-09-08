(* ************************************************************************** *)
(* Project: La Vie Est Un Jeu - Internal Portal                               *)
(* Description: GitHub API Module                                             *)
(* Author: db0 (db0company@gmail.com, http://db0.fr/)                         *)
(* Latest Version is on GitHub: http://goo.gl/nq7mj                           *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(* Requirements                                                               *)
(* ************************************************************************** *)

(* Set the user and the password on GitHub.                                   *)
val account : string -> string -> unit

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

val get_repos : ?usertype : usertype -> string -> repositories

(* ************************************************************************** *)
(* Get Issues                                                                 *)
(* ************************************************************************** *)

type assignee =
    {
      a_name   : string;
      a_avatar : string;
    }

type issue =
    {
      title     : string;
      issue_url : string;
      assignee  : assignee option;
    }

type issues =
    {
      user      : string;
      repo_name : string;
      html_url  : string;
      issues    : issue list;
    }

val get_issues : string -> string -> issues
val get_issues_from_repository : repository -> issues

type organization_issues =
    {
      o_name       : string;
      o_issues_url : string;
      o_issues     : (repository * issue) list;
    }

val get_issues_from_organization : string -> organization_issues
