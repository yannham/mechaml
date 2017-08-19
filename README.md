# Mechaml [![Build Status](https://travis-ci.org/yannham/mechaml.svg?branch=master)](https://travis-ci.org/yannham/mechaml)

## Description

Mechaml is a simple web scraping library that allows to :
* Fetch web content
* Analyze, fill and submit HTML forms
* Handle cookies, headers and redirections
* Use a web proxy **(soon to be implemented)**

Mechaml is built on top of existing libraries that alreay provide most of the
interesting features : [Cohttp](https://github.com/mirage/ocaml-cohttp) and
[Lwt](https://github.com/ocsigen/lwt) for asynchronous I/O and HTTP handling, and
[Lambdasoup](https://github.com/aantron/lambda-soup) to parse HTML. It provides
an interface that handles the interactions between these and add a few
other features.

## Overview

The library is divided into 3 main modules :
* Agent : User-agent features. Perform requests, get back content, headers, status code, etc...
* Cookiejar : Cookies handling
* Page : HTML and forms parsing & handling

For more details, see the [documentation](https://yannham.github.io/mechaml/)

## Usage

Here is sample of code that fetches a web page, fills a login form and submits it in the monadic style:

```ocaml
open Mechaml
module M = Agent.Monad
open M.Infix

let require msg = function
  | Some a -> a
  | None -> failwith msg

let action_login =
  Agent.get "http://www.somewebsite.com"
  >|= Agent.HttpResponse.page
  >|= function page -> (
    page
    |> Page.form_with "[name=login]"
    |> require "Can't find the login form !"
    |> Page.Form.set "username" "mynick"
    |> Page.Form.set "password" "@xlz43")
  >>= Agent.submit

let _ =
  M.run (Agent.init ()) action_login
```

More examples are available in the dedicated folder.

# license

GNU LGPL v3
