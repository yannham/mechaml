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

Here is sample of code that fetches a web page, fills a login form and submits it:

```ocaml
let agent =
  Agent.init ()
  |> Agent.get "https://www.mywebsite.com"
  |> Lwt_main.run in

let form =
  Agent.page agent
  |> (function
    | Some page -> Page.form_with "[name=loginForm]" page
    | None -> None)
  |> (function
    | Some form -> form
    | None -> failwith "unable to find the login form !")
  |> Page.Form.set "username" ["myusername"]
  |> Page.Form.set "password" ["mypassword"] in

Agent.submit form agent
|> Lwt_main.run
```

And there a function that visits the specified page, searches for all the JPG images and
downloads them in a temporary folder


```ocaml
let agent =
  Agent.init ()
  |> Agent.get "http://www.somewebsite.com/some/page"
  |> Lwt_main.run in

let page = match Agent.page agent with
  | Some page -> page
  | None -> failwith "couldn't open or parse the page as html" in

let image_filename src =
  let last_slash = match String.rindex src '/' with
    | exception Not_found -> 0
    | i -> i+1 in
  String.sub src last_slash (String.length src - last_slash) in

Page.images_with "[src$=.jpg]" page
|> List.map (fun img ->
  try
    let file = Page.Image.source img
      |> image_filename
      |> (^) "/tmp/" in
    Agent.save_image img file agent
  with _ -> Lwt.return ())
|> Lwt.join
|> Lwt_main.run
```

# license

GNU LGPL v3
