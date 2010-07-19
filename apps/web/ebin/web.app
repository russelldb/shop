%%-*- mode: erlang -*-
{application, web,
 [
  {description, "web"},
  {vsn, "1"},
  {modules, [
             web,
             web_app,
             web_sup,
             web_resource
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { web_app, []}},
  {env, []}
 ]}.
