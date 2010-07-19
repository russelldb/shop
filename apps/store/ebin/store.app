{application, store,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             store_app,
             store_sup,
	     store_db,
	     store_mnesia_db,
	     store_authenticator,
	     store_auth_event_handler,
	     store_event_manager
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { store_app, []}},
  {env, []}
 ]}.
