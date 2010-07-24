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
	     store_event_manager,
	     store_db_behaviour,
	     store_mnesia_authenticator,
	     store_authenticator_behaviour,
	     store_util
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { store_app, []}},
  {env, []}
 ]}.
