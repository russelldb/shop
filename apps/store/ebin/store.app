{application, store,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             store_app,
             store_sup,
	     store_db,
	     store_mnesia_db
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { store_app, []}},
  {env, []}
 ]}.
