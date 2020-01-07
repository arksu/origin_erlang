{application, a1,
    [{description, "a1 server" },
        {vsn, "1.0" },
        {modules, [
                db_server, 
                login_server,
                game_server, 
                map_server,
                player_server,
                session_server,
                objects_server,
                a1_sup
        ]},
        {registered,[a1_sup, 
                login_server, 
                game_server, 
                player_server, 
                tick_server, 
                map_server,
                objects_server]},
        {applications, [kernel,stdlib]},
        {mod, {a1_app,[]}},
        {start_phases, []}
    ]}.