nomina_resuelve_fc
=====

An OTP application

Compilar
-----

    $ cd nomina_resuelve_fc
	$ rebar3 compile

Probar gen_server
-----

    $ cd nomina_resuelve_fc
	$ rebar3 shell
	[Return]

	1> Jugadores = #{<<"jugadores">> =>
		[#{<<"bono">> => 25000,<<"equipo">> => <<"rojo">>,
		   <<"goles">> => 6,<<"nivel">> => <<"A">>,
		   <<"nombre">> => <<"Juan">>,
		   <<"sueldo">> => 50000,
		   <<"sueldo_completo">> => null},
		 #{<<"bono">> => 30000,<<"equipo">> => <<"azul">>,
		   <<"goles">> => 7,<<"nivel">> => <<"B">>,
		   <<"nombre">> => <<"Pedro">>,
		   <<"sueldo">> => 100000,
		   <<"sueldo_completo">> => null},
		 #{<<"bono">> => 30000,<<"equipo">> => <<"azul">>,
		   <<"goles">> => 16,<<"nivel">> => <<"C">>,
		   <<"nombre">> => <<"Martin">>,
		   <<"sueldo">> => 100000,
		   <<"sueldo_completo">> => null},
		 #{<<"bono">> => 10000,<<"equipo">> => <<"azul">>,
		   <<"goles">> => 19,<<"nivel">> => <<"Cuauh">>,
		   <<"nombre">> => <<"Luis">>,
		   <<"sueldo">> => 50000,
		   <<"sueldo_completo">> => null}]}.
	2>
	2> nomina_resuelve_fc_server:obtener_nomina(Jugadores).
	3>
	3> halt().

Probar API REST
-----

    $ cd nomina_resuelve_fc
	$ rebar3 shell
	[Return]

	1> 
	
    En otra Terminal
	
	$ cd nomina_resuelve_fc/datos
	$ ls
	
    Revisar (y si se desea, modificar) el archivo JSON
	
	$ vi jugadores.json
	
    Probar
	
	$ curl -v -i --ipv4 \
          -H "Content-Type: application/json" \
          --data @jugadores.json \
          'http://localhost:8080/api/nomina'
	
