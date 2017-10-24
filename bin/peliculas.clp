;; AUTOR: Adrián Valero Gimeno
;;Sistema experto de recomendación de películas
;;Dos templates principales: Peli y Usuario
;;Peli y sus extend: Parte más estática. Info basada en hechos y inferible
;;Usuario: Parte variable. Se pueden modificar los hechos dependiendo de que tipo de pelis se quieran recomendar


;;;;;;;;; TEMPLATES ;;;;;;;;;;
(reset)

(deftemplate Peli
    (slot nombre) 
    (slot genero (allowed-values nil animacion comedia crimen accion drama terror ciencia-ficcion thriller fantasia misterio documental historico))
    (slot duracion (allowed-values corta media larga))
    (slot idioma (allowed-values nil español ingles frances italiano) (default ingles))
    (slot candidata (default no) (allowed-values no si aceptada validada rechazada))
    (slot tema)
    (slot descripcion (default nil))
    (slot edad (type integer)))


(deftemplate Usuario
    (slot nombre)
    (slot edad (type integer)))

(deftemplate Gustos extends Usuario
    (slot idioma)
    (slot duracion-preferida (allowed-values corta media larga))
    (slot test)
    (slot no-duracion (allowed-values corta media larga))
    (multislot no-idioma)
    )

(deftemplate Pelivista extends Peli
    )


(do-backward-chaining Peli)
(do-backward-chaining Usuario)


;;;;;;;;; BASES DE HECHOS ;;;;;;;;;;;;

(deffacts peliculas ;;Equivalente a BD de peliculas
    (Peli (nombre "Baby Driver") (duracion media) (tema coches) (edad 16))
    (Peli (nombre "The Big Lebowski") (genero comedia) (duracion larga) (edad 16))
    (Peli (nombre "American Beauty") (genero drama) (duracion larga) (tema familia) (edad 16))
    (Peli (nombre "Snatch") (duracion media) (tema crimen) (edad 18))
    (Peli (nombre "Nueve Reinas") (duracion media) (tema crimen) (idioma español) (edad 13))
    (Peli (nombre "The Big Short (La Gran Apuesta)") (genero drama) (duracion larga) (tema economia) (edad 16))
    (Peli (nombre "Arrival (La Llegada)") (duracion media) (tema aliens) (edad 16))
    (Peli (nombre "Kill Bill vol.1") (descripcion violenta))
    (Peli (nombre "It") (genero terror) (edad 18))
    (Peli (nombre "La bella y la bestia") (genero animacion))
    )

(deffacts pelis-vistas ;;Pelis vistas por el usuario, como el las describiria
    (Pelivista (descripcion graciosa))
    (Pelivista (tema carcel))
    (Pelivista (tema robots))
    (Pelivista (descripcion triste))
    )



(deffacts user
    (Gustos (nombre Pepito) (idioma ingles) (duracion-preferida larga))
)


 
;;;;;;;;;; REGLAS ;;;;;;;;;;;


(defrule pregunta-duracion
    (declare (salience -1))
    (exists (need-Usuario (nombre ?n)))
    ?f <- (Gustos (nombre ?n)(duracion-preferida nil))
    =>
    	(printout t ?n ", prefieres ver una peli corta, media o larga? ")
    	(modify ?f (duracion-preferida (read)))
    )

(defrule pregunta-idioma
    (declare (salience -1))
    (exists (need-Usuario (nombre ?n)))
    ?f <- (Gustos (nombre ?n)(idioma nil))
    =>
    	(printout t ?n ", en qué idioma prefieres ver la película? ")
    	(modify ?f (duracion-preferida (read)))
    )

(defrule pregunta-edad
    (declare (salience -1))
    (exists (need-Usuario (nombre ?n)))
    ?f <- (Gustos (nombre ?n)(edad nil))
    =>
    	(printout t ?n ", Cuantos años tienes? ")
    	(modify ?f (edad (read)))
    )
    

(defrule robots
    (declare (no-loop TRUE))
     ?f <- (Peli (nombre ?n) (tema robots))
    =>
    	(modify ?f (genero ciencia-ficcion))
)

(defrule graciosa
    (declare (no-loop TRUE))
    ?f <- (Peli (nombre ?n) (descripcion graciosa))
    =>
    	(modify ?f (genero comedia))
)
(defrule triste
    (declare (no-loop TRUE))
    ?f <- (Peli (nombre ?n) (descripcion triste))
    	=>
    	(modify ?f (genero drama))
)        

(defrule violenta
    (declare (no-loop TRUE))
     ?f <-  (Peli (nombre ?n)(descripcion violenta))
            =>
            	(modify ?f (edad 16)))


(defrule animacion
    (declare (no-loop TRUE))
     ?f <-  (Peli (nombre ?n)(genero animacion))
            =>
            	(modify ?f (edad 0)))

(defrule coches
    (declare (no-loop TRUE))
     ?f <-  (Peli (nombre ?n)(tema coches))
            =>
            	(modify ?f (genero accion))
)


(defrule carcel
    (declare (no-loop TRUE))
    ?f <- (Peli (nombre ?n) (tema carcel))
    	=>
    		(modify ?f (genero drama))
)


(defrule crimen
    (declare (no-loop TRUE))
    ?f <- (Peli (nombre ?n) (tema crimen))
    	=>
    		(modify ?f (genero thriller))
    )

(defrule select-film1 ;;Si tienen el mismo genero una peli vista y una no vista
    (declare (salience 5)(no-loop TRUE))
    ?f <- (Peli (genero ?g) (candidata no))
  		  (Pelivista (genero ?x))
    	  ;;(test (neq ?g nil)) ;;Problema?
    	  (test (eq ?g ?x))
    =>
    	 (modify ?f (candidata si))
    )

(defrule select-film2 ;;Si tienen el mismo tema una peli vista y una no vista
    (declare (salience 5) (no-loop TRUE))
    ?f <- (Peli (tema ?t) (candidata no))
    	  (Pelivista (tema ?x))
    	  (test (neq ?t nil)) ;;Problema?
     	  (test (neq ?x nil))
    	  (test (eq ?t ?x))
    => 
    	  (modify ?f (candidata si))
    )

(defrule select-film3 ;;Si tienen el la misma duracion una peli vista y una no vista que ya sea candidata
    (declare (salience 5) (no-loop TRUE))
    ?f <- (Peli (duracion ?d) (candidata no))
    	  (Pelivista (duracion ?x))
    	  ;;(test (neq ?d )) ;;Problema?
    	  (test (neq ?d nil))
    	  (test (neq ?x nil))
    	  (test (= ?d ?x))
    => 
    	  (modify ?f (candidata si))
    )



(defrule rechazar-edad  ;;Comprueba si la peli es adecuada para la edad del usuario antes de recomendar
        (declare (salience 50))
  ?f <- (Peli (nombre ?n) (edad ?e) (candidata ?c))
        (Usuario (edad ?x))
    	(test (neq ?e nil))
    	(test (neq ?x nil))
    	(test (neq ?c rechazada))
    	(test (< ?x ?e))
    => 
    	(modify ?f (candidata rechazada))
    )
 
(defrule no-idioma ;Rechaza peliculas de idioma rechazado
    (declare (salience 55))
    (Gustos (no-idioma ?x))
   ?f <- (Peli (idioma ?i) (candidata ?c))
    (test (eq ?i ?x))
    (test (neq ?c rechazada))
    =>	
    	(modify ?f (candidata rechazada))
    
    )
    

(defrule sort-duracion  ;;Si la duracion es distinta de la preferida por el usuario, pregunta antes de recomendar
   		  (declare (no-loop TRUE) (salience 30))
    ?G <- (Peli (nombre ?n) (idioma ?i)(genero ?g) (duracion ?d) (candidata si))
    ?f <- (Gustos (duracion-preferida ?x) (edad ?e) (idioma ?y) (no-duracion ?z))
   	 	  (test (neq ?d ?x))
    	  (test (neq ?d nil))
    	  (test (neq nil ?n))
    	  (test (neq nil ?g))
    	  (test (neq nil ?e))
    	  (test (neq nil ?x))
    	  (test (neq ?z ?d))
    	=>
    		(printout t "Quieres ver una película de duración " ?d "?" crlf) 
    		(printout t "Escribe si/no en función de tu respuesta. ")
    		(if (= (read) si) then (modify ?f (duracion-preferida ?d))
        						else (modify ?G (candidata rechazada))
        							 (modify ?f (no-duracion ?d)))
    )

(defrule sort-idioma
  		  (declare (no-loop TRUE) (salience 35))
   ?G <-  (Peli (nombre ?n) (genero ?g) (idioma ?i) (candidata si))
   ?f <-  (Gustos (idioma ?x) (edad ?e) (no-idioma ?z))
   		  (test (neq ?i ?x))
    	  (test (neq nil ?x))
    	  (test (neq nil ?g))
    	  (test (neq nil ?n))
    	  (test (neq nil ?e))
       	  (test (neq ?i ?z))
    =>
    	(printout t "Quieres ver una película en " ?i "?" crlf) 
    	(printout t "Escribe si/no en función de tu respuesta. ")
    	(if (= (read) si) then (modify ?f (idioma ?i))
        			       else (modify ?G (candidata rechazada))
        					    (modify ?f (no-idioma $? ?i))))

(defrule sort-both
    (declare (no-loop TRUE) (salience 40))
   ?G <- (Peli (nombre ?n) (genero ?g) (idioma ?i) (edad ?e) (duracion ?d) (candidata si))
   ?f <- (Gustos (idioma ?x) (duracion-preferida ?y) (no-duracion ?a) (edad ?c) (no-idioma ?b))
         (test (neq nil ?x))
    	 (test (neq nil ?y))
         (test (neq nil ?g))
         (test (neq nil ?n))
    	 (test (< ?e ?c))
    	 (test (neq ?i ?x))
    	 (test (neq ?d ?y))
    	 (test (neq ?b ?i))
    	 (test (neq ?a ?d))
    
    =>
    	(printout t "Quieres ver una película de duración " ?d " en " ?i "?" crlf)
    	(printout t "Escribe si/no en función de tu respuesta. ")
    
    	(if (= (read) si) then (modify ?f (idioma ?i) (duracion-preferida ?d)
                						 ?g (candidata aceptada))
        			       else (modify ?G (candidata rechazada))
        					    (modify ?f (no-idioma $? ?i)(no-duracion $? ?d))))
    	
    
(defrule aceptacion
    (declare (salience 45))
    ?f <- (Peli (idioma ?i) (duracion ?d) (candidata si))
    (Gustos (duracion-preferida ?y) (edad ?z) (idioma ?x)) 
    (test (neq nil ?d))
    (test (neq nil ?i))
    (test (neq nil ?y))
    (test (neq nil ?x))
    (test (neq nil ?z))
    (test (eq ?y ?d))
    (test (eq ?x ?i))
    =>
    	(modify ?f (candidata aceptada))
    
    )

(defrule recomendacion ;;recomendación final. Qué me hace recomendar una pelicula definitivamente?
            (declare (no-loop TRUE) (salience 500))
  	?f <-	(Peli (nombre ?n)(duracion ?d)(genero ?g) (edad ?e) (candidata aceptada)(duracion ?d))
    		(Gustos (duracion-preferida ?d) (edad ?c))
    		
    		(test (< ?e ?c))
    		(test (neq nil ?n))
    		(test (neq nil ?g))  
    	=>
    		(printout t "Te recomiendo la película " ?n ". Se trata de una película de género " ?g " y una duración " ?d "." crlf)
    		(printout t "Escribe si/no para confirmar la elección o obtener otra recomendación. ") 
    		(if (eq (read) si) then (modify ?f (candidata validada))
        					else (modify ?f (candidata rechazada))))
    
        


(defrule final
    (declare (no-loop TRUE) (salience 1000))
    (Peli (nombre ?n) (candidata validada))
    => 
    	(printout t "Has confirmado la elección de la película " ?n"." crlf)
    	(halt)
    )

(defrule fallo
    (declare (salience -10))
    
    =>
    	(printout t "Lo siento, no he encontrado ninguna peli adecuada para tus gustos." crlf)
    	(halt)
    )
 
(reset)
  
(facts)
(watch activations)

(run)





